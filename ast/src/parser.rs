/// Recursive descent and Knuth–Morris–Pratt (for expression parsing) algorithms.
use std::iter::{Filter, Peekable};

use diagnostic::{Span, Spanned};
use tokenizer::{Bracket, Keyword, Number, SpecialSymbol, Token};
use utils::binop::BinaryOp;

use super::ast::{
    AST, Argument, BinaryExpr, Block, CallArgument, Expr, ExternalFunction, Function, FunctionCall,
    FunctionSignature, InternalFunction, Literal, Statement, StructField, StructType, Type,
    TypeDefinition, UnaryExpr, UnaryOp, VariableDeclaration,
};
use super::error::ParseError;

pub fn parse<I>(token_iter: &mut I) -> (AST, Vec<Spanned<ParseError>>)
where
    I: Iterator<Item = (Token, Span)>,
{
    let parser = ASTParser::new(token_iter);
    parser.parse()
}

type FilterFn = fn(&(Token, Span)) -> bool;

pub struct ASTParser<'a, I: Iterator<Item = (Token, Span)>> {
    iter: Peekable<Filter<&'a mut I, FilterFn>>,
    errors: Vec<Spanned<ParseError>>,
    last_span: Span,
}

impl<'a, I> ASTParser<'a, I>
where
    I: Iterator<Item = (Token, Span)>,
{
    pub fn new(token_iter: &'a mut I) -> Self {
        ASTParser {
            iter: token_iter
                .by_ref()
                .filter(Self::token_filter as FilterFn)
                .peekable(),
            errors: Vec::new(),
            last_span: Span::zero(),
        }
    }

    fn token_filter(element: &(Token, Span)) -> bool {
        !matches!(
            element.0,
            Token::Comment(_) | Token::Whitespace(_) | Token::NewLine
        )
    }

    /// Take the next token and remember its span if it's not EOF.
    fn take_next(&mut self) -> Option<(Token, Span)> {
        let (token, span) = self.iter.next()?;
        self.last_span = span;
        Some((token, span))
    }

    fn push_unexpected_token(&mut self, token: Token, span: Span, expected: impl Into<String>) {
        self.errors.push(Spanned::new(
            ParseError::UnexpectedToken {
                token,
                expected: expected.into(),
            },
            span,
        ));
    }

    fn push_unexpected_eof(&mut self, expected: impl Into<String>) {
        self.errors.push(Spanned::new(
            ParseError::UnexpectedEOF {
                expected: expected.into(),
            },
            self.last_span,
        ));
    }

    fn expect(&mut self, expected: Token) -> Option<Span> {
        match self.take_next() {
            Some((token, span)) => {
                if token == expected {
                    Some(span)
                } else {
                    self.push_unexpected_token(token, span, format!("`{expected}`"));
                    None
                }
            }
            None => {
                self.push_unexpected_eof(format!("`{expected:?}`"));
                None
            }
        }
    }

    fn expect_extract<F, T>(&mut self, extract: F, expected: &str) -> Option<(T, Span)>
    where
        F: FnOnce(Token) -> Option<T>,
    {
        match self.take_next() {
            Some((token, span)) => match extract(token.clone()) {
                Some(val) => Some((val, span)),
                None => {
                    self.push_unexpected_token(token, span, expected);
                    None
                }
            },
            None => {
                self.push_unexpected_eof(expected);
                None
            }
        }
    }

    fn expect_ident(&mut self) -> Option<(String, Span)> {
        self.expect_extract(
            |t| match t {
                Token::Identifier(s) => Some(s),
                _ => None,
            },
            "identifier",
        )
    }

    fn expect_number(&mut self) -> Option<(Number, Span)> {
        self.expect_extract(
            |t| match t {
                Token::Number(n) => Some(n),
                _ => None,
            },
            "number",
        )
    }

    // Parses a comma-separated list of items (for function arguments and struct fields).
    fn parse_comma_separated<T, F>(
        &mut self,
        // Token that terminates the list.
        end_token: &Token,
        // Error message when an item is expected.
        item_expected_msg: &str,
        // Error message when a separator is expected.
        separator_expected_msg: &str,
        mut parse_item: F,
    ) -> Vec<T>
    where
        F: FnMut(&mut Self) -> Option<T>,
    {
        let mut items = Vec::new();
        let mut expect_item = true;

        loop {
            // End token closes the list and returns accumulated items.
            match self.iter.peek() {
                Some((token, _end_span)) if token == end_token => {
                    self.take_next();
                    return items;
                }
                // A comma while expecting an item means a missing item.
                Some((Token::SpecialSymbol(SpecialSymbol::Comma), _)) if expect_item => {
                    let (token, span) = match self.take_next() {
                        Some(value) => value,
                        None => {
                            self.push_unexpected_eof(item_expected_msg);
                            return items;
                        }
                    };
                    self.push_unexpected_token(token, span, item_expected_msg);
                    return items;
                }
                // Parse the next item when it is expected.
                Some(_) if expect_item => {
                    if let Some(item) = parse_item(self) {
                        items.push(item);
                        expect_item = false;
                    } else {
                        return items;
                    }
                }
                // Comma after an item switches back to expecting the next item.
                Some((Token::SpecialSymbol(SpecialSymbol::Comma), _)) => {
                    self.take_next();
                    expect_item = true;
                }
                // Any non-comma token after an item is a separator error.
                Some(_) => {
                    let (token, span) = match self.take_next() {
                        Some(value) => value,
                        None => {
                            self.push_unexpected_eof(separator_expected_msg);
                            return items;
                        }
                    };
                    self.push_unexpected_token(token, span, separator_expected_msg);
                    return items;
                }
                // EOF while still parsing the list.
                None => {
                    self.push_unexpected_eof(separator_expected_msg);
                    return items;
                }
            }
        }
    }

    pub fn parse(mut self) -> (AST, Vec<Spanned<ParseError>>) {
        let mut ast = AST::new();

        while let Some((token, _)) = self.iter.peek() {
            match token {
                Token::Keyword(Keyword::Fn) => {
                    if let Some(func) = self.parse_function() {
                        ast.add_function(func);
                    }
                }
                Token::Keyword(Keyword::Type) => {
                    if let Some(ty_def) = self.parse_type_definition() {
                        ast.add_type_definition(ty_def);
                    }
                }
                _ => {
                    let Some((token, span)) = self.take_next() else {
                        continue;
                    };
                    self.push_unexpected_token(token, span, "function or type definition");
                }
            }
        }

        (ast, self.errors)
    }

    fn parse_type_definition(&mut self) -> Option<TypeDefinition> {
        self.expect(Token::Keyword(Keyword::Type))?;
        let (type_name, type_name_span) = self.expect_ident()?;
        self.expect(Token::SpecialSymbol(SpecialSymbol::Equals))?;
        let ty = self.parse_type()?;
        let ty_def = TypeDefinition {
            name: Spanned::new(type_name, type_name_span),
            ty,
        };
        Some(ty_def)
    }

    fn parse_type(&mut self) -> Option<Spanned<Type>> {
        match self.iter.peek() {
            Some((Token::Identifier(_), _)) => {
                let (type_name, type_name_span) = self.expect_ident()?;
                let ty = Type::from_str(&type_name);
                Some(Spanned::new(ty, type_name_span))
            }
            Some((Token::Keyword(Keyword::Struct), _)) => {
                let (struct_type, span) = self.parse_struct()?.unwrap();
                Some(Spanned::new(Type::Struct(struct_type), span))
            }
            Some(_) => {
                let (token, span) = self.take_next()?;
                self.push_unexpected_token(token, span, "type");
                None
            }
            None => {
                self.push_unexpected_eof("type");
                None
            }
        }
    }

    fn parse_struct(&mut self) -> Option<Spanned<StructType>> {
        let start_span = self.expect(Token::Keyword(Keyword::Struct))?;
        self.expect(Token::Bracket(Bracket::CurlyOpen))?;

        let fields = self.parse_comma_separated(
            &Token::Bracket(Bracket::CurlyClose),
            "struct field or `}`",
            "`,` or `}`",
            Self::parse_struct_field,
        );

        let struct_type = StructType { fields };
        let span = start_span.join(self.last_span);
        Some(Spanned::new(struct_type, span))
    }

    fn parse_struct_field(&mut self) -> Option<Spanned<StructField>> {
        let (field_name, field_name_span) = self.expect_ident()?;
        self.expect(Token::SpecialSymbol(SpecialSymbol::Colon))?;
        let ty = self.parse_type()?;
        let type_span = ty.span.clone();
        Some(Spanned::new(
            StructField {
                name: Spanned::new(field_name, field_name_span),
                ty,
            },
            field_name_span.join(type_span),
        ))
    }

    fn parse_function_arg(&mut self) -> Option<Spanned<Argument>> {
        let (arg_name, arg_name_span) = self.expect_ident()?;
        self.expect(Token::SpecialSymbol(SpecialSymbol::Colon))?;
        let ty = self.parse_type()?;
        let arg = Argument {
            name: Spanned::new(arg_name, arg_name_span),
            ty,
        };
        let arg_span = arg.name.span.join(arg.ty.span);
        Some(Spanned::new(arg, arg_span))
    }

    fn parse_function(&mut self) -> Option<Function> {
        self.expect(Token::Keyword(Keyword::Fn))?;
        let (func_name, func_name_span) = self.expect_ident()?;
        self.expect(Token::Bracket(Bracket::RoundOpen))?;

        let args = self.parse_comma_separated(
            &Token::Bracket(Bracket::RoundClose),
            "function argument or `)`",
            "`,` or `)`",
            Self::parse_function_arg,
        );

        let return_type = match self.iter.peek() {
            Some((token, _span)) => match token {
                Token::SpecialSymbol(SpecialSymbol::Minus) => {
                    self.take_next(); // consume '-'
                    self.expect(Token::SpecialSymbol(SpecialSymbol::GreaterThan))?;
                    Some(self.parse_type()?)
                }
                _ => None,
            },
            None => None,
        };

        let signature = FunctionSignature {
            name: Spanned::new(func_name, func_name_span),
            args,
            return_type,
        };

        match self.iter.peek() {
            Some((token, _span)) => match token {
                Token::Bracket(Bracket::CurlyOpen) => {
                    let block = self.parse_block()?.node;
                    let func = Function::Internal(InternalFunction {
                        signature,
                        body: block,
                    });
                    Some(func)
                }
                _ => Some(Function::External(ExternalFunction { signature })),
            },
            None => Some(Function::External(ExternalFunction { signature })),
        }
    }

    fn parse_block(&mut self) -> Option<Spanned<Block>> {
        let start_span = self.expect(Token::Bracket(Bracket::CurlyOpen))?;
        let mut block = Vec::new();

        loop {
            match self.iter.peek() {
                Some((token, _span)) => match token {
                    Token::Bracket(Bracket::CurlyClose) => {
                        let (_, end_span) = self.take_next()?;
                        break Some(Spanned::new(block, start_span.join(end_span)));
                    }
                    _ => {
                        let Some(stmt) = self.parse_statement() else {
                            continue;
                        };
                        block.push(stmt);
                    }
                },
                None => {
                    self.push_unexpected_eof("statement or `}`");
                    break Some(Spanned::new(block, start_span.join(self.last_span)));
                }
            }
        }
    }

    fn parse_statement(&mut self) -> Option<Spanned<Statement>> {
        let Some((token, _)) = self.iter.peek() else {
            self.push_unexpected_eof("statement (function call, new variable, return, etc.)");
            return None;
        };

        match token {
            Token::Keyword(Keyword::Let) => self.parse_variable_declaration(),
            Token::Identifier(_) => self.parse_identifier_statement(),
            Token::Keyword(Keyword::Return) => self.parse_return_statement(),
            _ => {
                let (token, span) = self.take_next()?;
                self.push_unexpected_token(
                    token,
                    span,
                    "statement (function call, new variable, return, etc.)",
                );
                None
            }
        }
    }

    fn parse_identifier_statement(&mut self) -> Option<Spanned<Statement>> {
        let Some((Token::Identifier(id), span)) = self.take_next() else {
            unreachable!(); // Unreachable: parse_statement dispatches this branch only for identifiers.
        };

        let Some((next_token, _)) = self.iter.peek() else {
            self.push_unexpected_eof("`(` or `=`");
            return None;
        };

        match next_token {
            Token::Bracket(Bracket::RoundOpen) => self
                .parse_function_call(id, span)
                .map(|spanned_call| spanned_call.map(Expr::FunctionCall).map(Statement::Expr)),
            // Variable assignment
            // TODO: Add support for variable type annotation in assignment, e.g. `let x: i32 = 5`
            Token::SpecialSymbol(SpecialSymbol::Equals) => {
                self.take_next(); // consume '='
                let expr = self.parse_expression(0)?;
                let end_span = expr.span;
                let var_decl = VariableDeclaration {
                    name: Spanned::new(id, span),
                    ty: None,
                    value: expr,
                };
                Some(Spanned::new(
                    Statement::VariableDeclaration(var_decl),
                    span.join(end_span),
                ))
            }
            _ => {
                let (token, span) = self.take_next()?;
                self.push_unexpected_token(token, span, "`(` or `=`");
                None
            }
        }
    }

    fn parse_return_statement(&mut self) -> Option<Spanned<Statement>> {
        let (_, return_span) = self.take_next()?;
        let Some((token, _)) = self.iter.peek() else {
            self.push_unexpected_eof("expression or new line with `}`");
            return None;
        };

        if Self::is_expression_start(token) {
            let expr = self.parse_expression(0)?;
            let end_span = expr.span;
            return Some(Spanned::new(
                Statement::Return(Some(expr)),
                return_span.join(end_span),
            ));
        }

        Some(Spanned::new(Statement::Return(None), return_span))
    }

    fn is_expression_start(token: &Token) -> bool {
        matches!(
            token,
            Token::Identifier(_)
                | Token::Number(_)
                | Token::Bracket(Bracket::RoundOpen)
                | Token::Bracket(Bracket::CurlyOpen)
                | Token::SpecialSymbol(SpecialSymbol::Minus)
                | Token::SpecialSymbol(SpecialSymbol::Exclamation)
                | Token::SpecialSymbol(SpecialSymbol::Tilde)
        )
    }

    fn parse_function_call(&mut self, id: String, span: Span) -> Option<Spanned<FunctionCall>> {
        self.expect(Token::Bracket(Bracket::RoundOpen))?;
        let parse_call_arg = |parser: &mut Self| {
            let expr = parser.parse_expression(0)?;
            Some(CallArgument {
                name: None,
                value: expr,
            })
        };

        let arguments = self.parse_comma_separated(
            &Token::Bracket(Bracket::RoundClose),
            "function argument or `)`",
            "`,` or `)`",
            parse_call_arg,
        );

        Some(Spanned::new(
            FunctionCall {
                name: Spanned::new(id, span),
                args: arguments,
            },
            span.join(self.last_span),
        ))
    }

    fn parse_variable_declaration(&mut self) -> Option<Spanned<Statement>> {
        let first_span = self.expect(Token::Keyword(Keyword::Let))?;
        let (var_name, var_name_span) = self.expect_ident()?;

        match self.take_next() {
            // If type is not specified, expect '=' next
            Some((Token::SpecialSymbol(SpecialSymbol::Equals), _span)) => {
                let expr = self.parse_expression(0)?;
                let end_span = expr.span;
                let stmt = VariableDeclaration {
                    name: Spanned::new(var_name, var_name_span),
                    ty: None,
                    value: expr,
                };
                Some(Spanned::new(
                    Statement::VariableDeclaration(stmt),
                    first_span.join(end_span),
                ))
            }
            // If type is specified, expect ': Type =' next
            Some((Token::SpecialSymbol(SpecialSymbol::Colon), _span)) => {
                let ty = self.parse_type()?;
                self.expect(Token::SpecialSymbol(SpecialSymbol::Equals))?;
                let expr = self.parse_expression(0)?;
                let end_span = expr.span;
                let stmt = VariableDeclaration {
                    name: Spanned::new(var_name, var_name_span),
                    ty: Some(ty),
                    value: expr,
                };
                Some(Spanned::new(
                    Statement::VariableDeclaration(stmt),
                    first_span.join(end_span),
                ))
            }
            Some((token, span)) => {
                self.push_unexpected_token(token, span, "`:` or `=`");
                None
            }
            None => {
                self.push_unexpected_eof("`:` or `=`");
                None
            }
        }
    }

    fn parse_expression(&mut self, min_bp: u8) -> Option<Spanned<Expr>> {
        let mut left_expr = self.parse_atomic_expression()?;

        loop {
            let op = match self.iter.peek() {
                Some((token, _span)) => match token {
                    // Arithemetic operations
                    Token::SpecialSymbol(SpecialSymbol::Plus) => BinaryOp::Add,
                    Token::SpecialSymbol(SpecialSymbol::Minus) => BinaryOp::Sub,
                    Token::SpecialSymbol(SpecialSymbol::Asterisk) => BinaryOp::Mul,
                    Token::SpecialSymbol(SpecialSymbol::Slash) => BinaryOp::Div,
                    Token::SpecialSymbol(SpecialSymbol::Percent) => BinaryOp::Mod,
                    Token::SpecialSymbol(SpecialSymbol::Backslash) => BinaryOp::DivInt,
                    Token::SpecialSymbol(SpecialSymbol::DoubleAsterisk) => BinaryOp::Pow,

                    // Bitwise operations
                    Token::SpecialSymbol(SpecialSymbol::Ampersand) => BinaryOp::BitAnd,
                    Token::SpecialSymbol(SpecialSymbol::Pipe) => BinaryOp::BitOr,
                    Token::SpecialSymbol(SpecialSymbol::Caret) => BinaryOp::BitXor,
                    Token::SpecialSymbol(SpecialSymbol::DoubleLessThan) => BinaryOp::Shl,
                    Token::SpecialSymbol(SpecialSymbol::DoubleGreaterThan) => BinaryOp::Shr,

                    // Logical operations
                    Token::SpecialSymbol(SpecialSymbol::DoubleAmpersand) => BinaryOp::And,
                    Token::SpecialSymbol(SpecialSymbol::DoublePipe) => BinaryOp::Or,
                    Token::SpecialSymbol(SpecialSymbol::DoubleEquals) => BinaryOp::Eq,
                    Token::SpecialSymbol(SpecialSymbol::ExclamationEquals) => BinaryOp::Neq,
                    Token::SpecialSymbol(SpecialSymbol::LessThan) => BinaryOp::Lt,
                    Token::SpecialSymbol(SpecialSymbol::GreaterThan) => BinaryOp::Gt,
                    Token::SpecialSymbol(SpecialSymbol::GreaterThanEquals) => BinaryOp::Geq,
                    Token::SpecialSymbol(SpecialSymbol::LessThanEquals) => BinaryOp::Leq,
                    _ => break,
                },
                None => {
                    self.push_unexpected_eof("expression");
                    return None;
                }
            };

            let (current_l_bp, current_r_bp) = op.binding_power();
            if current_l_bp < min_bp {
                break;
            }
            self.take_next();

            let right_expr = self.parse_expression(current_r_bp)?;

            let left_expr_span = left_expr.span;
            let right_expr_span = right_expr.span;
            let span = left_expr_span.join(right_expr_span);
            left_expr = Spanned::new(
                Expr::Binary(BinaryExpr {
                    op,
                    left: Box::new(left_expr),
                    right: Box::new(right_expr),
                }),
                span,
            );
        }

        Some(left_expr)
    }

    /// Parse an atomic expression: a literal, variable, function call.
    fn parse_atomic_expression(&mut self) -> Option<Spanned<Expr>> {
        match self.iter.peek() {
            Some((token, _span)) => match token {
                Token::Identifier(_) => {
                    let Some((Token::Identifier(id), span)) = self.take_next() else {
                        unreachable!(); // Unreachable: we peeked and saw Identifier
                    };
                    match id.as_str() {
                        "true" => {
                            // TODO: it's incorrect to be identifier, must be a literal
                            return Some(Spanned::new(Expr::Literal(Literal::Bool(true)), span));
                        }
                        "false" => {
                            return Some(Spanned::new(Expr::Literal(Literal::Bool(false)), span));
                        }
                        "void" => return Some(Spanned::new(Expr::Literal(Literal::Void), span)),
                        _ => {}
                    }
                    match self.iter.peek() {
                        Some((token, _span)) => match token {
                            Token::Bracket(Bracket::RoundOpen) => self
                                .parse_function_call(id, span)
                                .map(|spanned_call| spanned_call.map(Expr::FunctionCall)),
                            _ => Some(Spanned::new(Expr::Variable(id), span)),
                        },
                        None => {
                            self.push_unexpected_eof("'('");
                            None
                        }
                    }
                }
                Token::Number(_) => self
                    .parse_number()
                    .map(|spanned_lit| spanned_lit.map(Expr::Literal)),
                Token::Bracket(Bracket::RoundOpen) => {
                    self.take_next(); // consume '('
                    let expr = self.parse_expression(0)?;
                    self.expect(Token::Bracket(Bracket::RoundClose))?;
                    Some(expr)
                }
                Token::Bracket(Bracket::CurlyOpen) => {
                    let block = self.parse_block()?;
                    Some(block.map(Expr::Block))
                }

                // Unary expressions
                Token::SpecialSymbol(SpecialSymbol::Minus) => {
                    self.take_next(); // consume '-'
                    let expr = self.parse_atomic_expression()?;
                    let span = expr.span;
                    Some(Spanned::new(
                        Expr::Unary(UnaryExpr {
                            op: UnaryOp::Negate,
                            expr: Box::new(expr),
                        }),
                        span,
                    ))
                }
                Token::SpecialSymbol(SpecialSymbol::Exclamation) => {
                    self.take_next(); // consume '!'
                    let expr = self.parse_atomic_expression()?;
                    let span = expr.span;
                    Some(Spanned::new(
                        Expr::Unary(UnaryExpr {
                            op: UnaryOp::Not,
                            expr: Box::new(expr),
                        }),
                        span,
                    ))
                }
                Token::SpecialSymbol(SpecialSymbol::Tilde) => {
                    self.take_next(); // consume '~'
                    let expr = self.parse_atomic_expression()?;
                    let span = expr.span;
                    Some(Spanned::new(
                        Expr::Unary(UnaryExpr {
                            op: UnaryOp::BitNot,
                            expr: Box::new(expr),
                        }),
                        span,
                    ))
                }
                _ => {
                    let (token, span) = self.take_next()?;
                    self.push_unexpected_token(token, span, "expression");
                    None
                }
            },
            None => {
                self.push_unexpected_eof("expression");
                None
            }
        }
    }

    fn parse_number(&mut self) -> Option<Spanned<Literal>> {
        let (number, number_span) = self.expect_number()?;
        Some(Spanned::new(Literal::from_number(number), number_span))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use tokenizer::tokenize;

    fn parse_and_check_by_display(code: &str, expected_display: &str) {
        let mut token_iter = tokenize(code.char_indices());
        let (ast, errors) = ASTParser::new(&mut token_iter).parse();
        assert!(errors.is_empty());
        assert_eq!(format!("{ast}"), expected_display);
    }

    fn parse_and_get_errors(code: &str) -> Vec<Spanned<ParseError>> {
        let mut token_iter = tokenize(code.char_indices());
        let (_ast, errors) = ASTParser::new(&mut token_iter).parse();
        errors
    }

    fn assert_unexpected_token(code: &str, expected: &str) {
        let errors = parse_and_get_errors(code);
        assert!(!errors.is_empty());
        match &errors[0].node {
            ParseError::UnexpectedToken {
                expected: actual, ..
            } => {
                assert_eq!(actual, expected);
            }
            ParseError::UnexpectedEOF { .. } => {
                panic!("expected UnexpectedToken as first error")
            }
        }
    }

    #[test]
    fn test_function_signatures() {
        let code = indoc! {"
            fn empty() {}

            fn no_args() {
                return
            }

            fn one_arg(x: i32) {
                return
            }

            fn two_args(x: i32, y: f64) {
                return
            }

            fn voidf() -> void {
                return
            }

            fn intf() -> i32 {
                return 5
            }

            fn floatf() -> f64 {
                return 5.0
            }

            fn boolf() -> bool {
                return true
            }

            fn ext_no_args()

            fn ext_one_arg(x: i32)

            fn ext_two_args(x: i32, y: f64) -> bool

            fn multi_line(
                x: i32,
                y: f64,
                z: bool,
            ) -> void {
                return
            }
        "};

        let expected_display = indoc! {"
            fn empty() {}

            fn no_args() {
                return
            }

            fn one_arg(x: i32) {
                return
            }

            fn two_args(x: i32, y: f64) {
                return
            }

            fn voidf() -> void {
                return
            }

            fn intf() -> i32 {
                return 5
            }

            fn floatf() -> f64 {
                return 5.0
            }

            fn boolf() -> bool {
                return true
            }

            fn ext_no_args()

            fn ext_one_arg(x: i32)

            fn ext_two_args(x: i32, y: f64) -> bool

            fn multi_line(x: i32, y: f64, z: bool) -> void {
                return
            }
        "};

        parse_and_check_by_display(code, expected_display);
    }

    #[test]
    fn test_arithmetic_binary_expressions() {
        let code = indoc! {"
            fn main() {
                let a = 1 + 2 + 3 - 4
                let b = 1 * 2 * 3
                let c = 1 + 2 * 3
                let d = (1 + 2) * 3
                let e = 1 * 2 / 3 % 4 \\ 5
                let f = (1+2)-(3-4)
            }
        "};

        let expected_display = indoc! {"
            fn main() {
                let a = (((1 + 2) + 3) - 4)
                let b = ((1 * 2) * 3)
                let c = (1 + (2 * 3))
                let d = ((1 + 2) * 3)
                let e = ((((1 * 2) / 3) % 4) \\ 5)
                let f = ((1 + 2) - (3 - 4))
            }
        "};

        parse_and_check_by_display(code, expected_display);
    }

    #[test]
    fn test_blocks() {
        let code = indoc! {"
            fn main() {
                let x = {
                    let a = {
                        let a = {
                            return get_value() + 1 * 2
                        }
                        return a
                    }
                    let b = {return (1)}
                    return a * b
                }
                let y = {}
                print({
                    let value = compute_value(x, 1, 3)
                    return value + x
                })
            }
        "};

        let expected_display = indoc! {"
            fn main() {
                let x = {
                    let a = {
                        let a = {
                            return (get_value() + (1 * 2))
                        }
                        return a
                    }
                    let b = {
                        return 1
                    }
                    return (a * b)
                }
                let y = {}
                print({
                    let value = compute_value(x, 1, 3)
                    return (value + x)
                })
            }
        "};

        parse_and_check_by_display(code, expected_display);
    }

    #[test]
    fn test_logical_binary_expressions() {
        let code = indoc! {"
            fn main() {
                let a = true && false || true
                let b = 1 == 2 && 3 != 4 || 5 < 6 && 7 <= 8 || 9 > 10 && 11 >= 12
            }
        "};

        let expected_display = indoc! {"
            fn main() {
                let a = ((true && false) || true)
                let b = ((((1 == 2) && (3 != 4)) || ((5 < 6) && (7 <= 8))) || ((9 > 10) && (11 >= 12)))
            }
        "};

        parse_and_check_by_display(code, expected_display);
    }

    #[test]
    fn test_unary_expressions() {
        let code = indoc! {"
            fn main() {
                let a = !true
                let b = ~1
                let c = !!false
                let d = ~~2
                let e = !~!~3
                let f = ~!~!4
                let g = -5
                let h = --6
                let i = - -7
                let j = -(-8)
                let k = !-!9
                let l = !{return !true}
            }
        "};

        let expected_display = indoc! {"
            fn main() {
                let a = !true
                let b = ~1
                let c = !!false
                let d = ~~2
                let e = !~!~3
                let f = ~!~!4
                let g = -5
                let h = --6
                let i = --7
                let j = --8
                let k = !-!9
                let l = !{
                    return !true
                }
            }
        "};

        parse_and_check_by_display(code, expected_display);
    }

    #[test]
    fn test_struct_field_commas_valid() {
        let code = indoc! {"
            type Pos = struct {
                x: i32,
                y: i32,
            }

            type Pos2 = struct {
                x: i32,
                y: i32
            }

            type Pos3 = struct { x: i32, y: i32 }
        "};

        assert!(parse_and_get_errors(code).is_empty());
    }

    #[test]
    fn test_struct_field_commas_invalid() {
        assert_unexpected_token(
            indoc! {"
                type Pos = struct {
                    x: i32
                    y: i32,
                }
            "},
            "`,` or `}`",
        );

        assert_unexpected_token(
            indoc! {"
                type Pos = struct {
                    x: i32,,,,,
                    y: i32,
                }
            "},
            "struct field or `}`",
        );

        assert_unexpected_token(
            indoc! {"
                type Pos = struct { x: i32 y: i32 }
            "},
            "`,` or `}`",
        );

        assert_unexpected_token(
            indoc! {"
                type Pos = struct {,
                    x: i32,
                    y: i32,
                }
            "},
            "struct field or `}`",
        );
    }

    #[test]
    fn test_function_signature_commas_invalid() {
        assert_unexpected_token(
            indoc! {"
                fn bad(x: i32 y: i32) {
                    return
                }
            "},
            "`,` or `)`",
        );

        assert_unexpected_token(
            indoc! {"
                fn bad(x: i32,, y: i32) {
                    return
                }
            "},
            "function argument or `)`",
        );

        assert_unexpected_token(
            indoc! {"
                fn bad(, x: i32) {
                    return
                }
            "},
            "function argument or `)`",
        );
    }

    #[test]
    fn test_function_call_commas_valid() {
        let code = indoc! {"
            fn main() {
                foo(1, 2)
                foo(1, 2,)
                foo()
            }
        "};

        assert!(parse_and_get_errors(code).is_empty());
    }

    #[test]
    fn test_function_call_commas_invalid() {
        assert_unexpected_token(
            indoc! {"
                fn main() {
                    foo(1 2)
                }
            "},
            "`,` or `)`",
        );

        assert_unexpected_token(
            indoc! {"
                fn main() {
                    foo(1,,2)
                }
            "},
            "function argument or `)`",
        );

        assert_unexpected_token(
            indoc! {"
                fn main() {
                    foo(,1)
                }
            "},
            "function argument or `)`",
        );
    }
}
