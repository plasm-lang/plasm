/// Recursive descent and Pratt (for expression parsing) algorithms.
use std::iter::Filter;

use diagnostic::{Span, Spanned};
use tokenizer::{Bracket, Keyword, Number, SpecialSymbol, Token};
use utils::bin_op::BinaryOp;

use super::ast::{
    AST, Argument, BinaryExpr, Block, CallArgument, Expr, ExternalFunction,
    FieldAccess, Function, FunctionCall, FunctionSignature, InternalFunction,
    Literal, Place, Statement, StructField, StructLiteralExpr, StructLiteralField,
    StructType, Type, TypeDefinition, UnaryExpr, UnaryOp, VariableDeclaration,
};
use super::error::ParseError;
use super::lookahead::Lookahead;

// Const messages for expected tokens in error reporting
const IDENTIFIER: &str = "identifier";
const NUMBER: &str = "number";
const FUNCTION_OR_TYPE: &str = "function or type definition";
const TYPE: &str = "type";
const STRUCT_FIELD_OR_CURLY: &str = "struct field or `}`";
const COMMA_OR_CURLY: &str = "`,` or `}`";
const COMMA_OR_ROUND: &str = "`,` or `)`";
const ARG_OR_ROUND: &str = "function argument or `)`";
const STATEMENT_OR_CURLY: &str = "statement or `}`";
const STATEMENT: &str =
    "statement (function call, variable declaration, return, etc.)";
const ROUND_OR_EQUAL: &str = "`(` or `=`";
const EXPRESSION_OR_CURLY: &str = "expression or `}`";
const COLON_OR_EQUAL: &str = "`:` or `=`";
const EXPRESSION: &str = "expression";
const ROUND: &str = "`(`";
const ASSIGNMENT_OR_STATEMENT_OR_CURLY: &str = "assignment, statement, or `}`";

// For brevity
type S<T> = Spanned<T>;

pub fn parse<I>(token_iter: &mut I) -> (AST, Vec<S<ParseError>>)
where
    I: Iterator<Item = (Token, Span)>,
{
    let parser = ASTParser::new(token_iter);
    parser.parse()
}

type FilterFn = fn(&(Token, Span)) -> bool;

pub struct ASTParser<'a, I: Iterator<Item = (Token, Span)>> {
    iter: Lookahead<Filter<&'a mut I, FilterFn>, 3>,
    // errors: Vec<S<ParseError>>,
    last_span: Span,
}

impl<'a, I> ASTParser<'a, I>
where
    I: Iterator<Item = (Token, Span)>,
{
    pub fn new(token_iter: &'a mut I) -> Self {
        ASTParser {
            iter: Lookahead::new(token_iter.by_ref().filter(Self::token_filter)),
            // errors: Vec::new(),
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

    fn unexpected_token(
        &mut self,
        token: Token,
        span: Span,
        expected: impl Into<String>,
    ) -> S<ParseError> {
        S::new(
            ParseError::UnexpectedToken {
                token,
                expected: expected.into(),
            },
            span,
        )
    }

    fn unexpected_eof(&mut self, expected: impl Into<String>) -> S<ParseError> {
        S::new(
            ParseError::UnexpectedEOF {
                expected: expected.into(),
            },
            self.last_span,
        )
    }

    fn expect(&mut self, expected: Token) -> Result<Span, S<ParseError>> {
        match self.take_next() {
            Some((token, span)) => {
                if token == expected {
                    Ok(span)
                } else {
                    Err(self.unexpected_token(token, span, format!("`{expected}`")))
                }
            }
            None => Err(self.unexpected_eof(format!("`{expected:?}`"))),
        }
    }

    fn expect_extract<F, T>(
        &mut self,
        extract: F,
        expected: &str,
    ) -> Result<(T, Span), S<ParseError>>
    where
        F: FnOnce(Token) -> Option<T>,
    {
        match self.take_next() {
            Some((token, span)) => match extract(token.clone()) {
                Some(val) => Ok((val, span)),
                None => Err(self.unexpected_token(token, span, expected)),
            },
            None => Err(self.unexpected_eof(expected)),
        }
    }

    fn expect_ident(&mut self) -> Result<(String, Span), S<ParseError>> {
        self.expect_extract(
            |t| match t {
                Token::Identifier(s) => Some(s),
                _ => None,
            },
            IDENTIFIER,
        )
    }

    fn expect_number(&mut self) -> Result<(Number, Span), S<ParseError>> {
        self.expect_extract(
            |t| match t {
                Token::Number(n) => Some(n),
                _ => None,
            },
            NUMBER,
        )
    }

    // Parses a comma-separated list of items (for function arguments and struct
    // fields).
    fn parse_comma_separated<T, F>(
        &mut self,
        // Token that terminates the list.
        end_token: &Token,
        // Error message when an item is expected.
        item_expected_msg: &str,
        // Error message when a separator is expected.
        separator_expected_msg: &str,
        mut parse_item: F,
    ) -> Result<Vec<T>, S<ParseError>>
    where
        F: FnMut(&mut Self) -> Result<T, S<ParseError>>,
    {
        let mut items = Vec::new();
        let mut expect_item = true;

        loop {
            // End token closes the list and returns accumulated items.
            match self.iter.peek() {
                Some((token, _end_span)) if token == end_token => {
                    self.take_next();
                    return Ok(items);
                }
                // A comma while expecting an item means a missing item.
                Some((Token::SpecialSymbol(SpecialSymbol::Comma), _))
                    if expect_item =>
                {
                    let (token, span) = match self.take_next() {
                        Some(value) => value,
                        None => {
                            return Err(self.unexpected_eof(item_expected_msg));
                        }
                    };
                    return Err(self.unexpected_token(
                        token,
                        span,
                        item_expected_msg,
                    ));
                }
                // Parse the next item when it is expected.
                Some(_) if expect_item => {
                    let item = parse_item(self)?;
                    items.push(item);
                    expect_item = false;
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
                            return Err(self.unexpected_eof(separator_expected_msg));
                        }
                    };
                    return Err(self.unexpected_token(
                        token,
                        span,
                        separator_expected_msg,
                    ));
                }
                // EOF while still parsing the list.
                None => {
                    return Err(self.unexpected_eof(separator_expected_msg));
                }
            }
        }
    }

    pub fn parse(mut self) -> (AST, Vec<S<ParseError>>) {
        // It's `(AST, Vec<S<ParseError>>)` instead of `Result<AST,
        // S<ParseError>>` for error recovery in the future.
        let mut ast = AST::new();
        let mut errors = Vec::new();

        while let Some((token, _)) = self.iter.peek() {
            match token {
                Token::Keyword(Keyword::Fn) => match self.parse_function() {
                    Ok(func) => ast.add_function(func),
                    Err(err) => {
                        errors.push(err);
                        self.iter.last(); // Skip to the last token to fill lines table.
                        break;
                    }
                },
                Token::Keyword(Keyword::Type) => {
                    match self.parse_type_definition() {
                        Ok(ty_def) => ast.add_type_definition(ty_def),
                        Err(err) => {
                            errors.push(err);
                            self.iter.last(); // Skip to the last token to fill lines table.
                            break;
                        }
                    }
                }
                _ => {
                    let Some((token, span)) = self.take_next() else {
                        continue;
                    };
                    errors.push(self.unexpected_token(
                        token,
                        span,
                        FUNCTION_OR_TYPE,
                    ));
                    self.iter.last(); // Skip to the last token to fill lines table.
                    break;
                }
            }
        }

        (ast, errors)
    }

    fn parse_type_definition(&mut self) -> Result<TypeDefinition, S<ParseError>> {
        self.expect(Token::Keyword(Keyword::Type))?;
        let (type_name, type_name_span) = self.expect_ident()?;
        self.expect(Token::SpecialSymbol(SpecialSymbol::Equals))?;
        let ty = self.parse_type()?;
        let ty_def = TypeDefinition {
            name: S::new(type_name, type_name_span),
            ty,
        };
        Ok(ty_def)
    }

    fn parse_type(&mut self) -> Result<S<Type>, S<ParseError>> {
        match self.iter.peek() {
            Some((Token::Identifier(_), _)) => {
                let (type_name, type_name_span) = self.expect_ident()?;
                let ty = Type::from_ident(&type_name);
                Ok(S::new(ty, type_name_span))
            }
            Some((Token::Keyword(Keyword::Struct), _)) => {
                let (struct_type, span) = self.parse_struct()?.unwrap();
                Ok(S::new(Type::Struct(struct_type), span))
            }
            Some(_) => {
                let (token, span) = self.take_next().unwrap();
                Err(self.unexpected_token(token, span, TYPE))
            }
            None => Err(self.unexpected_eof(TYPE)),
        }
    }

    fn parse_struct(&mut self) -> Result<S<StructType>, S<ParseError>> {
        let start_span = self.expect(Token::Keyword(Keyword::Struct))?;
        self.expect(Token::Bracket(Bracket::CurlyOpen))?;

        let fields = self.parse_comma_separated(
            &Token::Bracket(Bracket::CurlyClose),
            STRUCT_FIELD_OR_CURLY,
            COMMA_OR_CURLY,
            Self::parse_struct_field,
        )?;

        let struct_type = StructType { fields };
        let span = start_span.join(self.last_span);
        Ok(S::new(struct_type, span))
    }

    fn parse_struct_field(&mut self) -> Result<S<StructField>, S<ParseError>> {
        let (field_name, field_name_span) = self.expect_ident()?;
        self.expect(Token::SpecialSymbol(SpecialSymbol::Colon))?;
        let ty = self.parse_type()?;
        let type_span = ty.span;
        Ok(S::new(
            StructField {
                name: S::new(field_name, field_name_span),
                ty,
            },
            field_name_span.join(type_span),
        ))
    }

    fn parse_function_arg(&mut self) -> Result<S<Argument>, S<ParseError>> {
        let (arg_name, arg_name_span) = self.expect_ident()?;
        self.expect(Token::SpecialSymbol(SpecialSymbol::Colon))?;
        let ty = self.parse_type()?;
        let arg = Argument {
            name: S::new(arg_name, arg_name_span),
            ty,
        };
        let arg_span = arg.name.span.join(arg.ty.span);
        Ok(S::new(arg, arg_span))
    }

    fn parse_function(&mut self) -> Result<Function, S<ParseError>> {
        self.expect(Token::Keyword(Keyword::Fn))?;
        let (func_name, func_name_span) = self.expect_ident()?;
        self.expect(Token::Bracket(Bracket::RoundOpen))?;

        let args = self.parse_comma_separated(
            &Token::Bracket(Bracket::RoundClose),
            ARG_OR_ROUND,
            COMMA_OR_ROUND,
            Self::parse_function_arg,
        )?;

        let return_type = match self.iter.peek() {
            Some((Token::SpecialSymbol(SpecialSymbol::Minus), _span)) => {
                self.take_next(); // consume '-'
                self.expect(Token::SpecialSymbol(SpecialSymbol::GreaterThan))?;
                Some(self.parse_type()?)
            }
            _ => None,
        };

        let signature = FunctionSignature {
            name: S::new(func_name, func_name_span),
            args,
            return_type,
        };

        match self.iter.peek() {
            Some((Token::Bracket(Bracket::CurlyOpen), _span)) => {
                let block = self.parse_block()?.node;
                let func = Function::Internal(InternalFunction {
                    signature,
                    body: block,
                });
                Ok(func)
            }
            _ => Ok(Function::External(ExternalFunction { signature })),
        }
    }

    fn parse_block(&mut self) -> Result<S<Block>, S<ParseError>> {
        let start_span = self.expect(Token::Bracket(Bracket::CurlyOpen))?;
        let mut block = Vec::new();

        loop {
            match self.iter.peek() {
                Some((Token::Bracket(Bracket::CurlyClose), _span)) => {
                    let (_, end_span) = self.take_next().unwrap();
                    break Ok(S::new(block, start_span.join(end_span)));
                }
                Some(_) => {
                    let stmt = self.parse_statement()?;
                    block.push(stmt);
                }
                None => {
                    return Err(self.unexpected_eof(STATEMENT_OR_CURLY));
                }
            }
        }
    }

    fn parse_statement(&mut self) -> Result<S<Statement>, S<ParseError>> {
        let Some((token, _)) = self.iter.peek() else {
            return Err(self.unexpected_eof(STATEMENT));
        };

        match token {
            Token::Keyword(Keyword::Let) => {
                self.parse_variable_declaration_statement()
            }
            Token::Keyword(Keyword::Return) => self.parse_return_statement(),
            Token::Identifier(_) => self.parse_identifier_statement(),
            _ => {
                let (token, span) = self.take_next().unwrap();
                Err(self.unexpected_token(token, span, STATEMENT))
            }
        }
    }

    fn parse_variable_declaration_statement(
        &mut self,
    ) -> Result<S<Statement>, S<ParseError>> {
        let first_span = self.expect(Token::Keyword(Keyword::Let))?;
        let (var_name, var_name_span) = self.expect_ident()?;

        match self.take_next() {
            // If type is not specified, expect '=' next
            Some((Token::SpecialSymbol(SpecialSymbol::Equals), _span)) => {
                let expr = self.parse_expression(0)?;
                let end_span = expr.span;
                let stmt = VariableDeclaration {
                    name: S::new(var_name, var_name_span),
                    ty: None,
                    value: expr,
                };
                Ok(S::new(
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
                    name: S::new(var_name, var_name_span),
                    ty: Some(ty),
                    value: expr,
                };
                Ok(S::new(
                    Statement::VariableDeclaration(stmt),
                    first_span.join(end_span),
                ))
            }
            Some((token, span)) => {
                Err(self.unexpected_token(token, span, COLON_OR_EQUAL))
            }
            None => Err(self.unexpected_eof(COLON_OR_EQUAL)),
        }
    }

    fn parse_identifier_statement(&mut self) -> Result<S<Statement>, S<ParseError>> {
        let expr = self.parse_expression(0)?;

        let Some((next_token, _)) = self.iter.peek() else {
            return Err(self.unexpected_eof(ASSIGNMENT_OR_STATEMENT_OR_CURLY));
        };

        match next_token {
            Token::SpecialSymbol(SpecialSymbol::Equals) => {
                self.take_next(); // consume '='
                let r_value_expr = self.parse_expression(0)?;
                let place = Self::expr_to_place(expr)?;
                let span = place.span.join(r_value_expr.span);
                Ok(S::new(Statement::Assignment(place, r_value_expr), span))
            }
            _ => Ok(S::new(Statement::Expr(expr.node), expr.span)),
        }
    }

    fn expr_to_place(expr: S<Expr>) -> Result<S<Place>, S<ParseError>> {
        let (expr, span) = expr.unwrap();
        let place = match expr {
            Expr::Variable(name) => Place::Variable(S::new(name, span)),
            Expr::FieldAccess(FieldAccess { base, field_name }) => {
                let base_place = Self::expr_to_place(*base)?;
                Place::Field {
                    base: Box::new(base_place),
                    field_name,
                }
            }
            _ => {
                return Err(S::new(ParseError::InvalidLeftValue, span));
            }
        };
        Ok(S::new(place, span))
    }

    fn parse_return_statement(&mut self) -> Result<S<Statement>, S<ParseError>> {
        let return_span = self.expect(Token::Keyword(Keyword::Return))?;
        let Some((token, _)) = self.iter.peek() else {
            return Err(self.unexpected_eof(EXPRESSION_OR_CURLY));
        };

        if Self::is_expression_start(token) {
            let expr = self.parse_expression(0)?;
            let end_span = expr.span;
            return Ok(S::new(
                Statement::Return(Some(expr)),
                return_span.join(end_span),
            ));
        }

        Ok(S::new(Statement::Return(None), return_span))
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
                | Token::Keyword(Keyword::Struct)
        )
    }

    fn parse_function_call(
        &mut self,
        id: String,
        span: Span,
    ) -> Result<S<FunctionCall>, S<ParseError>> {
        self.expect(Token::Bracket(Bracket::RoundOpen))?;
        let parse_call_arg = |parser: &mut Self| {
            let expr = parser.parse_expression(0)?;
            Ok(CallArgument {
                name: None,
                value: expr,
            })
        };

        let arguments = self.parse_comma_separated(
            &Token::Bracket(Bracket::RoundClose),
            ARG_OR_ROUND,
            COMMA_OR_ROUND,
            parse_call_arg,
        )?;

        Ok(S::new(
            FunctionCall {
                name: S::new(id, span),
                args: arguments,
            },
            span.join(self.last_span),
        ))
    }

    fn parse_expression(&mut self, min_bp: u8) -> Result<S<Expr>, S<ParseError>> {
        let mut left_expr = self.parse_atomic_expression()?;

        loop {
            // Field access: highest precedence, left-associative postfix
            if let Some((Token::SpecialSymbol(SpecialSymbol::Dot), _span)) =
                self.iter.peek()
            {
                self.take_next(); // consume '.'
                let (field_name, field_span) = self.expect_ident()?;
                let base_span = left_expr.span;
                let span = base_span.join(field_span);
                left_expr = S::new(
                    Expr::FieldAccess(FieldAccess {
                        base: Box::new(left_expr),
                        field_name: S::new(field_name, field_span),
                    }),
                    span,
                );
                continue;
            }

            let op = match self.iter.peek() {
                Some((token, _span)) => match token {
                    // Arithmetic operations
                    Token::SpecialSymbol(SpecialSymbol::Plus) => BinaryOp::Add,
                    Token::SpecialSymbol(SpecialSymbol::Minus) => BinaryOp::Sub,
                    Token::SpecialSymbol(SpecialSymbol::Asterisk) => BinaryOp::Mul,
                    Token::SpecialSymbol(SpecialSymbol::Slash) => BinaryOp::Div,
                    Token::SpecialSymbol(SpecialSymbol::Percent) => BinaryOp::Mod,
                    Token::SpecialSymbol(SpecialSymbol::Backslash) => {
                        BinaryOp::DivInt
                    }
                    Token::SpecialSymbol(SpecialSymbol::DoubleAsterisk) => {
                        BinaryOp::Pow
                    }

                    // Bitwise operations
                    Token::SpecialSymbol(SpecialSymbol::Ampersand) => {
                        BinaryOp::BitAnd
                    }
                    Token::SpecialSymbol(SpecialSymbol::Pipe) => BinaryOp::BitOr,
                    Token::SpecialSymbol(SpecialSymbol::Caret) => BinaryOp::BitXor,
                    Token::SpecialSymbol(SpecialSymbol::DoubleLessThan) => {
                        BinaryOp::Shl
                    }
                    Token::SpecialSymbol(SpecialSymbol::DoubleGreaterThan) => {
                        BinaryOp::Shr
                    }

                    // Logical operations
                    Token::SpecialSymbol(SpecialSymbol::DoubleAmpersand) => {
                        BinaryOp::And
                    }
                    Token::SpecialSymbol(SpecialSymbol::DoublePipe) => BinaryOp::Or,
                    Token::SpecialSymbol(SpecialSymbol::DoubleEquals) => {
                        BinaryOp::Eq
                    }
                    Token::SpecialSymbol(SpecialSymbol::ExclamationEquals) => {
                        BinaryOp::Neq
                    }
                    Token::SpecialSymbol(SpecialSymbol::LessThan) => BinaryOp::Lt,
                    Token::SpecialSymbol(SpecialSymbol::GreaterThan) => BinaryOp::Gt,
                    Token::SpecialSymbol(SpecialSymbol::GreaterThanEquals) => {
                        BinaryOp::Geq
                    }
                    Token::SpecialSymbol(SpecialSymbol::LessThanEquals) => {
                        BinaryOp::Leq
                    }
                    _ => break,
                },
                None => {
                    return Err(self.unexpected_eof(EXPRESSION));
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
            left_expr = S::new(
                Expr::Binary(BinaryExpr {
                    op,
                    left: Box::new(left_expr),
                    right: Box::new(right_expr),
                }),
                span,
            );
        }

        Ok(left_expr)
    }

    /// Parse an atomic expression: a literal, variable, function call.
    fn parse_atomic_expression(&mut self) -> Result<S<Expr>, S<ParseError>> {
        match self.iter.peek() {
            Some((token, _span)) => match token {
                Token::Identifier(_) => {
                    let Some((Token::Identifier(id), span)) = self.take_next()
                    else {
                        unreachable!(); // Unreachable: we peeked and saw Identifier
                    };
                    match id.as_str() {
                        "true" => {
                            // TODO: it's incorrect to be identifier, must be a
                            // literal
                            return Ok(S::new(
                                Expr::Literal(Literal::Bool(true)),
                                span,
                            ));
                        }
                        "false" => {
                            return Ok(S::new(
                                Expr::Literal(Literal::Bool(false)),
                                span,
                            ));
                        }
                        "void" => {
                            return Ok(S::new(Expr::Literal(Literal::Void), span));
                        }
                        _ => {}
                    }
                    match self.iter.peek() {
                        Some((token, _span)) => match token {
                            Token::Bracket(Bracket::RoundOpen) => self
                                .parse_function_call(id, span)
                                .map(|spanned_call| {
                                    spanned_call.map(Expr::FunctionCall)
                                }),
                            _ => Ok(S::new(Expr::Variable(id), span)),
                        },
                        None => Err(self.unexpected_eof(ROUND)),
                    }
                }
                Token::Number(_) => self
                    .parse_number()
                    .map(|spanned_lit| spanned_lit.map(Expr::Literal)),
                Token::Bracket(Bracket::RoundOpen) => {
                    self.take_next(); // consume '('
                    let expr = self.parse_expression(0)?;
                    self.expect(Token::Bracket(Bracket::RoundClose))?;
                    Ok(expr)
                }

                Token::Bracket(Bracket::CurlyOpen) => {
                    // Lookahead to distinguish between struct literal and block
                    match (self.iter.peek_nth(1), self.iter.peek_nth(2)) {
                        // If we see an identifier with `:` after `{`, it's a struct
                        // literal
                        (
                            Some((Token::Identifier(_), _)),
                            Some((Token::SpecialSymbol(SpecialSymbol::Colon), _)),
                        ) => self.parse_struct_literal().map(|spanned_struct| {
                            spanned_struct.map(Expr::StructLiteral)
                        }),
                        // Empty `{}` should be parsed as struct literal
                        (Some((Token::Bracket(Bracket::CurlyClose), _)), _) => {
                            let start_span = self.take_next().unwrap().1; // consume '{'
                            let end_span = self.take_next().unwrap().1; // consume '}'
                            Ok(S::new(
                                Expr::StructLiteral(StructLiteralExpr {
                                    fields: Vec::new(),
                                }),
                                start_span.join(end_span),
                            ))
                        }
                        // Otherwise, it's a block
                        _ => self.parse_block().map(|block| block.map(Expr::Block)),
                    }
                }

                // Unary expressions
                Token::SpecialSymbol(SpecialSymbol::Minus) => {
                    let (_, start_span) = self.take_next().unwrap(); // consume '-'
                    self.parse_atomic_expression().map(|spanned_expr| {
                        let end_span = spanned_expr.span;
                        S::new(
                            Expr::Unary(UnaryExpr {
                                op: UnaryOp::Negate,
                                expr: Box::new(spanned_expr),
                            }),
                            start_span.join(end_span),
                        )
                    })
                }
                Token::SpecialSymbol(SpecialSymbol::Exclamation) => {
                    let (_, start_span) = self.take_next().unwrap(); // consume '!'
                    self.parse_atomic_expression().map(|spanned_expr| {
                        let end_span = spanned_expr.span;
                        S::new(
                            Expr::Unary(UnaryExpr {
                                op: UnaryOp::Not,
                                expr: Box::new(spanned_expr),
                            }),
                            start_span.join(end_span),
                        )
                    })
                }
                Token::SpecialSymbol(SpecialSymbol::Tilde) => {
                    let (_, start_span) = self.take_next().unwrap(); // consume '~'
                    self.parse_atomic_expression().map(|spanned_expr| {
                        let end_span = spanned_expr.span;
                        S::new(
                            Expr::Unary(UnaryExpr {
                                op: UnaryOp::BitNot,
                                expr: Box::new(spanned_expr),
                            }),
                            start_span.join(end_span),
                        )
                    })
                }
                _ => {
                    let (token, span) = self.take_next().unwrap();
                    Err(self.unexpected_token(token, span, EXPRESSION))
                }
            },
            None => Err(self.unexpected_eof(EXPRESSION)),
        }
    }

    fn parse_struct_literal(
        &mut self,
    ) -> Result<S<StructLiteralExpr>, S<ParseError>> {
        let start_span = self.expect(Token::Bracket(Bracket::CurlyOpen))?;

        let fields = self.parse_comma_separated(
            &Token::Bracket(Bracket::CurlyClose),
            STRUCT_FIELD_OR_CURLY,
            COMMA_OR_CURLY,
            Self::parse_struct_literal_field,
        )?;

        let struct_lit = StructLiteralExpr { fields };
        let span = start_span.join(self.last_span);
        Ok(S::new(struct_lit, span))
    }

    fn parse_struct_literal_field(
        &mut self,
    ) -> Result<S<StructLiteralField>, S<ParseError>> {
        let (field_name, field_name_span) = self.expect_ident()?;
        self.expect(Token::SpecialSymbol(SpecialSymbol::Colon))?;
        let value = self.parse_expression(0)?;
        let value_span = value.span;
        Ok(S::new(
            StructLiteralField {
                name: S::new(field_name, field_name_span),
                value,
            },
            field_name_span.join(value_span),
        ))
    }

    fn parse_number(&mut self) -> Result<S<Literal>, S<ParseError>> {
        let (number, number_span) = self.expect_number()?;
        Ok(S::new(Literal::from_number(number), number_span))
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use tokenizer::tokenize;

    use super::*;

    fn parse_and_check_by_display(code: &str, expected_display: &str) {
        let mut token_iter = tokenize(code.char_indices());
        let (ast, errors) = ASTParser::new(&mut token_iter).parse();
        assert!(
            errors.is_empty(),
            "Expected no parsing errors, but got: {errors:#?}"
        );
        assert_eq!(format!("{ast}"), expected_display);
    }

    fn parse_and_get_errors(code: &str) -> Vec<S<ParseError>> {
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
            _ => {
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
            COMMA_OR_CURLY,
        );

        assert_unexpected_token(
            indoc! {"
                type Pos = struct {
                    x: i32,,,,,
                    y: i32,
                }
            "},
            STRUCT_FIELD_OR_CURLY,
        );

        assert_unexpected_token(
            indoc! {"
                type Pos = struct { x: i32 y: i32 }
            "},
            COMMA_OR_CURLY,
        );

        assert_unexpected_token(
            indoc! {"
                type Pos = struct {,
                    x: i32,
                    y: i32,
                }
            "},
            STRUCT_FIELD_OR_CURLY,
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
            COMMA_OR_ROUND,
        );

        assert_unexpected_token(
            indoc! {"
                fn bad(x: i32,, y: i32) {
                    return
                }
            "},
            ARG_OR_ROUND,
        );

        assert_unexpected_token(
            indoc! {"
                fn bad(, x: i32) {
                    return
                }
            "},
            ARG_OR_ROUND,
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
            COMMA_OR_ROUND,
        );

        assert_unexpected_token(
            indoc! {"
                fn main() {
                    foo(1,,2)
                }
            "},
            ARG_OR_ROUND,
        );

        assert_unexpected_token(
            indoc! {"
                fn main() {
                    foo(,1)
                }
            "},
            ARG_OR_ROUND,
        );
    }

    #[test]
    fn test_struct_literal() {
        parse_and_check_by_display(
            indoc! {"
                fn get_pos() -> struct { x: i32, y: i32 } {
                    return { x: 10, y: 20 }
                }
            "},
            indoc! {"
                fn get_pos() -> struct {
                    x: i32,
                    y: i32,
                } {
                    return {
                        x: 10,
                        y: 20,
                    }
                }
            "},
        );
    }

    #[test]
    fn test_struct_literal_with_variable() {
        parse_and_check_by_display(
            indoc! {"
                fn make_pos(a: i32, b: i32) -> struct { x: i32, y: i32 } {
                    return { x: a, y: b }
                }
            "},
            indoc! {"
                fn make_pos(a: i32, b: i32) -> struct {
                    x: i32,
                    y: i32,
                } {
                    return {
                        x: a,
                        y: b,
                    }
                }
            "},
        );
    }

    #[test]
    fn test_nested_struct_literal() {
        parse_and_check_by_display(
            indoc! {"
                fn get_transform() -> struct { pos: struct { x: i32, y: i32 } } {
                    return { pos: { x: 10, y: 20 } }
                }
            "},
            indoc! {"
                fn get_transform() -> struct {
                    pos: struct {
                        x: i32,
                        y: i32,
                    },
                } {
                    return {
                        pos: {
                            x: 10,
                            y: 20,
                        },
                    }
                }
            "},
        );

        parse_and_check_by_display(
            indoc! {"
                fn main() {
                    let x = {
                        nested: {
                            block: {
                                return {
                                    x: 10,
                                    block: {
                                        return {
                                            y: 20,
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            "},
            indoc! {"
                fn main() {
                    let x = {
                        nested: {
                            block: {
                                return {
                                    x: 10,
                                    block: {
                                        return {
                                            y: 20,
                                        }
                                    },
                                }
                            },
                        },
                    }
                }
            "},
        );
    }

    #[test]
    fn test_nested_struct_type_in_type_definition() {
        parse_and_check_by_display(
            indoc! {"
                type Transform = struct {
                    pos: struct {
                        x: i32,
                        y: i32,
                    },
                }
            "},
            indoc! {"
                type Transform = struct {
                    pos: struct {
                        x: i32,
                        y: i32,
                    },
                }
            "},
        );
    }

    #[test]
    fn test_struct_type_in_function_arg() {
        parse_and_check_by_display(
            indoc! {"
                fn print_pos(pos: struct { x: i32, y: i32 }) {}
            "},
            indoc! {"
                fn print_pos(pos: struct {
                    x: i32,
                    y: i32,
                }) {}
            "},
        );
    }

    #[test]
    fn test_field_access() {
        parse_and_check_by_display(
            indoc! {"
                fn main() {
                    let pos = { x: 10, y: 20 }
                    let x = pos.x
                }
            "},
            indoc! {"
                fn main() {
                    let pos = {
                        x: 10,
                        y: 20,
                    }
                    let x = pos.x
                }
            "},
        );
    }

    #[test]
    fn test_assignment() {
        parse_and_check_by_display(
            indoc! {"
                fn main() {
                    let x = 5
                    x = 10
                    let pos = { x: 1, y: 2 }
                    pos.x = 3
                }
            "},
            indoc! {"
                fn main() {
                    let x = 5
                    x = 10
                    let pos = {
                        x: 1,
                        y: 2,
                    }
                    pos.x = 3
                }
            "},
        );
    }
}
