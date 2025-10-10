use std::iter::{Filter, Peekable};

use super::ast::{
    AST, Block, CallArgument, Expr, Function, FunctionCall, Literal, Statement, Type,
    VariableDeclaration,
};
use diagnostic::{Span, Spanned};
use tokenizer::{Bracket, Keyword, Number, SpecialSymbol, Token};

use crate::{Argument, error::ParseError};

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

    fn expect(&mut self, expected: Token) -> Option<Span> {
        match self.take_next() {
            Some((token, span)) => {
                if token == expected {
                    Some(span)
                } else {
                    self.errors.push(Spanned::new(
                        ParseError::UnexpectedToken {
                            token,
                            expected: format!("`{expected}`"),
                        },
                        span,
                    ));
                    None
                }
            }
            None => {
                self.errors.push(Spanned::new(
                    ParseError::UnexpectedEOF {
                        expected: format!("`{expected:?}`"),
                    },
                    self.last_span,
                ));
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
                    self.errors.push(Spanned::new(
                        ParseError::UnexpectedToken {
                            token,
                            expected: expected.to_string(),
                        },
                        span,
                    ));
                    None
                }
            },
            None => {
                self.errors.push(Spanned::new(
                    ParseError::UnexpectedEOF {
                        expected: expected.to_string(),
                    },
                    self.last_span,
                ));
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

    pub fn parse(mut self) -> (AST, Vec<Spanned<ParseError>>) {
        let mut ast = AST::new();

        while let Some((token, _)) = self.iter.peek() {
            match token {
                Token::Keyword(Keyword::Fn) => {
                    if let Some(func) = self.parse_function() {
                        ast.add_function(func);
                    }
                }
                _ => {
                    let Some((token, span)) = self.take_next() else {
                        continue;
                    };
                    let err = ParseError::UnexpectedToken {
                        token,
                        expected: "function definition".to_string(),
                    };
                    self.errors.push(Spanned::new(err, span));
                }
            }
        }

        (ast, self.errors)
    }

    fn parse_function(&mut self) -> Option<Function> {
        self.expect(Token::Keyword(Keyword::Fn))?;
        let (func_name, func_name_span) = self.expect_ident()?;
        self.expect(Token::Bracket(Bracket::RoundOpen))?;

        // parse arguments

        let mut args = Vec::new();
        loop {
            match self.iter.peek() {
                Some((token, _span)) => match token {
                    Token::Identifier(_) => {
                        let (arg_name, arg_name_span) = self.expect_ident()?;
                        self.expect(Token::SpecialSymbol(SpecialSymbol::Colon))?;
                        let (type_name, type_name_span) = self.expect_ident()?;
                        let ty = Type::from_str(&type_name);
                        let arg = Argument {
                            name: Spanned::new(arg_name, arg_name_span),
                            ty: Spanned::new(ty, type_name_span),
                        };
                        let arg_span = arg.name.span.join(arg.ty.span);
                        args.push(Spanned::new(arg, arg_span));
                    }
                    Token::Bracket(Bracket::RoundClose) => {
                        self.take_next();
                        break;
                    }
                    Token::SpecialSymbol(SpecialSymbol::Comma) => {
                        self.take_next();
                    }
                    _ => {
                        let (token, span) = self.take_next()?;
                        let err = ParseError::UnexpectedToken {
                            token,
                            expected: "function argument or `)`".to_string(),
                        };
                        self.errors.push(Spanned::new(err, span));
                        return None;
                    }
                },
                None => {
                    self.errors.push(Spanned::new(
                        ParseError::UnexpectedEOF {
                            expected: "function argument or `)`".to_string(),
                        },
                        self.last_span,
                    ));
                    return None;
                }
            }
        }

        let (block, return_type) = match self.iter.peek() {
            Some((token, _span)) => match token {
                // Function without return type
                Token::Bracket(Bracket::CurlyOpen) => {
                    let block = self.parse_block()?.node;
                    (block, None)
                }
                // Function with return type
                Token::SpecialSymbol(SpecialSymbol::Minus) => {
                    self.take_next(); // consume '-'
                    self.expect(Token::SpecialSymbol(SpecialSymbol::GreaterThan))?;
                    let (type_name, type_name_span) = self.expect_ident()?;
                    let ty = Type::from_str(&type_name);
                    let return_type = Some(Spanned::new(ty, type_name_span));
                    let block = self.parse_block()?.node;
                    (block, return_type)
                }
                _ => {
                    let (token, span) = self.take_next()?;
                    let err = ParseError::UnexpectedToken {
                        token,
                        expected: "`-> T {` or `{`".to_string(),
                    };
                    self.errors.push(Spanned::new(err, span));
                    return None;
                }
            },
            None => {
                self.errors.push(Spanned::new(
                    ParseError::UnexpectedEOF {
                        expected: "`-> T {` or `{`".to_string(),
                    },
                    self.last_span,
                ));
                return None;
            }
        };

        let func = Function {
            name: Spanned::new(func_name, func_name_span),
            args,
            return_type,
            body: block,
        };
        Some(func)
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
                    self.errors.push(Spanned::new(
                        ParseError::UnexpectedEOF {
                            expected: "statement or `}`".to_string(),
                        },
                        self.last_span,
                    ));
                    break Some(Spanned::new(block, start_span.join(self.last_span)));
                }
            }
        }
    }

    fn parse_statement(&mut self) -> Option<Spanned<Statement>> {
        match self.iter.peek() {
            Some((token, _span)) => match token {
                Token::Keyword(Keyword::Let) => self.parse_variable_declaration(),
                Token::Identifier(_) => {
                    let Some((Token::Identifier(id), span)) = self.take_next() else {
                        unreachable!(); // Unreachable: we peeked and saw Identifier
                    };
                    match self.iter.peek() {
                        Some((token, _span)) => match token {
                            Token::Bracket(Bracket::RoundOpen) => {
                                self.parse_function_call(id, span).map(|spanned_call| {
                                    spanned_call.map(Expr::FunctionCall).map(Statement::Expr)
                                })
                            }
                            Token::SpecialSymbol(SpecialSymbol::Equals) => todo!(), // Variable assignment
                            _ => {
                                let (token, span) = self.take_next()?;
                                let err = ParseError::UnexpectedToken {
                                    token,
                                    expected: "`(` or `=`".to_string(),
                                };
                                self.errors.push(Spanned::new(err, span));
                                None
                            }
                        },
                        None => {
                            self.take_next();
                            let err = ParseError::UnexpectedEOF {
                                expected: "`(` or `=`".to_string(),
                            };
                            self.errors.push(Spanned::new(err, self.last_span));
                            None
                        }
                    }
                }
                Token::Keyword(Keyword::Return) => {
                    let (_, return_span) = self.take_next()?;
                    match self.iter.peek() {
                        Some((token, _span)) => match token {
                            Token::Identifier(_)
                            | Token::Number(_)
                            | Token::Bracket(Bracket::RoundOpen)
                            | Token::Bracket(Bracket::CurlyOpen) => {
                                let expr = self.parse_expression()?;
                                let end_span = expr.span;
                                Some(Spanned::new(
                                    Statement::Return(Some(expr)),
                                    return_span.join(end_span),
                                ))
                            }
                            _ => Some(Spanned::new(Statement::Return(None), return_span)),
                        },
                        None => {
                            self.take_next();
                            let err = ParseError::UnexpectedEOF {
                                expected: "expression or new line with `}`".to_string(),
                            };
                            self.errors.push(Spanned::new(err, self.last_span));
                            None
                        }
                    }
                }
                _ => {
                    let (token, span) = self.take_next()?;
                    let err = ParseError::UnexpectedToken {
                        token,
                        expected: "statement (function call, new variable, return, etc.)"
                            .to_string(),
                    };
                    self.errors.push(Spanned::new(err, span));
                    None
                }
            },
            None => {
                self.errors.push(Spanned::new(
                    ParseError::UnexpectedEOF {
                        expected: "statement (function call, new variable, return, etc.)"
                            .to_string(),
                    },
                    self.last_span,
                ));
                None
            }
        }
    }

    fn parse_function_call(&mut self, id: String, span: Span) -> Option<Spanned<FunctionCall>> {
        self.expect(Token::Bracket(Bracket::RoundOpen))?;
        let mut arguments = Vec::new();
        let end_span: Span;
        loop {
            match self.iter.peek() {
                Some((token, next_span)) => match token {
                    Token::Bracket(Bracket::RoundClose) => {
                        end_span = *next_span;
                        self.take_next();
                        break;
                    }
                    Token::SpecialSymbol(SpecialSymbol::Comma) => {
                        self.take_next();
                    }
                    _ => {
                        let Some(expr) = self.parse_expression() else {
                            continue;
                        };
                        let arg = CallArgument {
                            name: None,
                            value: expr,
                        };
                        arguments.push(arg);
                    }
                },
                None => {
                    self.errors.push(Spanned::new(
                        ParseError::UnexpectedEOF {
                            expected: "function argument or ')'".to_string(),
                        },
                        self.last_span,
                    ));
                    return None;
                }
            }
        }

        Some(Spanned::new(
            FunctionCall {
                name: Spanned::new(id, span),
                args: arguments,
            },
            span.join(end_span),
        ))
    }

    fn parse_variable_declaration(&mut self) -> Option<Spanned<Statement>> {
        let first_span = self.expect(Token::Keyword(Keyword::Let))?;
        let (var_name, var_name_span) = self.expect_ident()?;

        match self.take_next() {
            // If type is not specified, expect '=' next
            Some((Token::SpecialSymbol(SpecialSymbol::Equals), _span)) => {
                let expr = self.parse_expression()?;
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
                let (type_name, type_name_span) = self.expect_ident()?;
                let ty = Type::from_str(&type_name);
                self.expect(Token::SpecialSymbol(SpecialSymbol::Equals))?;
                let expr = self.parse_expression()?;
                let end_span = expr.span;
                let stmt = VariableDeclaration {
                    name: Spanned::new(var_name, var_name_span),
                    ty: Some(Spanned::new(ty, type_name_span)),
                    value: expr,
                };
                Some(Spanned::new(
                    Statement::VariableDeclaration(stmt),
                    first_span.join(end_span),
                ))
            }
            Some((token, span)) => {
                self.errors.push(Spanned::new(
                    ParseError::UnexpectedToken {
                        token,
                        expected: "`:` or `=`".to_string(),
                    },
                    span,
                ));
                None
            }
            None => {
                self.errors.push(Spanned::new(
                    ParseError::UnexpectedEOF {
                        expected: "`:` or `=`".to_string(),
                    },
                    self.last_span,
                ));
                None
            }
        }
    }

    fn parse_expression(&mut self) -> Option<Spanned<Expr>> {
        match self.iter.peek() {
            Some((token, _span)) => match token {
                Token::Identifier(_) => {
                    let Some((Token::Identifier(id), span)) = self.take_next() else {
                        unreachable!(); // Unreachable: we peeked and saw Identifier
                    };
                    match id.as_str() {
                        "true" => {
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
                            _ => {
                                println!("Parsed variable: {id}");
                                Some(Spanned::new(Expr::Variable(id), span))
                            }
                        },
                        None => {
                            let err = ParseError::UnexpectedEOF {
                                expected: "'('".to_string(),
                            };
                            self.errors.push(Spanned::new(err, self.last_span));
                            None
                        }
                    }
                }
                Token::Number(_) => self
                    .parse_number()
                    .map(|spanned_lit| spanned_lit.map(Expr::Literal)),
                _ => {
                    let (token, span) = self.take_next()?;
                    let err = ParseError::UnexpectedToken {
                        token,
                        expected: "expression".to_string(),
                    };
                    self.errors.push(Spanned::new(err, span));
                    None
                }
            },
            None => {
                self.errors.push(Spanned::new(
                    ParseError::UnexpectedEOF {
                        expected: "expression".to_string(),
                    },
                    self.last_span,
                ));
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

            fn multi_line(x: i32, y: f64, z: bool) -> void {
                return
            }
        "};

        parse_and_check_by_display(code, expected_display);
    }
}
