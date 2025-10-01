use std::iter::{Filter, Peekable};

use super::ast::{
    AST, Block, CallArgument, Expr, Function, FunctionCall, Literal, Statement, Type,
    VariableDeclaration,
};
use diagnostic::{Span, Spanned};
use tokenizer::{Bracket, Keyword, Number, SpecialSymbol, Token};

use crate::error::ParseError;

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

    fn expect(&mut self, expected: Token) -> Option<()> {
        match self.take_next() {
            Some((token, span)) => {
                if token == expected {
                    Some(())
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
        self.expect(Token::Bracket(Bracket::RoundClose))?;
        let block = self.parse_block()?;

        let func = Function {
            name: Spanned::new(func_name, func_name_span),
            args: vec![],
            return_type: None,
            body: block,
        };
        Some(func)
    }

    fn parse_block(&mut self) -> Option<Block> {
        self.expect(Token::Bracket(Bracket::CurlyOpen))?;
        let mut block = Vec::new();

        loop {
            match self.iter.peek() {
                Some((token, _span)) => match token {
                    Token::Bracket(Bracket::CurlyClose) => {
                        self.take_next();
                        break Some(block);
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
                    break Some(block);
                }
            }
        }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.iter.peek() {
            Some((token, _span)) => match token {
                Token::Keyword(Keyword::Let) => self.parse_variable_declaration(),
                Token::Identifier(_) => {
                    let Some((Token::Identifier(id), span)) = self.take_next() else {
                        unreachable!(); // Unreachable: we peeked and saw Identifier
                    };
                    match self.iter.peek() {
                        Some((token, _span)) => match token {
                            Token::Bracket(Bracket::RoundOpen) => self
                                .parse_function_call(id, span)
                                .map(Statement::FunctionCall),
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

    fn parse_function_call(&mut self, id: String, span: Span) -> Option<FunctionCall> {
        self.expect(Token::Bracket(Bracket::RoundOpen))?;
        let mut arguments = Vec::new();
        loop {
            match self.iter.peek() {
                Some((token, _span)) => match token {
                    Token::Bracket(Bracket::RoundClose) => {
                        self.take_next();
                        break;
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

        Some(FunctionCall {
            name: Spanned::new(id, span),
            args: arguments,
        })
    }

    fn parse_variable_declaration(&mut self) -> Option<Statement> {
        self.expect(Token::Keyword(Keyword::Let))?;
        let (var_name, var_name_span) = self.expect_ident()?;
        self.expect(Token::SpecialSymbol(SpecialSymbol::Colon))?;
        let (type_name, type_name_span) = self.expect_ident()?;
        let ty = Type::from_str(&type_name);
        self.expect(Token::SpecialSymbol(SpecialSymbol::Equals))?;
        let expr = self.parse_expression()?;

        let stmt = VariableDeclaration {
            name: Spanned::new(var_name, var_name_span),
            ty: Some(Spanned::new(ty, type_name_span)),
            value: expr,
        };
        Some(Statement::VariableDeclaration(stmt))
    }

    fn parse_expression(&mut self) -> Option<Expr> {
        match self.iter.peek() {
            Some((token, _span)) => match token {
                Token::Identifier(_) => {
                    let Some((Token::Identifier(id), span)) = self.take_next() else {
                        unreachable!(); // Unreachable: we peeked and saw Identifier
                    };
                    match self.iter.peek() {
                        Some((token, _span)) => match token {
                            Token::Bracket(Bracket::RoundOpen) => {
                                self.parse_function_call(id, span).map(Expr::FunctionCall)
                            }
                            _ => Some(Expr::Variable(Spanned::new(id, span))),
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
                Token::Number(_) => self.parse_number().map(Expr::Literal),
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

    fn parse_number(&mut self) -> Option<Literal> {
        let (number, number_span) = self.expect_number()?;
        Some(Literal::from_number(number, number_span))
    }
}

#[cfg(test)]
mod tests {
    use super::super::ast::{Item, PrimitiveType};
    use super::*;
    use indoc::indoc;
    use tokenizer::tokenize;

    #[test]
    fn test_basic_code_cst_parsing() {
        let code = indoc! {"
            // Basic inline comment

            /* Multiline
            comment
            */
            fn main() {
                let x: i32 = 5
                print(x)
            }"};

        let mut token_iter = tokenize(code.char_indices());
        let (ast, errors) = ASTParser::new(&mut token_iter).parse();

        let expected = AST {
            items: vec![Item::Function(Function {
                name: Spanned {
                    node: "main".to_string(),
                    span: Span { start: 52, end: 56 },
                },
                args: vec![],
                return_type: None,
                body: vec![
                    Statement::VariableDeclaration(VariableDeclaration {
                        name: Spanned {
                            node: "x".to_string(),
                            span: Span { start: 69, end: 70 },
                        },
                        ty: Some(Spanned {
                            node: Type::Primitive(PrimitiveType::I32),
                            span: Span { start: 72, end: 75 },
                        }),
                        value: Expr::Literal(Literal::Integer(Spanned {
                            node: "5".to_string(),
                            span: Span { start: 78, end: 79 },
                        })),
                    }),
                    Statement::FunctionCall(FunctionCall {
                        name: Spanned {
                            node: "print".to_string(),
                            span: Span { start: 84, end: 89 },
                        },
                        args: vec![CallArgument {
                            name: None,
                            value: Expr::Variable(Spanned {
                                node: "x".to_string(),
                                span: Span { start: 90, end: 91 },
                            }),
                        }],
                    }),
                ],
            })],
        };

        assert_eq!(ast, expected);
        assert_eq!(errors, vec![]);
    }
}
