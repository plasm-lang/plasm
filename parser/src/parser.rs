use std::iter::{Filter, Peekable};

use syntax_tree::ast::{
    AST, Block, CallArgument, Expr, Function, FunctionCall, Literal, PrimitiveType, Statement, Type,
};
use tokenizer::{Bracket, Keyword, Number, Span, SpecialSymbol, Token};

use crate::error::ParseError;

pub struct ASTParser<I: Iterator<Item = (Token, Span)>> {
    iter: Peekable<Filter<I, fn(&(Token, Span)) -> bool>>,
    pub errors: Vec<ParseError>,
}

impl<I> ASTParser<I>
where
    I: Iterator<Item = (Token, Span)>,
{
    pub fn new(token_iter: I) -> Self {
        let pred: fn(&(Token, Span)) -> bool = |(token, _)| {
            !matches!(
                token,
                Token::Comment(_) | Token::Whitespace(_) | Token::NewLine
            )
        };
        ASTParser {
            iter: token_iter.filter(pred).peekable(),
            errors: Vec::new(),
        }
    }

    fn expect(&mut self, expected: Token) -> Option<()> {
        match self.iter.next() {
            Some((token, span)) => {
                if token == expected {
                    Some(())
                } else {
                    self.errors.push(ParseError::UnexpectedToken {
                        token,
                        span,
                        expected: format!("{:?}", expected),
                    });
                    None
                }
            }
            None => {
                self.errors.push(ParseError::UnexpectedEOF {
                    expected: format!("{:?}", expected),
                });
                None
            }
        }
    }

    fn expect_extract<F, T>(&mut self, extract: F, expected: &str) -> Option<(T, Span)>
    where
        F: FnOnce(Token) -> Option<T>,
    {
        match self.iter.next() {
            Some((token, span)) => match extract(token.clone()) {
                Some(val) => Some((val, span)),
                None => {
                    self.errors.push(ParseError::UnexpectedToken {
                        token,
                        span,
                        expected: expected.to_string(),
                    });
                    None
                }
            },
            None => {
                self.errors.push(ParseError::UnexpectedEOF {
                    expected: expected.to_string(),
                });
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

    pub fn parse(mut self) -> (AST, Vec<ParseError>) {
        let mut ast = AST::new();

        while let Some((token, _)) = self.iter.peek() {
            match token {
                Token::Keyword(Keyword::Fn) => {
                    if let Some(func) = self.parse_function() {
                        ast.add_function(func);
                    }
                }
                _ => {
                    let Some((token, span)) = self.iter.next() else {
                        continue;
                    };
                    let err = ParseError::UnexpectedToken {
                        token,
                        span,
                        expected: "function".to_string(),
                    };
                    self.errors.push(err);
                }
            }
        }

        (ast, self.errors)
    }

    fn parse_function(&mut self) -> Option<Function> {
        self.expect(Token::Keyword(Keyword::Fn))?;
        let (func_name, _func_name_span) = self.expect_ident()?;
        self.expect(Token::Bracket(Bracket::RoundOpen))?;
        self.expect(Token::Bracket(Bracket::RoundClose))?;
        let block = self.parse_block()?;

        let func = Function {
            name: func_name,
            args: vec![],
            return_type: Type::Primitive(PrimitiveType::Void),
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
                        self.iter.next();
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
                    self.errors.push(ParseError::UnexpectedEOF {
                        expected: "statement or end of the block".to_string(),
                    });
                }
            }
        }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.iter.peek() {
            Some((token, _span)) => match token {
                Token::Keyword(Keyword::New) => self.parse_variable_declaration(),
                Token::Identifier(_) => {
                    let Some((Token::Identifier(id), _span)) = self.iter.next() else {
                        unreachable!(); // Unreachable: we peeked and saw Identifier
                    };
                    match self.iter.peek() {
                        Some((token, _span)) => match token {
                            Token::Bracket(Bracket::RoundOpen) => {
                                self.parse_function_call(id).map(Statement::FunctionCall)
                            }
                            Token::SpecialSymbol(SpecialSymbol::Equals) => todo!(), // Variable assignment
                            _ => {
                                let (token, span) = self.iter.next()?;
                                let err = ParseError::UnexpectedToken {
                                    token,
                                    span,
                                    expected: "'(' or '='".to_string(),
                                };
                                self.errors.push(err);
                                None
                            }
                        },
                        None => {
                            self.iter.next();
                            let err = ParseError::UnexpectedEOF {
                                expected: "'(' or '='".to_string(),
                            };
                            self.errors.push(err);
                            None
                        }
                    }
                }
                _ => {
                    let (token, span) = self.iter.next()?;
                    let err = ParseError::UnexpectedToken {
                        token,
                        span,
                        expected: "statement (function call, new variable, return, etc.)"
                            .to_string(),
                    };
                    self.errors.push(err);
                    None
                }
            },
            None => {
                self.errors.push(ParseError::UnexpectedEOF {
                    expected: "statement (function call, new variable, return, etc.)".to_string(),
                });
                None
            }
        }
    }

    fn parse_function_call(&mut self, id: String) -> Option<FunctionCall> {
        self.expect(Token::Bracket(Bracket::RoundOpen))?;
        let mut arguments = Vec::new();
        loop {
            match self.iter.peek() {
                Some((token, _span)) => match token {
                    Token::Bracket(Bracket::RoundClose) => {
                        self.iter.next();
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
                    self.errors.push(ParseError::UnexpectedEOF {
                        expected: "function argument or ')'".to_string(),
                    });
                    return None;
                }
            }
        }

        Some(FunctionCall {
            name: id,
            args: arguments,
        })
    }

    fn parse_variable_declaration(&mut self) -> Option<Statement> {
        self.expect(Token::Keyword(Keyword::New))?;
        let (var_name, _var_name_span) = self.expect_ident()?;
        self.expect(Token::SpecialSymbol(SpecialSymbol::Colon))?;
        let (type_name, _type_name_span) = self.expect_ident()?;
        let type_ = Type::from_str(&type_name);
        self.expect(Token::SpecialSymbol(SpecialSymbol::Equals))?;
        let expr = self.parse_expression()?;

        let stmt = Statement::VariableDeclaration {
            name: var_name,
            type_,
            value: expr,
        };
        Some(stmt)
    }

    fn parse_expression(&mut self) -> Option<Expr> {
        match self.iter.peek() {
            Some((token, _span)) => match token {
                Token::Identifier(_) => {
                    let Some((Token::Identifier(id), _span)) = self.iter.next() else {
                        unreachable!(); // Unreachable: we peeked and saw Identifier
                    };
                    match self.iter.peek() {
                        Some((token, _span)) => match token {
                            Token::Bracket(Bracket::RoundOpen) => {
                                self.parse_function_call(id).map(Expr::FunctionCall)
                            }
                            _ => Some(Expr::Variable(id)),
                        },
                        None => {
                            let err = ParseError::UnexpectedEOF {
                                expected: "'('".to_string(),
                            };
                            self.errors.push(err);
                            None
                        }
                    }
                }
                Token::Number(_) => self.parse_number().map(Expr::Literal),
                _ => {
                    let (token, span) = self.iter.next()?;
                    let err = ParseError::UnexpectedToken {
                        token,
                        span,
                        expected: "expression".to_string(),
                    };
                    self.errors.push(err);
                    None
                }
            },
            None => {
                self.errors.push(ParseError::UnexpectedEOF {
                    expected: "expression".to_string(),
                });
                None
            }
        }
    }

    fn parse_number(&mut self) -> Option<Literal> {
        let (number, _number_span) = self.expect_number()?;
        Some(Literal::from_number(number))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use syntax_tree::ast::Item;
    use tokenizer::tokenize;

    #[test]
    fn test_basic_code_cst_parsing() {
        let code = indoc! {"
            // Basic inline comment

            /* Multiline
            comment
            */
            fn main() {
                new x: i32 = 5
                print(x)
            }"};

        let token_iter = tokenize(code.char_indices());
        let (ast, errors) = ASTParser::new(token_iter).parse();

        let expected = AST {
            items: vec![Item::Function(Function {
                name: "main".to_string(),
                args: vec![],
                return_type: Type::Primitive(PrimitiveType::Void),
                body: vec![
                    Statement::VariableDeclaration {
                        name: "x".to_string(),
                        type_: Type::Primitive(PrimitiveType::I32),
                        value: Expr::Literal(Literal::Integer("5".to_string())),
                    },
                    Statement::FunctionCall(FunctionCall {
                        name: "print".to_string(),
                        args: vec![CallArgument {
                            name: None,
                            value: Expr::Variable("x".to_string()),
                        }],
                    }),
                ],
            })],
        };
        assert_eq!(ast, expected);
        assert_eq!(errors, vec![]);
    }
}
