use std::iter::{Filter, Peekable};

use syntax_tree::ast::{AST, Block, Expr, Function, Literal, PrimitiveType, Statment, Type};
use tokenizer::{tokenize, Bracket, Keyword, LinesTable, Number, Span, SpecialSymbol, Token, TokenIter};

use crate::error::{ParseError, Result};

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
        return Some(func);
    }

    fn parse_block(&mut self) -> Option<Block> {
        self.expect(Token::Bracket(Bracket::CurlyOpen))?;
        let mut block = Vec::new();

        loop {
            match self.iter.peek() {
                Some((token, _span)) => match token {
                    Token::Keyword(Keyword::New) => {
                        let Some(stmt) = self.parse_variable_declaration() else {
                            continue;
                        };
                        block.push(stmt);
                    }
                    Token::Bracket(Bracket::CurlyClose) => {
                        self.iter.next();
                        break Some(block)
                    },
                    _ => {
                        let Some((token, span)) = self.iter.next() else {
                            continue;
                        };
                        self.errors.push(ParseError::UnexpectedToken {
                            token,
                            span,
                            expected: "statment or end of the block".to_string(),
                        })
                    }
                },
                None => {
                    self.errors.push(ParseError::UnexpectedEOF {
                        expected: "statment or end of the block".to_string(),
                    });
                }
            }
        }
    }

    fn parse_variable_declaration(&mut self) -> Option<Statment> {
        self.expect(Token::Keyword(Keyword::New))?;
        let (var_name, _var_name_span) = self.expect_ident()?;
        self.expect(Token::SpecialSymbol(SpecialSymbol::Colon))?;
        let (type_name, _type_name_span) = self.expect_ident()?;
        let type_ = Type::from_str(&type_name);
        self.expect(Token::SpecialSymbol(SpecialSymbol::Equals))?;
        let expr = self.parse_expression()?;

        let stmt = Statment::VariableDeclaration {
            name: var_name,
            type_,
            value: expr,
        };
        Some(stmt)
    }

    fn parse_expression(&mut self) -> Option<Expr> {
        let (number, _number_span) = self.expect_number()?;
        Some(Expr::Literal(Literal::from_number(number)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn prepare_string(input: &str) -> String {
        input
            .trim_start()
            .trim_end()
            .lines()
            // remove 12 spaces
            .map(|l| l.trim_start_matches("            "))
            .collect::<Vec<_>>()
            .join("\n")
    }

    #[test]
    fn test_basic_code_cst_parsing() {
        let code = r#"
            // Basic inline comment

            /* Multiline
            comment
            */
            fn main() {
                new x: i32 = 5
                print(x)
            }
        "#;

        let code = prepare_string(code);
        let token_iter = tokenize(code.char_indices());
        let (ast, errors) = ASTParser::new(token_iter).parse();
        println!("{:#?}", ast);
        println!("Errors: {:#?}", errors);
    }
}
