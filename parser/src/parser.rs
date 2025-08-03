use std::mem::take;

use syntax_tree::cst::{CST, CSTNode, CSTNodeKind};
use tokenizer::{tokenize, Keyword, LinesTable, Span, Token, TokenIter};

use crate::error::{ParseError, Result};

pub fn parse<I: Iterator<Item = char>>(mut token_iter: TokenIter<I>) -> (CST, Vec<ParseError>) {
    let mut errors = Vec::new();
    let mut cst = CST::new();

    loop {
        let Some((token, span)) = token_iter.next() else {
            return (cst, errors);
        };

        match token {
            Token::Comment(comment) => {
                cst.add_node(CSTNode::Token { token: Token::Comment(comment), span });
            }
            Token::Whitespace(amount) => {
                cst.add_node(CSTNode::Token { token: Token::Whitespace(amount), span });
            }
            Token::NewLine => {
                cst.add_node(CSTNode::Token { token: Token::NewLine, span });
            }
            Token::Keyword(Keyword::Fn) => {
                let (func_node, func_errors) = parse_function(&mut token_iter);
                cst.add_node(func_node);
                errors.extend(func_errors);
            }
            _ => {
                let err =  ParseError::UnexpectedToken {
                    token,
                    span,
                    lines_table: token_iter.lines_table().clone(),
                    // code: self.token_iter.code.clone(),
                    expected: "function or comment".to_string(),
                };
                errors.push(err);
            }
        }
    }
}

macro_rules! expect {
    ($iter:expr, $span:expr, $children:expr, $errors:expr, $pat:pat) => {
        {
            match $iter.next() {
                Some((token, span)) => {
                    match token {
                        $pat => (token, span),
                        _ => {
                            $errors.push(ParseError::UnexpectedToken {
                                token,
                                span,
                                lines_table: $iter.lines_table().clone(),
                                expected: stringify!($pat).to_string(),
                            });
                            return (CSTNode::Node {
                                kind: CSTNodeKind::Function,
                                children: $children,
                                span,
                            }, $errors);
                        }
                    }
                }
                None => {
                    $errors.push(ParseError::UnexpectedEOF {
                        expected: stringify!($pat).to_string(),
                    });
                    return (CSTNode::Node {
                        kind: CSTNodeKind::Function,
                        children: $children,
                        span: $span,
                    }, $errors);
                }
            }
        }
    };
}

fn parse_function<I: Iterator<Item = char>>(
    token_iter: &mut TokenIter<I>
) -> (CSTNode, Vec<ParseError>) {
    let mut errors = Vec::new();
    let mut children = Vec::new();
    let mut span = Span::new(0, 0);

    let (space_1, space_1_span) = expect!(token_iter, span, children, errors, Token::Whitespace(_));
    children.push(CSTNode::Token { token: space_1, span: space_1_span });

    let (func_name, func_name_span) = expect!(token_iter, span, children, errors, Token::Identifier(_));
    children.push(CSTNode::Token { token: func_name, span: func_name_span });

    let (space_2, space_2_span) = expect!(token_iter, span, children, errors, Token::Whitespace(_));
    children.push(CSTNode::Token { token: space_2, span: space_2_span });

    (CSTNode::Node {
        kind: CSTNodeKind::Function,
        children,
        span,
    }, errors)
}

pub struct ParseIter<I: Iterator<Item = char>> {
    token_iter: TokenIter<I>,
    cst: CST,
    done: bool,
}

// impl <I: Iterator<Item = char>> ParseIter<I> {
//     pub fn new(token_iter: TokenIter<I>) -> Self {
//         Self {
//             token_iter,
//             cst: CST::new(),
//             done: false,
//         }
//     }
// }

// impl<I: Iterator<Item = char>> Iterator for ParseIter<I> {
//     type Item = Result<CST>;

//     fn next(&mut self) -> Option<Self::Item> {
//         if self.done {
//             return None;
//         }

//         loop {
//             let Some((token, span)) = self.token_iter.next() else {
//                 self.done = true;
//                 return Some(Ok(take(&mut self.cst)));
//             };

//             match token {
//                 Token::Comment(comment) => {
//                     self.cst.add_node(CSTNode::Token { token: Token::Comment(comment), span });
//                 }
//                 Token::Whitespace(amount) => {
//                     self.cst.add_node(CSTNode::Token { token: Token::Whitespace(amount), span });
//                 }
//                 Token::NewLine => {
//                     self.cst.add_node(CSTNode::Token { token: Token::NewLine, span });
//                 }
//                 Token::Keyword(Keyword::Fn) => {
//                     let func_name = expect!(self, Token::Identifier(_));
//                 }
//                 _ => {
//                     return Some(Err(ParseError::UnexpectedToken {
//                         token,
//                         span,
//                         lines_table: self.token_iter.lines_table().clone(),
//                         // code: self.token_iter.code.clone(),
//                         expected: "function or comment".to_string(),
//                     }));
//                 }
//             }
//         }
//     }
// }

#[cfg(test)]
mod tests {
    use syntax_tree::cst;

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
                let x: i32 = 5
                print(x)
            }
        "#;

        let code = prepare_string(code);
        let token_iter = tokenize(code.chars());
        let (cst, errors) = parse(token_iter);
        println!("{:#?}", cst);
        println!("Errors: {:#?}", errors);
        // let cst = parse_iter.find_map(Result::ok).unwrap();
        // println!("{:#?}", cst);
    }
}
