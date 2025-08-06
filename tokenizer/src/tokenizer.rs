use std::iter::{Enumerate, Peekable};
use std::mem::take;

use super::diagnostic::{LinesTable, Span};
use super::token::{Bracket, Comment, Keyword, Number, SpecialSymbol, Token};

// pub struct Tokenizer<I: Iterator<Item = char>> {
//     chars: Peekable<Enumerate<I>>,
// }

// impl<I: Iterator<Item = char>> Tokenizer<I> {
//     pub fn new(code_iter: I) -> Self {
//         Tokenizer {
//             chars: code_iter.enumerate().peekable(),
//         }
//     }

//     pub fn tokenize(self) -> TokenIter<I> {
//         TokenIter {
//             chars: self.chars,
//             state: State::Default,
//             accumulated: String::new(),
//             lines_table: LinesTable::new(),
//         }
//     }
// }

pub fn tokenize<I: Iterator<Item = (usize, char)>>(chars: I) -> TokenIter<I> {
    TokenIter {
        chars: chars.peekable(),
        state: State::Default,
        accumulated: String::new(),
        lines_table: LinesTable::new(),
    }
}

enum State {
    Default,
    InSingleComment,
    InMultiComment,
}

pub struct TokenIter<I: Iterator<Item = (usize, char)>> {
    chars: Peekable<I>,
    state: State,
    accumulated: String,
    lines_table: LinesTable,
}

impl<I: Iterator<Item = (usize, char)>> TokenIter<I> {
    pub fn lines_table(&self) -> &LinesTable {
        &self.lines_table
    }

    fn lex_whitespace(&mut self) -> Option<(Token, Span)> {
        let mut last_i = 0;
        while let Some((i, ch)) = self.chars.peek() {
            last_i = *i;
            if ch.is_whitespace() {
                self.accumulated.push(*ch);
                self.chars.next(); // consume the whitespace character
            } else {
                break;
            }
        }

        let whitespace_len = self.accumulated.len();
        let span = Span::new(last_i - self.accumulated.as_bytes().len(), last_i);
        self.accumulated.clear();
        Some((Token::Whitespace(whitespace_len), span))
    }

    fn lex_number(&mut self) -> Option<(Token, Span)> {
        let mut last_i = 0;
        while let Some((i, ch)) = self.chars.peek() {
            last_i = *i;
            if ch.is_digit(10) || *ch == '.' || *ch == '_' {
                self.accumulated.push(*ch);
                self.chars.next(); // consume the digit
            } else {
                break;
            }
        }

        let span = Span::new(last_i - self.accumulated.bytes().len(), last_i);
        let number = if self.accumulated.contains('.') {
            Number::Float(take(&mut self.accumulated))
        } else {
            Number::Integer(take(&mut self.accumulated))
        };
        Some((Token::Number(number), span))
    }

    fn lex_alpahanumeric(&mut self) -> Option<(Token, Span)> {
        let mut last_i = 0;
        while let Some((i, ch)) = self.chars.peek() {
            last_i = *i;
            if ch.is_alphanumeric() || *ch == '_' {
                self.accumulated.push(*ch);
                self.chars.next(); // consume the alphanumeric character
            } else {
                break;
            }
        }

        let token = match self.accumulated.as_str() {
            "fn" => Token::Keyword(Keyword::Fn),
            "new" => Token::Keyword(Keyword::New),
            _ => Token::Identifier(self.accumulated.clone()),
        };
        let span = Span::new(last_i - self.accumulated.as_bytes().len(), last_i);
        self.accumulated.clear();
        Some((token, span))
    }

    fn lex_default(&mut self) -> Option<(Token, Span)> {
        while let Some((i, ch)) = self.chars.next() {
            match ch {
                '/' if self.chars.peek().map(|(_, ch)| ch) == Some(&'/') => {
                    self.state = State::InSingleComment;
                    self.accumulated.clear();
                    self.chars.next(); // consume the second '/'
                    return self.lex_single_comment();
                }
                '/' if self.chars.peek().map(|(_, ch)| ch) == Some(&'*') => {
                    self.state = State::InMultiComment;
                    self.accumulated.clear();
                    self.chars.next(); // consume the '*'
                    return self.lex_multiline_comment();
                }
                '\n' => {
                    let char_len = ch.len_utf8();
                    self.lines_table.add_line(i + char_len);
                    return Some((Token::NewLine, Span::new(i, i + char_len)));
                }
                ch if ch.is_whitespace() => {
                    self.accumulated.push(ch);
                    return self.lex_whitespace();
                }
                ch if ch.is_digit(10) => {
                    self.accumulated.push(ch);
                    return self.lex_number();
                }
                '{' => {
                    return Some((Token::Bracket(Bracket::CurlyOpen), Span::new(i, i + ch.len_utf8())));
                }
                '}' => {
                    return Some((Token::Bracket(Bracket::CurlyClose), Span::new(i, i + ch.len_utf8())));
                }
                '(' => {
                    return Some((Token::Bracket(Bracket::RoundOpen), Span::new(i, i + ch.len_utf8())));
                }
                ')' => {
                    return Some((Token::Bracket(Bracket::RoundClose), Span::new(i, i + ch.len_utf8())));
                }
                '[' => {
                    return Some((Token::Bracket(Bracket::SquareOpen), Span::new(i, i + ch.len_utf8())));
                }
                ']' => {
                    return Some((Token::Bracket(Bracket::SquareClose), Span::new(i, i + ch.len_utf8())));
                }
                ':' => {
                    return Some((
                        Token::SpecialSymbol(SpecialSymbol::Colon),
                        Span::new(i, i + ch.len_utf8()),
                    ));
                }
                '=' => {
                    return Some((
                        Token::SpecialSymbol(SpecialSymbol::Equals),
                        Span::new(i, i + ch.len_utf8()),
                    ));
                }
                ch if ch.is_alphanumeric() || ch == '_' => {
                    self.accumulated.push(ch);
                    return self.lex_alpahanumeric();
                }
                ch => {
                    todo!("Handle unrecognized character: '{}'", ch);
                }
            }
        }

        None
    }

    fn lex_single_comment(&mut self) -> Option<(Token, Span)> {
        while let Some((i, ch)) = self.chars.next() {
            if ch == '\n' {
                let span = Span::new(i - self.accumulated.as_bytes().len(), i);
                let comment_text = take(&mut self.accumulated);
                self.state = State::Default;
                self.lines_table.add_line(i + ch.len_utf8());
                return Some((Token::Comment(Comment::SingleLine(comment_text)), span));
            } else {
                self.accumulated.push(ch);
            }
        }

        None
    }

    fn lex_multiline_comment(&mut self) -> Option<(Token, Span)> {
        while let Some((i, ch)) = self.chars.next() {
            if ch == '*' && self.chars.peek().map(|(_, ch)| ch) == Some(&'/') {
                self.chars.next(); // consume the '/'
                let span = Span::new(i - self.accumulated.as_bytes().len(), i);
                let comment_text = take(&mut self.accumulated);
                self.state = State::Default;
                return Some((Token::Comment(Comment::MultiLine(comment_text)), span));
            } else {
                if ch == '\n' {
                    self.lines_table.add_line(i + ch.len_utf8());
                }
                self.accumulated.push(ch);
            }
        }

        None
    }
}

impl<I: Iterator<Item = (usize, char)>> Iterator for TokenIter<I> {
    type Item = (Token, Span);

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            State::Default => self.lex_default(),
            State::InSingleComment => self.lex_single_comment(),
            State::InMultiComment => self.lex_multiline_comment(),
        }
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
    fn test_basic_code_tokenization() {
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

        let mut token_iter = tokenize(code.char_indices());
        let tokens = token_iter
            .by_ref()
            .map(|(token, _span)| token)
            .collect::<Vec<_>>();

        let expected = [
            Token::Comment(Comment::SingleLine(" Basic inline comment".to_string())),
            Token::NewLine,
            Token::Comment(Comment::MultiLine(" Multiline\ncomment\n".to_string())),
            Token::NewLine,
            Token::Keyword(Keyword::Fn),
            Token::Whitespace(1),
            Token::Identifier("main".to_string()),
            Token::Bracket(Bracket::RoundOpen),
            Token::Bracket(Bracket::RoundClose),
            Token::Whitespace(1),
            Token::Bracket(Bracket::CurlyOpen),
            Token::NewLine,
            Token::Whitespace(4),
            Token::Keyword(Keyword::New),
            Token::Whitespace(1),
            Token::Identifier("x".to_string()),
            Token::SpecialSymbol(SpecialSymbol::Colon),
            Token::Whitespace(1),
            Token::Identifier("i32".to_string()),
            Token::Whitespace(1),
            Token::SpecialSymbol(SpecialSymbol::Equals),
            Token::Whitespace(1),
            Token::Number(Number::Integer("5".to_string())),
            Token::NewLine,
            Token::Whitespace(4),
            Token::Identifier("print".to_string()),
            Token::Bracket(Bracket::RoundOpen),
            Token::Identifier("x".to_string()),
            Token::Bracket(Bracket::RoundClose),
            Token::NewLine,
            Token::Bracket(Bracket::CurlyClose),
        ];

        assert_eq!(tokens, expected);
        assert_eq!(token_iter.lines_table().offsets().len(), 9);
    }

    #[test]
    fn test_numbers_tokenization() {
        let code = r#"
            1_000_000.0
            42
            3.14
        "#;

        let code = prepare_string(code);

        let mut token_iter = tokenize(code.char_indices());
        let tokens = token_iter.by_ref().collect::<Vec<_>>();

        let expected = vec![
            (
                Token::Number(Number::Float("1_000_000.0".to_string())),
                Span { start: 0, end: 11 },
            ),
            (Token::NewLine, Span { start: 11, end: 12 }),
            (
                Token::Number(Number::Integer("42".to_string())),
                Span { start: 12, end: 14 },
            ),
            (Token::NewLine, Span { start: 14, end: 15 }),
            (
                Token::Number(Number::Float("3.14".to_string())),
                Span { start: 14, end: 18 },
            ),
        ];
        
        println!("{:#?}", tokens);

        for (token, span) in tokens.iter() {
            println!("Token: {:?}, by span: {:?}", token, &code[span.start..span.end])
        }
        assert_eq!(tokens, expected);
        assert_eq!(token_iter.lines_table().offsets(), &[0, 12, 15]);
    }
}
