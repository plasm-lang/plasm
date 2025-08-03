use std::iter::{Enumerate, Peekable};

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

pub fn tokenize<I: Iterator<Item = char>>(chars: I) -> TokenIter<I> {
    TokenIter {
        chars: chars.enumerate().peekable(),
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

pub struct TokenIter<I: Iterator<Item = char>> {
    chars: Peekable<Enumerate<I>>,
    state: State,
    accumulated: String,
    lines_table: LinesTable,
}

impl<I: Iterator<Item = char>> TokenIter<I> {
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
        let span = Span::new(last_i - self.accumulated.len(), last_i);
        self.accumulated.clear();
        Some((Token::Whitespace(whitespace_len as u16), span))
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

        let number = Number(self.accumulated.clone());
        let span = Span::new(last_i - self.accumulated.len(), last_i);
        self.accumulated.clear();
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
            "let" => Token::Keyword(Keyword::Let),
            _ => Token::Identifier(self.accumulated.clone()),
        };
        let span = Span::new(last_i - self.accumulated.len(), last_i);
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
                    return self.lex_multi_comment();
                }
                '\n' => {
                    self.lines_table.add_line(i + 1);
                    return Some((Token::NewLine, Span::new(i - 1, i)));
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
                    return Some((Token::Bracket(Bracket::CurlyOpen), Span::new(i - 1, i)));
                }
                '}' => {
                    return Some((Token::Bracket(Bracket::CurlyClose), Span::new(i - 1, i)));
                }
                '(' => {
                    return Some((Token::Bracket(Bracket::RoundOpen), Span::new(i - 1, i)));
                }
                ')' => {
                    return Some((Token::Bracket(Bracket::RoundClose), Span::new(i - 1, i)));
                }
                '[' => {
                    return Some((Token::Bracket(Bracket::SquareOpen), Span::new(i - 1, i)));
                }
                ']' => {
                    return Some((Token::Bracket(Bracket::SquareClose), Span::new(i - 1, i)));
                }
                ':' => {
                    return Some((
                        Token::SpecialSymbol(SpecialSymbol::Colon),
                        Span::new(i - 1, i),
                    ));
                }
                '=' => {
                    return Some((
                        Token::SpecialSymbol(SpecialSymbol::Equals),
                        Span::new(i - 1, i),
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
                let comment = Comment::SingleLine(self.accumulated.clone());
                let span = Span::new(i - self.accumulated.len() - 2, i - 1);
                self.accumulated.clear();
                self.state = State::Default;
                self.lines_table.add_line(i + 1);
                return Some((Token::Comment(comment), span));
            } else {
                self.accumulated.push(ch);
            }
        }

        None
    }

    fn lex_multi_comment(&mut self) -> Option<(Token, Span)> {
        while let Some((i, ch)) = self.chars.next() {
            if ch == '*' && self.chars.peek().map(|(_, ch)| ch) == Some(&'/') {
                self.chars.next(); // consume the '/'
                let comment = Comment::MultiLine(self.accumulated.clone());
                let span = Span::new(i - self.accumulated.len() - 2, i + 1);
                self.accumulated.clear();
                self.state = State::Default;
                return Some((Token::Comment(comment), span));
            } else {
                if ch == '\n' {
                    self.lines_table.add_line(i + 1);
                }
                self.accumulated.push(ch);
            }
        }

        None
    }
}

impl<I: Iterator<Item = char>> Iterator for TokenIter<I> {
    type Item = (Token, Span);

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            State::Default => self.lex_default(),
            State::InSingleComment => self.lex_single_comment(),
            State::InMultiComment => self.lex_multi_comment(),
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
                let x: i32 = 5
                print(x)
            }
        "#;

        let code = prepare_string(code);

        let mut token_iter = tokenize(code.chars());
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
            Token::Keyword(Keyword::Let),
            Token::Whitespace(1),
            Token::Identifier("x".to_string()),
            Token::SpecialSymbol(SpecialSymbol::Colon),
            Token::Whitespace(1),
            Token::Identifier("i32".to_string()),
            Token::Whitespace(1),
            Token::SpecialSymbol(SpecialSymbol::Equals),
            Token::Whitespace(1),
            Token::Number(Number("5".to_string())),
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
            42.0
            3.14
        "#;

        let code = prepare_string(code);

        let mut token_iter = tokenize(code.chars());
        let tokens = token_iter.by_ref().collect::<Vec<_>>();

        let expected = vec![
            (
                Token::Number(Number("1_000_000.0".to_string())),
                Span { start: 0, end: 11 },
            ),
            (Token::NewLine, Span { start: 10, end: 11 }),
            (
                Token::Number(Number("42.0".to_string())),
                Span { start: 12, end: 16 },
            ),
            (Token::NewLine, Span { start: 15, end: 16 }),
            (
                Token::Number(Number("3.14".to_string())),
                Span { start: 16, end: 20 },
            ),
        ];

        assert_eq!(tokens, expected);
        assert_eq!(token_iter.lines_table().offsets(), &[0, 12, 17]);
    }
}
