use std::iter::Peekable;
use std::mem::take;

use super::token::{Bracket, Comment, Keyword, Number, SpecialSymbol, Token};
use diagnostic::{LinesTable, Span};

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
    pub fn lines_table_ref(&self) -> &LinesTable {
        &self.lines_table
    }

    fn lex_whitespace_from(&mut self, start_i: usize, first_ch: char) -> Option<(Token, Span)> {
        self.accumulated.clear();
        self.accumulated.push(first_ch);
        let mut end_i = start_i + first_ch.len_utf8();

        while let Some(&(i, ch)) = self.chars.peek() {
            if ch.is_whitespace() {
                end_i = i + ch.len_utf8();
                self.accumulated.push(ch);
                self.chars.next();
            } else {
                break;
            }
        }

        let span = Span::new(start_i, end_i);
        let whitespace_len = self.accumulated.chars().count();
        self.accumulated.clear();
        Some((Token::Whitespace(whitespace_len), span))
    }

    fn lex_number_from(&mut self, start_i: usize, first_ch: char) -> Option<(Token, Span)> {
        self.accumulated.clear();
        self.accumulated.push(first_ch);
        let mut end_i = start_i + first_ch.len_utf8();

        while let Some(&(i, ch)) = self.chars.peek() {
            if ch.is_ascii_digit() || ch == '.' || ch == '_' {
                end_i = i + ch.len_utf8();
                self.accumulated.push(ch);
                self.chars.next();
            } else {
                break;
            }
        }

        let span = Span::new(start_i, end_i);
        let number = if self.accumulated.contains('.') {
            Number::Float(std::mem::take(&mut self.accumulated))
        } else {
            Number::Integer(std::mem::take(&mut self.accumulated))
        };
        Some((Token::Number(number), span))
    }

    fn lex_alphanumeric_from(&mut self, start_i: usize, first_ch: char) -> Option<(Token, Span)> {
        self.accumulated.clear();
        self.accumulated.push(first_ch);
        let mut end_i = start_i + first_ch.len_utf8();

        while let Some(&(i, ch)) = self.chars.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                end_i = i + ch.len_utf8();
                self.accumulated.push(ch);
                self.chars.next();
            } else {
                break;
            }
        }

        let token = match self.accumulated.as_str() {
            "fn" => Token::Keyword(Keyword::Fn),
            "let" => Token::Keyword(Keyword::Let),
            _ => Token::Identifier(self.accumulated.clone()),
        };
        let span = Span::new(start_i, end_i);
        self.accumulated.clear();
        Some((token, span))
    }

    fn lex_single_comment(&mut self) -> Option<(Token, Span)> {
        self.accumulated.clear();

        let &(start_i, ch0) = self.chars.peek()?;

        let mut end_i = start_i + ch0.len_utf8();
        while let Some(&(i, ch)) = self.chars.peek() {
            if ch == '\n' {
                break;
            } else {
                end_i = i + ch.len_utf8();
                let (_, ch) = self.chars.next()?;
                self.accumulated.push(ch);
            }
        }

        let span = Span::new(start_i, end_i);
        let comment_text = take(&mut self.accumulated);
        self.state = State::Default;
        return Some((Token::Comment(Comment::SingleLine(comment_text)), span));
    }

    fn lex_multiline_comment(&mut self) -> Option<(Token, Span)> {
        self.accumulated.clear();

        let (start_i, ch0) = self.chars.next()?;

        self.accumulated.push(ch0);

        while let Some((i, ch)) = self.chars.next() {
            if ch == '*' && self.chars.peek().map(|(_, ch)| ch) == Some(&'/') {
                self.chars.next(); // consume the '/'
                let span = Span::new(start_i, i);
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

    fn lex_default(&mut self) -> Option<(Token, Span)> {
        let (i, ch) = self.chars.next()?;
        match ch {
            '/' if self.chars.peek().map(|(_, ch)| ch) == Some(&'/') => {
                self.state = State::InSingleComment;
                self.chars.next(); // consume the second '/'
                self.lex_single_comment()
            }
            '/' if self.chars.peek().map(|(_, ch)| ch) == Some(&'*') => {
                self.state = State::InMultiComment;
                self.chars.next(); // consume the '*'
                self.lex_multiline_comment()
            }
            '\n' => {
                let char_len = ch.len_utf8();
                self.lines_table.add_line(i + char_len);
                Some((Token::NewLine, Span::new(i, i + char_len)))
            }
            ch if ch.is_whitespace() => self.lex_whitespace_from(i, ch),
            ch if ch.is_ascii_digit() => self.lex_number_from(i, ch),
            '{' => Some((
                Token::Bracket(Bracket::CurlyOpen),
                Span::new(i, i + ch.len_utf8()),
            )),
            '}' => Some((
                Token::Bracket(Bracket::CurlyClose),
                Span::new(i, i + ch.len_utf8()),
            )),
            '(' => Some((
                Token::Bracket(Bracket::RoundOpen),
                Span::new(i, i + ch.len_utf8()),
            )),
            ')' => Some((
                Token::Bracket(Bracket::RoundClose),
                Span::new(i, i + ch.len_utf8()),
            )),
            '[' => Some((
                Token::Bracket(Bracket::SquareOpen),
                Span::new(i, i + ch.len_utf8()),
            )),
            ']' => Some((
                Token::Bracket(Bracket::SquareClose),
                Span::new(i, i + ch.len_utf8()),
            )),
            ':' => Some((
                Token::SpecialSymbol(SpecialSymbol::Colon),
                Span::new(i, i + ch.len_utf8()),
            )),
            '=' => Some((
                Token::SpecialSymbol(SpecialSymbol::Equals),
                Span::new(i, i + ch.len_utf8()),
            )),
            ',' => Some((
                Token::SpecialSymbol(SpecialSymbol::Comma),
                Span::new(i, i + ch.len_utf8()),
            )),
            ch if ch.is_alphanumeric() || ch == '_' => self.lex_alphanumeric_from(i, ch),
            ch => Some((
                Token::Impossible(ch.to_string()),
                Span::new(i, i + ch.len_utf8()),
            )),
        }
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
    use indoc::indoc;

    const BASIC_CODE: &str = indoc! {"
        // Basic inline comment 1

        /* Multiline
        comment 0123
        */
        fn main() {
            let x: i32 = 5
            print(x)
        }"};

    #[test]
    fn test_basic_code_tokenization() {
        let mut token_iter = tokenize(BASIC_CODE.char_indices());
        let tokens = token_iter
            .by_ref()
            .map(|(token, _span)| token)
            .collect::<Vec<_>>();

        let expected = [
            Token::Comment(Comment::SingleLine(" Basic inline comment 1".to_string())),
            Token::NewLine,
            Token::NewLine,
            Token::Comment(Comment::MultiLine(" Multiline\ncomment 0123\n".to_string())),
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
        assert_eq!(token_iter.lines_table_ref().offsets().len(), 9);
    }

    #[test]
    fn test_spans() {
        let token_iter = tokenize(BASIC_CODE.char_indices());
        for (token, span) in token_iter {
            let str_by_span = &BASIC_CODE[span.start..span.end];
            match token {
                Token::Keyword(keyword) => match keyword {
                    Keyword::Fn => assert_eq!(str_by_span, "fn"),
                    Keyword::Let => assert_eq!(str_by_span, "let"),
                },
                Token::Identifier(id) => assert_eq!(id, str_by_span),
                Token::Number(number) => assert_eq!(number.raw_value(), str_by_span),
                Token::SpecialSymbol(special_symbol) => match special_symbol {
                    SpecialSymbol::Colon => assert_eq!(str_by_span, ":"),
                    SpecialSymbol::Equals => assert_eq!(str_by_span, "="),
                    SpecialSymbol::Comma => assert_eq!(str_by_span, ","),
                },
                Token::Bracket(bracket) => match bracket {
                    Bracket::RoundOpen => assert_eq!(str_by_span, "("),
                    Bracket::RoundClose => assert_eq!(str_by_span, ")"),
                    Bracket::SquareOpen => assert_eq!(str_by_span, "["),
                    Bracket::SquareClose => assert_eq!(str_by_span, "]"),
                    Bracket::CurlyOpen => assert_eq!(str_by_span, "{"),
                    Bracket::CurlyClose => assert_eq!(str_by_span, "}"),
                },
                Token::Whitespace(amount) => assert_eq!(str_by_span.len(), amount),
                Token::Comment(comment) => assert_eq!(comment.raw_value(), str_by_span),
                Token::NewLine => assert_eq!(str_by_span, "\n"),
                Token::Impossible(value) => {
                    unreachable!("Impossible token ({value:?}) during test is impossible!")
                }
            }
        }
    }

    #[test]
    fn test_lines_table() {
        let mut token_iter = tokenize(BASIC_CODE.char_indices());
        let _ = token_iter.by_ref().collect::<Vec<_>>();
        let mut offsets_iter = token_iter.lines_table_ref().offsets().iter();
        assert_eq!(offsets_iter.next(), Some(0).as_ref());
        for &i in offsets_iter {
            assert_eq!(&BASIC_CODE[i - 1..i], "\n");
        }
    }

    #[test]
    fn test_numbers_tokenization() {
        let code = indoc! {"
            1_000_000.0
            42
            3.14
            1"};

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
                Span { start: 15, end: 19 },
            ),
            (Token::NewLine, Span { start: 19, end: 20 }),
            (
                Token::Number(Number::Integer("1".to_string())),
                Span { start: 20, end: 21 },
            ),
        ];

        assert_eq!(tokens, expected);
        assert_eq!(token_iter.lines_table_ref().offsets(), &[0, 12, 15, 20]);
    }
}
