use std::iter::Peekable;

use diagnostic::{LinesTable, Span};

use super::token::{Bracket, Comment, Keyword, Number, SpecialSymbol, Token};

pub fn tokenize<I: Iterator<Item = (usize, char)>>(chars: I) -> TokenIter<I> {
    TokenIter {
        chars: chars.peekable(),
        state: State::default(),
        accumulated: String::new(),
        lines_table: LinesTable::new(),
    }
}

enum State {
    Default { number_strategy: NumberStrategy },
    InSingleComment,
    InMultiComment,
}

impl Default for State {
    fn default() -> Self {
        State::Default {
            number_strategy: NumberStrategy::default(),
        }
    }
}

#[derive(Default)]
enum NumberStrategy {
    /// ".1.0" will be tokenized as a [SpecialSymbol::Dot, Number::Float("1.0")]
    #[default]
    FloatAllowed,
    /// ".1.0" will be tokenized as a [SpecialSymbol::Dot, Number::Integer("1"),
    /// SpecialSymbol::Dot, Number::Integer("0")]
    IntegerOnly,
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

    fn lex_whitespace_from(
        &mut self,
        start_i: usize,
        first_ch: char,
    ) -> Option<(Token, Span)> {
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

    fn lex_number_from(
        &mut self,
        start_i: usize,
        first_ch: char,
    ) -> Option<(Token, Span)> {
        match self.state {
            State::Default {
                number_strategy: NumberStrategy::FloatAllowed,
            } => self.lex_number_as_float_allowed_from(start_i, first_ch),
            State::Default {
                number_strategy: NumberStrategy::IntegerOnly,
            } => self.lex_number_as_integer_only_from(start_i, first_ch),
            _ => unreachable!(),
        }
    }

    fn lex_number_as_integer_only_from(
        &mut self,
        start_i: usize,
        first_ch: char,
    ) -> Option<(Token, Span)> {
        self.accumulated.clear();
        self.accumulated.push(first_ch);
        let mut end_i = start_i + first_ch.len_utf8();

        while let Some(&(i, ch)) = self.chars.peek() {
            if ch.is_ascii_digit() || ch == '_' {
                end_i = i + ch.len_utf8();
                self.accumulated.push(ch);
                self.chars.next();
            } else {
                break;
            }
        }

        let span = Span::new(start_i, end_i);
        let number = Number::Integer(self.release_accumulated());
        Some((Token::Number(number), span))
    }

    fn lex_number_as_float_allowed_from(
        &mut self,
        start_i: usize,
        first_ch: char,
    ) -> Option<(Token, Span)> {
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
            Number::Float(self.release_accumulated())
        } else {
            Number::Integer(self.release_accumulated())
        };
        Some((Token::Number(number), span))
    }

    fn release_accumulated(&mut self) -> String {
        std::mem::take(&mut self.accumulated)
    }

    fn lex_alphanumeric_from(
        &mut self,
        start_i: usize,
        first_ch: char,
    ) -> Option<(Token, Span)> {
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
            "return" => Token::Keyword(Keyword::Return),
            "type" => Token::Keyword(Keyword::Type),
            "struct" => Token::Keyword(Keyword::Struct),
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
        let comment_text = self.release_accumulated();
        self.state = State::default();
        Some((Token::Comment(Comment::SingleLine(comment_text)), span))
    }

    fn lex_multiline_comment(&mut self) -> Option<(Token, Span)> {
        self.accumulated.clear();

        let (start_i, ch0) = self.chars.next()?;

        self.accumulated.push(ch0);

        while let Some((i, ch)) = self.chars.next() {
            if ch == '*' && self.chars.peek().map(|(_, ch)| ch) == Some(&'/') {
                self.chars.next(); // consume the '/'
                let span = Span::new(start_i, i);
                let comment_text = self.release_accumulated();
                self.state = State::default();
                return Some((
                    Token::Comment(Comment::MultiLine(comment_text)),
                    span,
                ));
            } else {
                if ch == '\n' {
                    self.lines_table.add_line(i + ch.len_utf8());
                }
                self.accumulated.push(ch);
            }
        }

        None
    }

    fn peek_check(&mut self, expected: char) -> bool {
        self.chars.peek().map(|&(_, ch)| ch) == Some(expected)
    }

    fn lex_default(&mut self) -> Option<(Token, Span)> {
        let (i, ch) = self.chars.next()?;
        let ch_span = || Span::new(i, i + ch.len_utf8());

        // Comments

        if ch == '/' && self.peek_check('/') {
            self.state = State::InSingleComment;
            self.chars.next(); // consume the second '/'
            return self.lex_single_comment();
        }
        if ch == '/' && self.peek_check('*') {
            self.state = State::InMultiComment;
            self.chars.next(); // consume the '*'
            return self.lex_multiline_comment();
        }

        // 2-character symbols

        if let Some(&(_, ch2)) = self.chars.peek()
            && let Some(token) = double_token(ch, ch2)
        {
            let (end_i, ch2) = self.chars.next()?; // consume the second character
            let span = Span::new(i, end_i + ch2.len_utf8());
            return Some((token, span));
        }

        // 1-character symbols and brackets

        if let Some(token) = single_token(ch) {
            return Some((token, ch_span()));
        }

        let res = match ch {
            '\n' => {
                self.lines_table.add_line(i + ch.len_utf8());
                (Token::NewLine, ch_span())
            }
            ch if ch.is_whitespace() => self.lex_whitespace_from(i, ch)?,
            ch if ch.is_ascii_digit() => self.lex_number_from(i, ch)?,
            ch if ch.is_alphanumeric() || ch == '_' => {
                self.lex_alphanumeric_from(i, ch)?
            }
            ch => (Token::Impossible(ch.to_string()), ch_span()),
        };
        Some(res)
    }

    fn reconsider_number_strategy(&mut self, token: Option<&Token>) {
        if let Some(token) = token
            && let Some(strategy) = choose_number_strategy(token)
        {
            self.state = State::Default {
                number_strategy: strategy,
            };
        }
    }
}

impl<I: Iterator<Item = (usize, char)>> Iterator for TokenIter<I> {
    type Item = (Token, Span);

    fn next(&mut self) -> Option<Self::Item> {
        let res = match self.state {
            State::Default { .. } => self.lex_default(),
            State::InSingleComment => self.lex_single_comment(),
            State::InMultiComment => self.lex_multiline_comment(),
        };
        self.reconsider_number_strategy(res.as_ref().map(|(token, _)| token));
        res
    }
}

const fn double_token(first: char, second: char) -> Option<Token> {
    use SpecialSymbol::*;
    let symbol = match (first, second) {
        ('*', '*') => DoubleAsterisk,
        ('&', '&') => DoubleAmpersand,
        ('|', '|') => DoublePipe,
        ('=', '=') => DoubleEquals,
        ('!', '=') => ExclamationEquals,
        ('<', '=') => LessThanEquals,
        ('>', '=') => GreaterThanEquals,
        ('<', '<') => DoubleLessThan,
        ('>', '>') => DoubleGreaterThan,
        _ => return None,
    };
    Some(Token::SpecialSymbol(symbol))
}
const fn single_token(ch: char) -> Option<Token> {
    use Bracket::*;
    use SpecialSymbol::*;
    Some(match ch {
        ':' => Token::SpecialSymbol(Colon),
        '=' => Token::SpecialSymbol(Equals),
        ',' => Token::SpecialSymbol(Comma),
        '>' => Token::SpecialSymbol(GreaterThan),
        '<' => Token::SpecialSymbol(LessThan),
        '-' => Token::SpecialSymbol(Minus),
        '+' => Token::SpecialSymbol(Plus),
        '*' => Token::SpecialSymbol(Asterisk),
        '/' => Token::SpecialSymbol(Slash),
        '%' => Token::SpecialSymbol(Percent),
        '\\' => Token::SpecialSymbol(Backslash),
        '!' => Token::SpecialSymbol(Exclamation),
        '&' => Token::SpecialSymbol(Ampersand),
        '|' => Token::SpecialSymbol(Pipe),
        '^' => Token::SpecialSymbol(Caret),
        '~' => Token::SpecialSymbol(Tilde),
        '.' => Token::SpecialSymbol(Dot),
        '{' => Token::Bracket(CurlyOpen),
        '}' => Token::Bracket(CurlyClose),
        '(' => Token::Bracket(RoundOpen),
        ')' => Token::Bracket(RoundClose),
        '[' => Token::Bracket(SquareOpen),
        ']' => Token::Bracket(SquareClose),
        _ => return None,
    })
}

const fn choose_number_strategy(last_token: &Token) -> Option<NumberStrategy> {
    match last_token {
        // After an identifier, a bracket, or a dot, we consider dots and numbers as
        // an index access (e.g. `some_var.0.123`).
        Token::Identifier(_)
        | Token::Bracket(
            Bracket::RoundClose | Bracket::SquareClose | Bracket::CurlyClose,
        )
        | Token::SpecialSymbol(SpecialSymbol::Dot) => {
            Some(NumberStrategy::IntegerOnly)
        }
        // Otherwise, we allow floats (e.g. `1.`, `0.5`, `3.14`, `(3.14,)`).
        Token::Keyword(_)
        | Token::SpecialSymbol(_)
        | Token::Number(_)
        | Token::Bracket(
            Bracket::RoundOpen | Bracket::SquareOpen | Bracket::CurlyOpen,
        ) => Some(NumberStrategy::FloatAllowed),
        // However sometimes we follow previous token's strategy.
        Token::Whitespace(_)
        | Token::Comment(_)
        | Token::NewLine
        | Token::Impossible(_) => None, // Follow the previous token's strategy
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;

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
            Token::Comment(Comment::SingleLine(
                " Basic inline comment 1".to_string(),
            )),
            Token::NewLine,
            Token::NewLine,
            Token::Comment(Comment::MultiLine(
                " Multiline\ncomment 0123\n".to_string(),
            )),
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
                    Keyword::Return => assert_eq!(str_by_span, "return"),
                    Keyword::Type => assert_eq!(str_by_span, "type"),
                    Keyword::Struct => assert_eq!(str_by_span, "struct"),
                },
                Token::Identifier(id) => assert_eq!(id, str_by_span),
                Token::Number(number) => {
                    assert_eq!(number.raw_value(), str_by_span)
                }
                Token::SpecialSymbol(special_symbol) => match special_symbol {
                    SpecialSymbol::Colon => assert_eq!(str_by_span, ":"),
                    SpecialSymbol::Equals => assert_eq!(str_by_span, "="),
                    SpecialSymbol::Comma => assert_eq!(str_by_span, ","),
                    SpecialSymbol::GreaterThan => assert_eq!(str_by_span, ">"),
                    SpecialSymbol::LessThan => assert_eq!(str_by_span, "<"),
                    SpecialSymbol::Minus => assert_eq!(str_by_span, "-"),
                    SpecialSymbol::Plus => assert_eq!(str_by_span, "+"),
                    SpecialSymbol::Asterisk => assert_eq!(str_by_span, "*"),
                    SpecialSymbol::Slash => assert_eq!(str_by_span, "/"),
                    SpecialSymbol::Percent => assert_eq!(str_by_span, "%"),
                    SpecialSymbol::Backslash => assert_eq!(str_by_span, "\\"),
                    SpecialSymbol::Exclamation => assert_eq!(str_by_span, "!"),
                    SpecialSymbol::Ampersand => assert_eq!(str_by_span, "&"),
                    SpecialSymbol::Pipe => assert_eq!(str_by_span, "|"),
                    SpecialSymbol::Caret => assert_eq!(str_by_span, "^"),
                    SpecialSymbol::DoubleAsterisk => {
                        assert_eq!(str_by_span, "**")
                    }
                    SpecialSymbol::DoubleAmpersand => {
                        assert_eq!(str_by_span, "&&")
                    }
                    SpecialSymbol::DoublePipe => assert_eq!(str_by_span, "||"),
                    SpecialSymbol::DoubleEquals => {
                        assert_eq!(str_by_span, "==")
                    }
                    SpecialSymbol::ExclamationEquals => {
                        assert_eq!(str_by_span, "!=")
                    }
                    SpecialSymbol::GreaterThanEquals => {
                        assert_eq!(str_by_span, ">=")
                    }
                    SpecialSymbol::LessThanEquals => {
                        assert_eq!(str_by_span, "<=")
                    }
                    SpecialSymbol::DoubleLessThan => {
                        assert_eq!(str_by_span, "<<")
                    }
                    SpecialSymbol::DoubleGreaterThan => {
                        assert_eq!(str_by_span, ">>")
                    }
                    SpecialSymbol::Tilde => assert_eq!(str_by_span, "~"),
                    SpecialSymbol::Dot => assert_eq!(str_by_span, "."),
                },
                Token::Bracket(bracket) => match bracket {
                    Bracket::RoundOpen => assert_eq!(str_by_span, "("),
                    Bracket::RoundClose => assert_eq!(str_by_span, ")"),
                    Bracket::SquareOpen => assert_eq!(str_by_span, "["),
                    Bracket::SquareClose => assert_eq!(str_by_span, "]"),
                    Bracket::CurlyOpen => assert_eq!(str_by_span, "{"),
                    Bracket::CurlyClose => assert_eq!(str_by_span, "}"),
                },
                Token::Whitespace(amount) => {
                    assert_eq!(str_by_span.len(), amount)
                }
                Token::Comment(comment) => {
                    assert_eq!(comment.raw_value(), str_by_span)
                }
                Token::NewLine => assert_eq!(str_by_span, "\n"),
                Token::Impossible(value) => {
                    unreachable!(
                        "Impossible token ({value:?}) during test is impossible!"
                    )
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

    #[test]
    fn test_special_symbols_tokenization() {
        let code = ": = , > < - + * / % \\";

        let mut token_iter = tokenize(code.char_indices());
        let tokens = token_iter.by_ref().map(|(t, _s)| t).collect::<Vec<_>>();

        let expected = vec![
            Token::SpecialSymbol(SpecialSymbol::Colon),
            Token::Whitespace(1),
            Token::SpecialSymbol(SpecialSymbol::Equals),
            Token::Whitespace(1),
            Token::SpecialSymbol(SpecialSymbol::Comma),
            Token::Whitespace(1),
            Token::SpecialSymbol(SpecialSymbol::GreaterThan),
            Token::Whitespace(1),
            Token::SpecialSymbol(SpecialSymbol::LessThan),
            Token::Whitespace(1),
            Token::SpecialSymbol(SpecialSymbol::Minus),
            Token::Whitespace(1),
            Token::SpecialSymbol(SpecialSymbol::Plus),
            Token::Whitespace(1),
            Token::SpecialSymbol(SpecialSymbol::Asterisk),
            Token::Whitespace(1),
            Token::SpecialSymbol(SpecialSymbol::Slash),
            Token::Whitespace(1),
            Token::SpecialSymbol(SpecialSymbol::Percent),
            Token::Whitespace(1),
            Token::SpecialSymbol(SpecialSymbol::Backslash),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_number_strategies() {
        let code = indoc! {"
            1.0
            .5
            3.14
            some_var.0.123
            func().456
            "
        };

        let mut token_iter = tokenize(code.char_indices());
        let tokens = token_iter.by_ref().map(|(t, _s)| t).collect::<Vec<_>>();

        let expected = vec![
            Token::Number(Number::Float("1.0".to_string())),
            Token::NewLine,
            Token::SpecialSymbol(SpecialSymbol::Dot),
            Token::Number(Number::Integer("5".to_string())),
            Token::NewLine,
            Token::Number(Number::Float("3.14".to_string())),
            Token::NewLine,
            Token::Identifier("some_var".to_string()),
            Token::SpecialSymbol(SpecialSymbol::Dot),
            Token::Number(Number::Integer("0".to_string())),
            Token::SpecialSymbol(SpecialSymbol::Dot),
            Token::Number(Number::Integer("123".to_string())),
            Token::NewLine,
            Token::Identifier("func".to_string()),
            Token::Bracket(Bracket::RoundOpen),
            Token::Bracket(Bracket::RoundClose),
            Token::SpecialSymbol(SpecialSymbol::Dot),
            Token::Number(Number::Integer("456".to_string())),
            Token::NewLine,
        ];
        assert_eq!(tokens, expected);
    }
}
