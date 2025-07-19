#[derive(Debug, PartialEq, Eq)]
pub enum Keyword {
    Fn,
    Let,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Number(pub String);

#[derive(Debug, PartialEq, Eq)]
pub enum SpecialSymbol {
    Colon,  // :
    Equals, // =
}

#[derive(Debug, PartialEq, Eq)]
pub enum Bracket {
    RoundOpen,   // (
    RoundClose,  // )
    SquareOpen,  // [
    SquareClose, // ]
    CurlyOpen,   // {
    CurlyClose,  // }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Comment {
    SingleLine(String),
    MultiLine(String),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    Number(Number),
    // StringLiteral(String),
    SpecialSymbol(SpecialSymbol),
    Bracket(Bracket),
    Whitespace(u16),
    Comment(Comment),
    NewLine,
    Impossible(String), // For unhandled cases
}

enum State {
    Default,
    InSingleComment,
    InMultiComment,
}

pub struct TokenStream<I: Iterator<Item = char>> {
    chars: std::iter::Peekable<I>,
    state: State,
    accumulated: String,
}

impl<I> TokenStream<I>
where
    I: Iterator<Item = char>,
{
    fn new(iter: I) -> Self {
        Self {
            chars: iter.peekable(),
            state: State::Default,
            accumulated: String::new(),
        }
    }

    fn lex_whitespace(&mut self) -> Option<Token> {
        while let Some(ch) = self.chars.peek() {
            if ch.is_whitespace() {
                self.accumulated.push(*ch);
                self.chars.next(); // consume the whitespace character
            } else {
                break;
            }
        }

        let whitespace_len = self.accumulated.len();
        self.accumulated.clear();
        Some(Token::Whitespace(whitespace_len as u16))
    }

    fn lex_number(&mut self) -> Option<Token> {
        while let Some(ch) = self.chars.peek() {
            if ch.is_digit(10) || *ch == '.' || *ch == '_' {
                self.accumulated.push(*ch);
                self.chars.next(); // consume the digit
            } else {
                break;
            }
        }

        let number = Number(self.accumulated.clone());
        self.accumulated.clear();
        Some(Token::Number(number))
    }

    fn lex_alpahanumeric(&mut self) -> Option<Token> {
        while let Some(ch) = self.chars.peek() {
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
        self.accumulated.clear();
        Some(token)
    }

    fn lex_default(&mut self) -> Option<Token> {
        while let Some(ch) = self.chars.next() {
            match ch {
                '/' if self.chars.peek() == Some(&'/') => {
                    self.state = State::InSingleComment;
                    self.accumulated.clear();
                    self.chars.next(); // consume the second '/'
                    return self.lex_single_comment();
                }
                '/' if self.chars.peek() == Some(&'*') => {
                    self.state = State::InMultiComment;
                    self.accumulated.clear();
                    self.chars.next(); // consume the '*'
                    return self.lex_multi_comment();
                }
                '\n' => {
                    return Some(Token::NewLine);
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
                    return Some(Token::Bracket(Bracket::CurlyOpen));
                }
                '}' => {
                    return Some(Token::Bracket(Bracket::CurlyClose));
                }
                '(' => {
                    return Some(Token::Bracket(Bracket::RoundOpen));
                }
                ')' => {
                    return Some(Token::Bracket(Bracket::RoundClose));
                }
                '[' => {
                    return Some(Token::Bracket(Bracket::SquareOpen));
                }
                ']' => {
                    return Some(Token::Bracket(Bracket::SquareClose));
                }
                ':' => {
                    return Some(Token::SpecialSymbol(SpecialSymbol::Colon));
                }
                '=' => {
                    return Some(Token::SpecialSymbol(SpecialSymbol::Equals));
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

    fn lex_single_comment(&mut self) -> Option<Token> {
        while let Some(ch) = self.chars.next() {
            if ch == '\n' {
                let comment = Comment::SingleLine(self.accumulated.clone());
                self.accumulated.clear();
                self.state = State::Default;
                return Some(Token::Comment(comment));
            } else {
                self.accumulated.push(ch);
            }
        }

        None
    }

    fn lex_multi_comment(&mut self) -> Option<Token> {
        while let Some(ch) = self.chars.next() {
            if ch == '*' && self.chars.peek() == Some(&'/') {
                self.chars.next(); // consume the '/'
                let comment = Comment::MultiLine(self.accumulated.clone());
                self.accumulated.clear();
                self.state = State::Default;
                return Some(Token::Comment(comment));
            } else {
                self.accumulated.push(ch);
            }
        }

        None
    }
}

impl<I> Iterator for TokenStream<I>
where
    I: Iterator<Item = char>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            State::Default => self.lex_default(),
            State::InSingleComment => self.lex_single_comment(),
            State::InMultiComment => self.lex_multi_comment(),
        }
    }
}

pub fn tokenize<I>(input: I) -> TokenStream<I::IntoIter>
where
    I: IntoIterator<Item = char>,
{
    TokenStream::new(input.into_iter())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_code_tokenization() {
        let code = r#"
            // Basic inline comment

            /* Myltiline
            comment
            */
            fn main() {
                let x: i32 = 5
                print(x)
            }
        "#;

        let code = code
            .trim_start()
            .trim_end()
            .lines()
            // remove 12 spaces
            .map(|l| l.trim_start_matches("            "))
            .collect::<Vec<_>>()
            .join("\n");

        let tokens = tokenize(code.chars()).collect::<Vec<_>>();

        let expected = [
            Token::Comment(Comment::SingleLine(" Basic inline comment".to_string())),
            Token::NewLine,
            Token::Comment(Comment::MultiLine(" Myltiline\ncomment\n".to_string())),
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
    }

    #[test]
    fn test_numbers_tokenization() {
        let code = r#"
            1_000_000.0
            42.0
            3.14
        "#;

        let code = code
            .trim_start()
            .trim_end()
            .lines()
            // remove 12 spaces
            .map(|l| l.trim_start_matches("            "))
            .collect::<Vec<_>>()
            .join("\n");

        let tokens = tokenize(code.chars()).collect::<Vec<_>>();

        let expected = [
            Token::Number(Number("1_000_000.0".to_string())),
            Token::NewLine,
            Token::Number(Number("42.0".to_string())),
            Token::NewLine,
            Token::Number(Number("3.14".to_string())),
        ];

        assert_eq!(tokens, expected);
    }
}
