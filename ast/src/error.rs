use std::fmt::{Display, Formatter};

use tokenizer::Token;

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    UnexpectedToken { token: Token, expected: String },
    UnexpectedEOF { expected: String },
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken { token, expected } => {
                write!(f, "Unexpected token `{token}`, expected {expected}")
            }
            ParseError::UnexpectedEOF { expected } => {
                write!(f, "Unexpected end of file, expected {expected}")
            }
        }
    }
}

impl std::error::Error for ParseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}
