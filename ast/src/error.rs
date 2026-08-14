use std::fmt::{Display, Formatter};

use diagnostic::ErrorType;
use strum_macros::IntoStaticStr;
use tokenizer::Token;

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, PartialEq, Eq, IntoStaticStr)]
pub enum ParseError {
    UnexpectedToken { token: Token, expected: String },
    UnexpectedEOF { expected: String },
    InvalidLeftValue,
    InvalidIndexAccess { index: String },
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken { token, expected } => {
                write!(f, "Unexpected token `{token}`, expected {expected}")
            }
            ParseError::UnexpectedEOF { expected } => {
                write!(f, "Unexpected end of the file, expected {expected}")
            }
            ParseError::InvalidLeftValue => {
                write!(f, "Invalid left value for assignment")
            }
            ParseError::InvalidIndexAccess { index } => {
                write!(f, "Invalid access by `{index}` index")
            }
        }
    }
}

impl std::error::Error for ParseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl ErrorType for ParseError {
    fn error_type(&self) -> &'static str {
        "ParseError"
    }

    fn error_sub_type(&self) -> &'static str {
        self.into()
    }
}
