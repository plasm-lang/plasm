use tokenizer::{LinesTable, Span, Token, TokenIter};

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    UnexpectedToken {
        token: Token,
        span: Span,
        // lines_table: LinesTable,
        // code: String,
        expected: String,
    },
    UnexpectedEOF {
        expected: String,
    },
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken {
                token,
                span,
                // lines_table,
                // code,
                expected,
            } => {
                todo!()
            }
            ParseError::UnexpectedEOF { expected } => {
                todo!()
            }
        }
    }
}

impl std::error::Error for ParseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}
