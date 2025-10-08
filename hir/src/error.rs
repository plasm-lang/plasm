use std::fmt::{Display, Formatter};

use diagnostic::Spanned;

#[derive(Debug)]
pub enum Error {
    FunctionMultipleDefinitions {
        first: Spanned<String>,
        second: Spanned<String>,
    },
    UnknownVariable {
        name: String,
    },
    UnknownFunction {
        name: String,
    },
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::FunctionMultipleDefinitions { first, second } => {
                write!(
                    f,
                    "Function `{}` is defined multiple times: first at {}, then at {}",
                    first.node, first.span, second.span
                )
            }
            Error::UnknownVariable { name } => {
                write!(f, "Unknown variable `{}`", name)
            }
            Error::UnknownFunction { name } => {
                write!(f, "Unknown function `{}`", name)
            }
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}
