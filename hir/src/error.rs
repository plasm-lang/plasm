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
        todo!()
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}
