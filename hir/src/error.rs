use std::fmt::{Display, Formatter};

use diagnostic::{ErrorType, Spanned};
use strum_macros::IntoStaticStr;

use super::type_inference::TypeInferenceError;

#[derive(Debug, IntoStaticStr)]
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
    ArgumentCountMismatch {
        found: usize,
        expected: usize,
    },
    TypeInferenceError(TypeInferenceError),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Error::*;

        match self {
            FunctionMultipleDefinitions { first, second } => {
                write!(
                    f,
                    "Function `{}` is defined multiple times: first at {}, then at \
                     {}",
                    first.node, first.span, second.span
                )
            }
            UnknownVariable { name } => {
                write!(f, "Unknown variable `{name}`",)
            }
            UnknownFunction { name } => {
                write!(f, "Unknown function `{name}`")
            }
            ArgumentCountMismatch { found, expected } => {
                write!(
                    f,
                    "Function call argument count mismatch: found {found}, \
                     expected {expected}"
                )
            }
            TypeInferenceError(type_inference_error) => {
                write!(f, "{type_inference_error}")
            }
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        use Error::*;
        match self {
            TypeInferenceError(type_inference_error) => Some(type_inference_error),
            _ => None,
        }
    }
}

impl ErrorType for Error {
    fn error_type(&self) -> &'static str {
        use Error::*;

        const SEMANTIC_ERROR: &str = "SemanticError";

        match self {
            FunctionMultipleDefinitions { .. }
            | UnknownVariable { .. }
            | UnknownFunction { .. }
            | ArgumentCountMismatch { .. } => SEMANTIC_ERROR,

            TypeInferenceError(error) => error.error_type(),
        }
    }

    fn error_sub_type(&self) -> &'static str {
        use Error::*;

        match self {
            TypeInferenceError(error) => error.error_sub_type(),
            _ => self.into(),
        }
    }
}
