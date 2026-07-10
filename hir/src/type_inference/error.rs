use std::fmt::{Display, Formatter};

use diagnostic::{ErrorType, MaybeSpanned};
use strum_macros::IntoStaticStr;

use super::type_class::TypeClass;
use crate::error::Error;
use crate::types::HIRType;

#[derive(Debug, IntoStaticStr)]
pub enum TypeInferenceError {
    TypesConflict {
        first: MaybeSpanned<HIRType>,
        second: MaybeSpanned<HIRType>,
    },
    IncompatibleTypeClass {
        ty: HIRType,
        class: TypeClass,
    },
    CantResolveType,
    UnknownStructField {
        struct_name: String,
        field_name: String,
    },
    MissingStructField {
        struct_name: String,
        field_name: String,
    },
    UnknownTypeName {
        name: String,
    },
    CircularTypeDefinition {
        cycle: Vec<String>,
    },
    ShapeOnNonStructType {
        known: MaybeSpanned<HIRType>,
    },
}

impl Display for TypeInferenceError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use TypeInferenceError::*;
        match self {
            TypesConflict { first, second } => {
                // let first_at = first.span.map(|s| format!(" ({s}
                // bytes)")).unwrap_or_default(); let second_at =
                // second.span.map(|s| format!(" ({s} bytes)")).unwrap_or_default();
                write!(
                    f,
                    "Types conflict between `{}` and `{}`",
                    first.node, second.node,
                )
            }
            IncompatibleTypeClass { ty, class } => {
                write!(f, "Type `{ty}` is not compatible with type class {class:?}")
            }
            CantResolveType => {
                write!(f, "Cannot resolve type")
            }
            UnknownStructField {
                struct_name,
                field_name,
            } => {
                write!(
                    f,
                    "Struct `{struct_name}` doesn't have field `{field_name}`"
                )
            }
            MissingStructField {
                struct_name,
                field_name,
            } => {
                write!(f, "Missing struct field `{field_name}` for `{struct_name}`")
            }
            UnknownTypeName { name } => {
                write!(f, "Unknown type name `{name}`")
            }
            CircularTypeDefinition { cycle } => {
                let names = cycle
                    .iter()
                    .map(|n| format!("`{n}`"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "Circular type definition involving types {names}")
            }
            ShapeOnNonStructType { known } => {
                write!(
                    f,
                    "Expected a struct type for struct literal, but found `{}`",
                    known.node
                )
            }
        }
    }
}

impl std::error::Error for TypeInferenceError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl ErrorType for TypeInferenceError {
    fn error_type(&self) -> &'static str {
        use TypeInferenceError::*;

        const TYPE_ERROR: &str = "TypeError";

        match self {
            TypesConflict { .. }
            | IncompatibleTypeClass { .. }
            | CantResolveType
            | UnknownStructField { .. }
            | MissingStructField { .. }
            | UnknownTypeName { .. }
            | CircularTypeDefinition { .. }
            | ShapeOnNonStructType { .. } => TYPE_ERROR,
        }
    }

    fn error_sub_type(&self) -> &'static str {
        self.into()
    }
}

impl From<TypeInferenceError> for Error {
    fn from(val: TypeInferenceError) -> Self {
        Error::TypeInferenceError(val)
    }
}
