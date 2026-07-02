use std::fmt::{Display, Formatter};

use strum_macros::IntoStaticStr;

use diagnostic::{ErrorType, MaybeSpanned, Spanned};
use utils::ids::{ExprId, LocalId};

use super::type_inference::TypeClass;
use super::types::HIRType;

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
    TypesConflict {
        first: MaybeSpanned<HIRType>,
        second: MaybeSpanned<HIRType>,
    },
    AmbiguousClass {
        possible_classes: Vec<TypeClass>,
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
    UnregisteredLocalId {
        id: LocalId,
    },
    UnregisteredExprId {
        id: ExprId,
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
                write!(f, "Unknown variable `{name}`",)
            }
            Error::UnknownFunction { name } => {
                write!(f, "Unknown function `{name}`")
            }
            Error::ArgumentCountMismatch { found, expected } => {
                write!(
                    f,
                    "Function call argument count mismatch: found {found}, expected {expected}"
                )
            }
            Error::TypesConflict { first, second } => {
                // let first_at = first.span.map(|s| format!(" ({s} bytes)")).unwrap_or_default();
                // let second_at = second.span.map(|s| format!(" ({s} bytes)")).unwrap_or_default();
                write!(
                    f,
                    "Types conflict between `{}` and `{}`",
                    first.node, second.node,
                )
            }
            Error::AmbiguousClass { possible_classes } => {
                let classes = possible_classes
                    .iter()
                    .map(|c| format!("{c:?}"))
                    .collect::<Vec<_>>()
                    .join(" or ");
                write!(
                    f,
                    "Cannot resolve type because literal is ambiguous - may be {classes}"
                )
            }
            Error::CantResolveType => {
                write!(f, "Cannot resolve type")
            }
            Error::UnknownStructField {
                struct_name,
                field_name,
            } => {
                write!(
                    f,
                    "Struct `{struct_name}` doesn't have field `{field_name}`"
                )
            }
            Error::MissingStructField {
                struct_name,
                field_name,
            } => {
                write!(f, "Missing struct field `{field_name}` for `{struct_name}`")
            }
            Error::UnregisteredLocalId { id } => {
                write!(
                    f,
                    "Unregistered local id {id}. It's a compiler issue, please report it"
                )
            }
            Error::UnregisteredExprId { id } => {
                write!(
                    f,
                    "Unregistered expression id {id}. It's a compiler issue, please report it"
                )
            }
            Error::UnknownTypeName { name } => {
                write!(f, "Unknown type name `{name}`")
            }
            Error::CircularTypeDefinition { cycle } => {
                let names = cycle
                    .iter()
                    .map(|n| format!("`{n}`"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "Circular type definition involving types {names}")
            }
            Error::ShapeOnNonStructType { known } => {
                write!(
                    f,
                    "Expected a struct type for struct literal, but found `{}`",
                    known.node
                )
            }
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl ErrorType for Error {
    fn error_type(&self) -> &'static str {
        const SEMANTIC_ERROR: &str = "SemanticError";
        const TYPE_ERROR: &str = "TypeError";
        const INTERNAL_ERROR: &str = "InternalError";

        match self {
            Error::FunctionMultipleDefinitions { .. }
            | Error::UnknownVariable { .. }
            | Error::UnknownFunction { .. }
            | Error::ArgumentCountMismatch { .. } => SEMANTIC_ERROR,

            Error::TypesConflict { .. }
            | Error::AmbiguousClass { .. }
            | Error::CantResolveType
            | Error::UnknownStructField { .. }
            | Error::MissingStructField { .. }
            | Error::UnknownTypeName { .. }
            | Error::CircularTypeDefinition { .. }
            | Error::ShapeOnNonStructType { .. } => TYPE_ERROR,

            Error::UnregisteredLocalId { .. } | Error::UnregisteredExprId { .. } => {
                INTERNAL_ERROR
            }
        }
    }

    fn error_sub_type(&self) -> &'static str {
        self.into()
    }
}
