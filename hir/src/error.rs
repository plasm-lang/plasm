use std::fmt::{Display, Formatter};

use diagnostic::{ErrorType, MaybeSpanned, Spanned};
use utils::ids::{ExprId, LocalId};

use super::hir::HIRType;
use super::hir_display::RenderType;
use super::type_annotator::TyClass;

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
    ArgumentCountMismatch {
        found: usize,
        expected: usize,
    },
    TypesConflict {
        first: MaybeSpanned<HIRType>,
        second: MaybeSpanned<HIRType>,
    },
    AmbiguousClass {
        possible_classes: Vec<TyClass>,
    },
    CantResolveType,
    UnregisteredLocalId {
        id: LocalId,
    },
    UnregisteredExprId {
        id: ExprId,
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
                    first.node.render_ty(),
                    second.node.render_ty(),
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
            Error::FunctionMultipleDefinitions { .. } => SEMANTIC_ERROR,
            Error::UnknownVariable { .. } => SEMANTIC_ERROR,
            Error::UnknownFunction { .. } => SEMANTIC_ERROR,
            Error::ArgumentCountMismatch { .. } => SEMANTIC_ERROR,

            Error::TypesConflict { .. } => TYPE_ERROR,
            Error::AmbiguousClass { .. } => TYPE_ERROR,
            Error::CantResolveType => TYPE_ERROR,

            Error::UnregisteredLocalId { .. } => INTERNAL_ERROR,
            Error::UnregisteredExprId { .. } => INTERNAL_ERROR,
        }
    }

    fn error_sub_type(&self) -> &'static str {
        match self {
            Error::FunctionMultipleDefinitions { .. } => "FunctionMultipleDefinitions",
            Error::UnknownVariable { .. } => "UnknownVariable",
            Error::UnknownFunction { .. } => "UnknownFunction",
            Error::ArgumentCountMismatch { .. } => "ArgumentCountMismatch",

            Error::TypesConflict { .. } => "TypesConflict",
            Error::AmbiguousClass { .. } => "AmbiguousClass",
            Error::CantResolveType => "CantResolveType",

            Error::UnregisteredLocalId { .. } => "UnregisteredLocalId",
            Error::UnregisteredExprId { .. } => "UnregisteredExprId",
        }
    }
}
