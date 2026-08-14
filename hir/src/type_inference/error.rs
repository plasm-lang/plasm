use std::fmt::{Display, Formatter};

use diagnostic::ErrorType;
use strum_macros::IntoStaticStr;

use super::type_class::TypeClass;
use crate::error::Error;

#[derive(Debug, IntoStaticStr)]
pub enum TypeInferenceError {
    TypesConflict {
        first: String,
        second: String,
    },
    UnknownTypeName {
        name: String,
    },
    CircularTypeDefinition {
        cycle: Vec<String>,
    },
    CantResolveType,

    // Constraint-related errors below
    IncompatibleTypeClass {
        ty: String,
        class: TypeClass,
    },
    MultipleTypeClasses {
        classes: Vec<TypeClass>,
    },
    UnknownStructField {
        struct_type: String,
        field_name: String,
    },
    ImpossibleTupleIndex {
        tuple_type: String,
        index: usize,
    },
    MissingStructField {
        struct_type: String,
        field_name: String,
    },
    TupleShapeLengthMismatch {
        ty: String,
        expected: usize,
        actual: usize,
    },
    ShapeOnNonStructType {
        ty: String,
    },
    ShapeOnNonTupleType {
        ty: String,
    },
    FieldOnNonStructType {
        ty: String,
        field_name: String,
    },
    IndexOnNonTupleType {
        ty: String,
        index: usize,
    },
    FieldOnUnknownType {
        field_name: String,
    },
    IndexOnUnknownType {
        index: usize,
    },
}

impl Display for TypeInferenceError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use TypeInferenceError::*;
        match self {
            TypesConflict { first, second } => {
                write!(f, "Types conflict between `{}` and `{}`.", first, second,)
            }
            IncompatibleTypeClass { ty, class } => {
                write!(
                    f,
                    "Type `{ty}` is not compatible with {class:?} type class."
                )
            }
            MultipleTypeClasses { classes } => {
                let class_list = classes
                    .iter()
                    .map(|c| format!("{c:?}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "Multiple type classes: {class_list}.",)
            }
            CantResolveType => {
                write!(f, "Cannot resolve type.")
            }
            UnknownStructField {
                struct_type,
                field_name,
            } => {
                write!(
                    f,
                    "Struct `{struct_type}` doesn't have field `{field_name}`."
                )
            }
            ImpossibleTupleIndex { tuple_type, index } => {
                write!(
                    f,
                    "Tuple `{tuple_type}` doesn't have an element at index {index}."
                )
            }
            MissingStructField {
                struct_type,
                field_name,
            } => {
                write!(
                    f,
                    "Missing struct field `{field_name}` for `{struct_type}`."
                )
            }
            TupleShapeLengthMismatch {
                ty,
                expected,
                actual,
            } => {
                write!(
                    f,
                    "Tuple length mismatch for `{ty}`: expected {expected}, actual {actual}."
                )
            }
            UnknownTypeName { name } => {
                write!(f, "Unknown type name `{name}`.")
            }
            CircularTypeDefinition { cycle } => {
                let names = cycle
                    .iter()
                    .map(|n| format!("`{n}`"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "Circular type definition involving types {names}.")
            }
            ShapeOnNonStructType { ty } => {
                write!(
                    f,
                    "Expected a struct type for a struct literal, but found `{ty}`.",
                )
            }
            ShapeOnNonTupleType { ty } => {
                write!(
                    f,
                    "Expected a tuple type for a tuple literal, but found `{ty}`.",
                )
            }
            FieldOnNonStructType { ty, field_name } => {
                write!(
                    f,
                    "Type `{ty}` is not a struct, so it cannot have a field `{field_name}`."
                )
            }
            IndexOnNonTupleType { ty, index } => {
                write!(
                    f,
                    "Type `{ty}` is not a tuple, so it cannot be indexed by `.{index}`."
                )
            }
            FieldOnUnknownType { field_name } => {
                write!(
                    f,
                    "Cannot determine the type of field `{field_name}` because the base type is unknown."
                )
            }
            IndexOnUnknownType { index } => {
                write!(
                    f,
                    "Cannot determine the type of element by index `{index}` because the base type is unknown."
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
        "TypeError"
    }

    fn error_sub_type(&self) -> &'static str {
        self.into()
    }
}

impl From<TypeInferenceError> for Error {
    fn from(error: TypeInferenceError) -> Self {
        Error::TypeInferenceError(error)
    }
}
