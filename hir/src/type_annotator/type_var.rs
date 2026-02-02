use diagnostic::{MaybeSpanned, Spanned};
use utils::ids::TypeVarId;
use utils::primitive_types::PrimitiveType;

use crate::hir::HIRType;

// For brevity
type S<T> = Spanned<T>;

/// Type variable (either known type or a variable to be inferred)
#[derive(Clone, Debug)]
pub enum TypeVar {
    Known(MaybeSpanned<HIRType>),
    Var(TypeVarId),
}

/// Type class (for literals)
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum TyClass {
    Void,
    Int,
    Float,
    Bool,
    // String,
}

impl TyClass {
    pub fn fallback_type(&self) -> HIRType {
        match self {
            TyClass::Void => HIRType::Primitive(PrimitiveType::Void),
            TyClass::Int => HIRType::Primitive(PrimitiveType::I32),
            TyClass::Float => HIRType::Primitive(PrimitiveType::F32),
            TyClass::Bool => HIRType::Primitive(PrimitiveType::Bool),
            // TyClass::String => HIRType::Primitive(PrimitiveType::String),
        }
    }

    pub fn from_type(ty: &HIRType) -> Option<Self> {
        use PrimitiveType::*;
        match ty {
            HIRType::Primitive(p) => match p {
                Void => Some(TyClass::Void),
                Bool => Some(TyClass::Bool),
                I8 | I16 | I32 | I64 | I128 | I256 | I512 | I1024 | U8 | U16 | U32 | U64 | U128
                | U256 | U512 | U1024 => Some(TyClass::Int),
                F8 | F16 | F32 | F64 | F128 | F256 | F512 | F1024 => Some(TyClass::Float),
            },
        }
    }
}

/// Type variable constraint
#[derive(Clone, Debug)]
pub enum Constraint {
    Eq(S<TypeVar>, S<TypeVar>),
    InClass(S<TypeVar>, TyClass),
}
