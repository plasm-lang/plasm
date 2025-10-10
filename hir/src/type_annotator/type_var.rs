use diagnostic::{MaybeSpanned, Spanned};

use crate::hir::HIRType;
use crate::ids::TypeVarId;

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
            TyClass::Void => HIRType::Primitive(ast::PrimitiveType::Void),
            TyClass::Int => HIRType::Primitive(ast::PrimitiveType::I32),
            TyClass::Float => HIRType::Primitive(ast::PrimitiveType::F32),
            TyClass::Bool => HIRType::Primitive(ast::PrimitiveType::Bool),
            // TyClass::String => HIRType::Primitive(ast::PrimitiveType::String),
        }
    }

    pub fn from_type(ty: &HIRType) -> Option<Self> {
        use ast::PrimitiveType::*;
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
