use diagnostic::{MaybeSpanned, Spanned};

use crate::hir::HIRType;
use crate::ids::TypeVarId;

/// For brevity
type OT = Option<S<HIRType>>;
type S<T> = Spanned<T>;
type MaybeS<T> = MaybeSpanned<T>;

/// Type variable (either known type or a variable to be inferred)
#[derive(Clone, Debug)]
pub enum TypeVar {
    Known(MaybeS<HIRType>),
    Var(TypeVarId),
}

/// Type class (for literals)
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum TyClass {
    Int,
    Float,
    // Bool,
    // String,
}

impl TyClass {
    pub fn fallback_type(&self) -> HIRType {
        match self {
            TyClass::Int => HIRType::Primitive(ast::PrimitiveType::I32),
            TyClass::Float => HIRType::Primitive(ast::PrimitiveType::F32),
            // TyClass::Bool => HIRType::Primitive(ast::PrimitiveType::Bool),
            // TyClass::String => HIRType::Primitive(ast::PrimitiveType::String),
        }
    }
}

/// Type variable constraint
#[derive(Clone, Debug)]
pub enum Constraint {
    Eq(S<TypeVar>, S<TypeVar>),
    InClass(S<TypeVar>, TyClass),
}
