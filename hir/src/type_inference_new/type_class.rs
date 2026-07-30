use utils::primitive_types::PrimitiveType;

use crate::types::HIRType;

/// Type class (for literals)
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TypeClass {
    Void,
    Int,
    Float,
    Bool,
    // String,
}

impl TypeClass {
    pub fn fallback_type(&self) -> HIRType {
        match self {
            TypeClass::Void => HIRType::Primitive(PrimitiveType::Void),
            TypeClass::Int => HIRType::Primitive(PrimitiveType::I32),
            TypeClass::Float => HIRType::Primitive(PrimitiveType::F32),
            TypeClass::Bool => HIRType::Primitive(PrimitiveType::Bool),
        }
    }

    pub fn from_type(ty: &HIRType) -> Option<Self> {
        use PrimitiveType::*;
        match ty.peel_named() {
            HIRType::Primitive(Void) => Some(TypeClass::Void),
            HIRType::Primitive(Bool) => Some(TypeClass::Bool),
            HIRType::Primitive(
                I8 | I16 | I32 | I64 | I128 | I256 | I512 | I1024 | U8 | U16 | U32
                | U64 | U128 | U256 | U512 | U1024,
            ) => Some(TypeClass::Int),
            HIRType::Primitive(
                F8 | F16 | F32 | F64 | F128 | F256 | F512 | F1024,
            ) => Some(TypeClass::Float),
            _ => None,
        }
    }

    pub fn is_compatible_with(&self, ty: &HIRType) -> bool {
        match Self::from_type(ty) {
            Some(class) => class == *self,
            None => false,
        }
    }
}
