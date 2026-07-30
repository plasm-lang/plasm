use std::str::FromStr;

use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum PrimitiveType {
    Void,
    Bool,

    I8,
    I16,
    I32,
    I64,
    I128,
    I256,
    I512,
    I1024,

    U8,
    U16,
    U32,
    U64,
    U128,
    U256,
    U512,
    U1024,

    F16,
    F32,
    F64,
    F128,
}

impl FromStr for PrimitiveType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Void" => Ok(Self::Void),
            "Bool" => Ok(Self::Bool),

            "I8" => Ok(Self::I8),
            "I16" => Ok(Self::I16),
            "I32" => Ok(Self::I32),
            "I64" => Ok(Self::I64),
            "I128" => Ok(Self::I128),
            "I256" => Ok(Self::I256),
            "I512" => Ok(Self::I512),
            "I1024" => Ok(Self::I1024),

            "U8" => Ok(Self::U8),
            "U16" => Ok(Self::U16),
            "U32" => Ok(Self::U32),
            "U64" => Ok(Self::U64),
            "U128" => Ok(Self::U128),
            "U256" => Ok(Self::U256),
            "U512" => Ok(Self::U512),
            "U1024" => Ok(Self::U1024),

            "F16" => Ok(Self::F16),
            "F32" => Ok(Self::F32),
            "F64" => Ok(Self::F64),
            "F128" => Ok(Self::F128),

            _ => Err(()),
        }
    }
}

impl std::fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimitiveType::Void => write!(f, "Void"),
            PrimitiveType::Bool => write!(f, "Bool"),
            PrimitiveType::I8 => write!(f, "I8"),
            PrimitiveType::I16 => write!(f, "I16"),
            PrimitiveType::I32 => write!(f, "I32"),
            PrimitiveType::I64 => write!(f, "I64"),
            PrimitiveType::I128 => write!(f, "I128"),
            PrimitiveType::I256 => write!(f, "I256"),
            PrimitiveType::I512 => write!(f, "I512"),
            PrimitiveType::I1024 => write!(f, "I1024"),
            PrimitiveType::U8 => write!(f, "U8"),
            PrimitiveType::U16 => write!(f, "U16"),
            PrimitiveType::U32 => write!(f, "U32"),
            PrimitiveType::U64 => write!(f, "U64"),
            PrimitiveType::U128 => write!(f, "U128"),
            PrimitiveType::U256 => write!(f, "U256"),
            PrimitiveType::U512 => write!(f, "U512"),
            PrimitiveType::U1024 => write!(f, "U1024"),
            PrimitiveType::F16 => write!(f, "F16"),
            PrimitiveType::F32 => write!(f, "F32"),
            PrimitiveType::F64 => write!(f, "F64"),
            PrimitiveType::F128 => write!(f, "F128"),
        }
    }
}
