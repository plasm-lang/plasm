use std::str::FromStr;

use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
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

    F8,
    F16,
    F32,
    F64,
    F128,
    F256,
    F512,
    F1024,
}

impl FromStr for PrimitiveType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "void" => Ok(Self::Void),
            "bool" => Ok(Self::Bool),

            "i8" => Ok(Self::I8),
            "i16" => Ok(Self::I16),
            "i32" => Ok(Self::I32),
            "i64" => Ok(Self::I64),
            "i128" => Ok(Self::I128),
            "i256" => Ok(Self::I256),
            "i512" => Ok(Self::I512),
            "i1024" => Ok(Self::I1024),

            "u8" => Ok(Self::U8),
            "u16" => Ok(Self::U16),
            "u32" => Ok(Self::U32),
            "u64" => Ok(Self::U64),
            "u128" => Ok(Self::U128),
            "u256" => Ok(Self::U256),
            "u512" => Ok(Self::U512),
            "u1024" => Ok(Self::U1024),

            "f8" => Ok(Self::F8),
            "f16" => Ok(Self::F16),
            "f32" => Ok(Self::F32),
            "f64" => Ok(Self::F64),
            "f128" => Ok(Self::F128),
            "f256" => Ok(Self::F256),
            "f512" => Ok(Self::F512),
            "f1024" => Ok(Self::F1024),

            _ => Err(()),
        }
    }
}

impl std::fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimitiveType::Void => write!(f, "void"),
            PrimitiveType::Bool => write!(f, "bool"),
            PrimitiveType::I8 => write!(f, "i8"),
            PrimitiveType::I16 => write!(f, "i16"),
            PrimitiveType::I32 => write!(f, "i32"),
            PrimitiveType::I64 => write!(f, "i64"),
            PrimitiveType::I128 => write!(f, "i128"),
            PrimitiveType::I256 => write!(f, "i256"),
            PrimitiveType::I512 => write!(f, "i512"),
            PrimitiveType::I1024 => write!(f, "i1024"),
            PrimitiveType::U8 => write!(f, "u8"),
            PrimitiveType::U16 => write!(f, "u16"),
            PrimitiveType::U32 => write!(f, "u32"),
            PrimitiveType::U64 => write!(f, "u64"),
            PrimitiveType::U128 => write!(f, "u128"),
            PrimitiveType::U256 => write!(f, "u256"),
            PrimitiveType::U512 => write!(f, "u512"),
            PrimitiveType::U1024 => write!(f, "u1024"),
            PrimitiveType::F8 => write!(f, "f8"),
            PrimitiveType::F16 => write!(f, "f16"),
            PrimitiveType::F32 => write!(f, "f32"),
            PrimitiveType::F64 => write!(f, "f64"),
            PrimitiveType::F128 => write!(f, "f128"),
            PrimitiveType::F256 => write!(f, "f256"),
            PrimitiveType::F512 => write!(f, "f512"),
            PrimitiveType::F1024 => write!(f, "f1024"),
        }
    }
}
