use serde::Serialize;

#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum BinaryOp {
    // Arithemetic operations
    /// a + b
    Add,
    /// a - b
    Sub,
    /// a * b
    Mul,
    /// a / b
    Div,
    /// a % b
    Mod,
    /// a \ b (Integer Division)
    DivInt,
    /// a ** b (Exponentiation)
    Pow,

    // Bitwise operations
    /// a & b (Bitwise AND)
    BitAnd,
    /// a | b (Bitwise OR)
    BitOr,
    /// a ^ b (Bitwise XOR)
    BitXor,
    /// a << b (Shift Left)
    Shl,
    /// a >> b (Shift Right)
    Shr,

    // Logical operations
    ///  a && b
    And,
    ///  a || b
    Or,
    /// a == b
    Eq,
    /// a != b
    Neq,
    /// a < b
    Lt,
    /// a <= b
    Leq,
    /// a > b
    Gt,
    /// a >= b
    Geq,
}

impl BinaryOp {
    /// Returns (left_binding_power, right_binding_power)
    /// Higher binding power means higher precedence
    /// Source: https://en.wikipedia.org/wiki/Order_of_operations
    pub fn binding_power(&self) -> (u8, u8) {
        use BinaryOp::*;
        match self {
            Or => (1, 2),
            And => (3, 4),
            BitOr => (5, 6),
            BitXor => (7, 8),
            BitAnd => (9, 10),
            Eq | Neq => (11, 12),
            Lt | Gt | Leq | Geq => (13, 14),
            Shl | Shr => (15, 16),
            Add | Sub => (17, 18),
            Mul | Div | Mod | DivInt => (19, 20),
            Pow => (21, 22),
        }
    }
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op_str = match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::DivInt => "\\",
            BinaryOp::Pow => "**",

            BinaryOp::BitAnd => "&",
            BinaryOp::BitOr => "|",
            BinaryOp::BitXor => "^",
            BinaryOp::Shl => "<<",
            BinaryOp::Shr => ">>",

            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
            BinaryOp::Eq => "==",
            BinaryOp::Neq => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Gt => ">",
            BinaryOp::Geq => ">=",
            BinaryOp::Leq => "<=",
        };
        write!(f, "{op_str}")
    }
}
