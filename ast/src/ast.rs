use std::str::FromStr;

use serde::Serialize;

use diagnostic::Spanned;
use tokenizer::Number;

pub type S<T> = Spanned<T>;

/// Abstract Syntax Tree
#[derive(Debug, PartialEq, Eq, Default, Serialize)]
pub struct AST {
    pub items: Vec<Item>,
}

impl AST {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    pub fn add_function(&mut self, func: Function) {
        self.items.push(Item::Function(func));
    }
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum Item {
    Function(Function),
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct Function {
    pub name: S<String>,
    pub args: Vec<S<Argument>>,
    pub return_type: Option<S<Type>>,
    pub body: Block,
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct Argument {
    pub name: S<String>,
    pub ty: S<Type>,
}

pub type Block = Vec<S<Statement>>;

/// Represents a statement in the AST
/// A statement is a line of code that does something, special language construction, it has no type, cannot be returned
#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    Expr(Expr),
    // Assignment,
    Return(Option<S<Expr>>),
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct VariableDeclaration {
    pub name: S<String>,
    pub ty: Option<S<Type>>,
    pub value: S<Expr>,
}

/// Represents an expression in the AST
/// Fundamentally, an expression is a value that can be evaluated, returned, has returning type
#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum Expr {
    Literal(Literal),
    Variable(String),
    FunctionCall(FunctionCall),
    Block(Block),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Box<S<Expr>>,
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum UnaryOp {
    Negate, // -a
    Not,    // !a
    BitNot, // ~a
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub left: Box<S<Expr>>,
    pub right: Box<S<Expr>>,
}

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

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct FunctionCall {
    pub name: S<String>,
    pub args: Vec<CallArgument>,
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum Literal {
    Void,
    Bool(bool),
    Integer(String),
    Float(String),
}

impl Literal {
    pub fn from_number(number: Number) -> Self {
        match number {
            Number::Integer(s) => Self::Integer(s),
            Number::Float(s) => Self::Float(s),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct CallArgument {
    pub name: Option<S<String>>,
    pub value: S<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum Type {
    Primitive(PrimitiveType),
    // String, // TODO
    // Path,   // TODO
}

impl Type {
    pub fn from_str(identifier: &str) -> Self {
        if let Ok(ty) = PrimitiveType::from_str(identifier) {
            return Self::Primitive(ty);
        }

        todo!()
    }
}

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
