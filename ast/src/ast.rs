use std::str::FromStr;

use diagnostic::{Span, Spanned};
use tokenizer::Number;

use serde::Serialize;

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

pub type Block = Vec<Statement>;

/// Represents a statement in the AST
/// A statement is a line of code that does something, special language construction, it has no type, cannot be returned
#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    Expr(Expr),
    // Assignment,
    Return(Expr),
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct VariableDeclaration {
    pub name: S<String>,
    pub ty: Option<S<Type>>,
    pub value: Expr,
}

/// Represents an expression in the AST
/// Fundamentally, an expression is a value that can be evaluated, returned, has returning type
#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum Expr {
    Literal(Literal),
    Variable(S<String>),
    FunctionCall(FunctionCall),
    Block(Block),
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct FunctionCall {
    pub name: S<String>,
    pub args: Vec<CallArgument>,
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum Literal {
    Integer(S<String>),
    Float(S<String>),
}

impl Literal {
    pub fn from_number(number: Number, span: Span) -> Self {
        match number {
            Number::Integer(s) => Self::Integer(S::new(s, span)),
            Number::Float(s) => Self::Float(S::new(s, span)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct CallArgument {
    pub name: Option<S<String>>,
    pub value: Expr,
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
