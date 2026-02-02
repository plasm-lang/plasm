use std::str::FromStr;

use serde::Serialize;

use diagnostic::Spanned;
use tokenizer::Number;
use utils::binop::BinaryOp;
use utils::primitive_types::PrimitiveType;

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
