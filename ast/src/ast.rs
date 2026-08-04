use std::str::FromStr;

use diagnostic::Spanned;
use serde::Serialize;
use tokenizer::Number;
use utils::bin_op::BinaryOp;
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

    pub fn add_type_definition(&mut self, ty_def: TypeDefinition) {
        self.items.push(Item::TypeDefinition(ty_def));
    }
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum Item {
    Function(Function),
    TypeDefinition(TypeDefinition),
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum Function {
    External(ExternalFunction),
    Internal(InternalFunction),
}

impl Function {
    pub fn signature(&self) -> &FunctionSignature {
        match self {
            Function::Internal(func) => &func.signature,
            Function::External(func) => &func.signature,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct InternalFunction {
    pub signature: FunctionSignature,
    pub body: Block,
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct ExternalFunction {
    pub signature: FunctionSignature,
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct FunctionSignature {
    pub name: S<String>,
    pub args: Vec<S<Argument>>,
    pub return_type: Option<S<Type>>,
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct Argument {
    pub name: S<String>,
    pub ty: S<Type>,
}

pub type Block = Vec<S<Statement>>;

/// Represents a statement in the AST
/// A statement is a line of code that does something, special language
/// construction, it has no type, cannot be returned
#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    Expr(Expr),
    Assignment(S<Place>, S<Expr>),
    Return(Option<S<Expr>>),
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct VariableDeclaration {
    pub name: S<String>,
    pub ty: Option<S<Type>>,
    pub value: S<Expr>,
}

/// Represents an expression in the AST
/// Fundamentally, an expression is a value that can be evaluated, returned, has
/// returning type
#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum Expr {
    // RValue?
    Literal(Literal),
    Variable(String),
    FunctionCall(FunctionCall),
    Block(Block),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    StructLiteral(StructLiteralExpr),
    FieldAccess(FieldAccess),
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct StructLiteralExpr {
    pub fields: Vec<S<StructLiteralField>>,
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct StructLiteralField {
    pub name: S<String>,
    pub value: S<Expr>,
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
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

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct TypeDefinition {
    pub name: S<String>,
    pub ty: S<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum Type {
    Primitive(PrimitiveType),
    Struct(StructType),
    Named(String),
    // String, // TODO
    // Path,   // TODO
}

impl Type {
    pub fn from_ident(identifier: &str) -> Self {
        if let Ok(ty) = PrimitiveType::from_str(identifier) {
            return Self::Primitive(ty);
        }

        Self::Named(identifier.to_string())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct StructType {
    pub fields: Vec<S<StructField>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct StructField {
    pub name: S<String>,
    pub ty: S<Type>,
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct FieldAccess {
    pub base: Box<S<Expr>>,
    pub field_name: S<String>,
}

// `Place` and `FieldAccess` are similar, but `Place` is for assignment target,
// `FieldAccess` is for expression. It's not unified to avoid expression
// assignment like `3 = 5` or `"string1" = "string2"`.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum Place {
    Variable(S<String>),
    Field {
        base: Box<S<Place>>,
        field_name: S<String>,
    },
}
