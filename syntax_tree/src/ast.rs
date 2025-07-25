pub struct AST {
    pub items: Vec<Item>,
}

pub enum Item {
    Function(Function),
}

pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Block,
}

pub struct Param {
    pub name: String,
    pub type_: Type,
}

type Block = Vec<Statment>;

/// Represents a statement in the AST
/// A statement is a line of code that does something, special language construction, it has no type, cannot be returned
pub enum Statment {
    VariableDeclaration {
        name: String,
        type_: Type,
        value: Expr,
    },
    // Assignment,
    Return(Expr),
}

/// Represents an expression in the AST
/// Fundametally, an expression is a value that can be evaluated, returned, has returning type
pub enum Expr {
    Variable(String),
    Call { name: String, args: Vec<Argument> },
    Block(Block),
}

pub struct Argument {
    pub name: Option<String>,
    pub value: Expr,
}

pub enum Type {
    Primitive(PrimitiveType),
    String, // TODO
    Path,   // TODO
}

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
    F256,
    F512,
    F1024,
}
