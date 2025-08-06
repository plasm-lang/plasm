use tokenizer::Number;

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Item {
    Function(Function),
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub args: Vec<Argument>,
    pub return_type: Type,
    pub body: Block,
}

#[derive(Debug)]
pub struct Argument {
    pub name: String,
    pub type_: Type,
}

pub type Block = Vec<Statment>;

/// Represents a statement in the AST
/// A statement is a line of code that does something, special language construction, it has no type, cannot be returned
#[derive(Debug)]
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
#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Variable(String),
    Call {
        name: String,
        args: Vec<CallArgument>,
    },
    Block(Block),
}

#[derive(Debug)]
pub enum Literal {
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

#[derive(Debug)]
pub struct CallArgument {
    pub name: Option<String>,
    pub value: Expr,
}

#[derive(Debug)]
pub enum Type {
    Primitive(PrimitiveType),
    // String, // TODO
    // Path,   // TODO
}

impl Type {
    pub fn from_str(identifier: &str) -> Self {
        if let Some(_type) = PrimitiveType::from_str(identifier) {
            return Self::Primitive(_type);
        }

        todo!()
    }
}

#[derive(Debug)]
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

impl PrimitiveType {
    pub fn from_str(identifier: &str) -> Option<Self> {
        match identifier {
            "void" => Some(Self::Void),
            "bool" => Some(Self::Bool),

            "i8" => Some(Self::I8),
            "i16" => Some(Self::I16),
            "i32" => Some(Self::I32),
            "i64" => Some(Self::I64),
            "i128" => Some(Self::I128),
            "i256" => Some(Self::I256),
            "i512" => Some(Self::I512),
            "i1024" => Some(Self::I1024),

            "u8" => Some(Self::U8),
            "u16" => Some(Self::U16),
            "u32" => Some(Self::U32),
            "u64" => Some(Self::U64),
            "u128" => Some(Self::U128),
            "u256" => Some(Self::U256),
            "u512" => Some(Self::U512),
            "u1024" => Some(Self::U1024),

            "f8" => Some(Self::F8),
            "f16" => Some(Self::F16),
            "f32" => Some(Self::F32),
            "f64" => Some(Self::F64),
            "f128" => Some(Self::F128),
            "f256" => Some(Self::F256),
            "f512" => Some(Self::F512),
            "f1024" => Some(Self::F1024),

            _ => None,
        }
    }
}
