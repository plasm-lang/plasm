use ast::ast::{Literal, PrimitiveType};
use diagnostic::Spanned;

use super::ids::{ExprId, FuncId, LocalId};

pub type S<T> = Spanned<T>;

/// Optionally typed HIR
pub type OptHIR = HIR<Option<HIRType>>;

/// Typed HIR
pub type THIR = HIR<HIRType>;

/// High-level Intermediate Representation
#[derive(Debug, Default)]
pub struct HIR<T> {
    pub items: Vec<Item<T>>,
}

impl<T> HIR<T> {
    pub fn with_function(self, func: Function<T>) -> Self {
        let mut items = self.items;
        items.push(Item::Function(func));
        Self { items }
    }
}

#[derive(Debug, Clone)]
pub enum HIRType {
    Primitive(PrimitiveType),
}

#[derive(Debug)]
pub enum Item<T> {
    Function(Function<T>),
}

#[derive(Debug)]
pub struct Function<T> {
    pub id: FuncId,
    pub name: S<String>,
    pub args: Vec<S<Argument>>,
    pub ret_ty: HIRType,
    pub body: Block<T>,
    pub expr_arena: ExprArena<T>,
}

#[derive(Debug)]
pub struct Argument {
    pub name: S<String>,
    pub local_id: LocalId,
    pub ty: S<HIRType>,
}

#[derive(Debug)]
pub struct Block<T> {
    pub locals: Vec<HIRLocal<T>>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct HIRLocal<T> {
    pub id: LocalId,
    pub ty: T,
    pub name: S<String>,
}

#[derive(Debug)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    Expr(ExprId),
    Return(ExprId),
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub local_id: LocalId,
    pub expr_id: ExprId,
}

// --- Expression-related --- //

#[derive(Debug, Default)]
pub struct ExprArena<T>(pub Vec<Expr<T>>);

impl<T> ExprArena<T> {
    pub fn join(self, other: Self) -> Self {
        let mut v = self.0;
        v.extend(other.0);
        Self(v)
    }

    pub fn add(&mut self, expr: Expr<T>) {
        self.0.push(expr);
    }
}

#[derive(Debug)]
pub struct Expr<T> {
    pub id: ExprId,
    pub ty: T,
    pub kind: ExprKind<T>,
}

#[derive(Debug)]
pub enum ExprKind<T> {
    Literal(Literal),
    Local(LocalId),
    FunctionCall(FunctionCall),
    Block(Block<T>),
}

#[derive(Debug)]
pub struct FunctionCall {
    pub func_id: FuncId,
    pub args: Vec<ExprId>,
}
