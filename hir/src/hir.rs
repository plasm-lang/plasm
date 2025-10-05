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
pub struct ExprArena<T>(Vec<Expr<T>>);

impl<T> ExprArena<T> {
    pub fn join(self, other: Self) -> Self {
        let mut v = self.0;
        v.extend(other.0);
        Self(v)
    }
}

#[derive(Debug)]
pub struct Expr<T> {
    id: ExprId,
    ty: T,
    kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    Literal(Literal),
    Local(LocalId),
    FunctionCall(FunctionCall),
}

#[derive(Debug)]
pub struct FunctionCall {
    func_id: FuncId,
    args: Vec<ExprId>,
}
