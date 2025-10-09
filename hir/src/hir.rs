use serde::Serialize;

use ast::ast::{Literal, PrimitiveType};
use diagnostic::{MaybeSpanned, Spanned};

use super::ids::{ExprId, FuncId, LocalId};

type S<T> = Spanned<T>;
type MaybeS<T> = MaybeSpanned<T>;

/// Optionally typed HIR
pub type OptHIR = HIR<Option<S<HIRType>>>;

/// Typed HIR
pub type THIR = HIR<MaybeS<HIRType>>;

/// High-level Intermediate Representation
#[derive(Debug, Default, Serialize)]
pub struct HIR<T> {
    pub items: Vec<Item<T>>,
}

impl<T> HIR<T> {
    pub fn with_function(self, func: Function<T>) -> Self {
        let mut items = self.items;
        items.push(Item::Function(func));
        Self { items }
    }

    pub fn empty() -> Self {
        Self { items: Vec::new() }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum HIRType {
    Primitive(PrimitiveType),
}

#[derive(Debug, Serialize)]
pub enum Item<T> {
    Function(Function<T>),
}

#[derive(Debug, Serialize)]
pub struct Function<T> {
    pub signature: FunctionSignature,
    pub body: Block<T>,
    pub expr_arena: ExprArena<T>,
}

#[derive(Debug, Clone, Serialize)]
pub struct FunctionSignature {
    pub id: FuncId,
    pub name: S<String>,
    pub args: Vec<S<Argument>>,
    pub ret_ty: MaybeS<HIRType>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Argument {
    pub name: S<String>,
    pub local_id: LocalId,
    pub ty: S<HIRType>,
}

#[derive(Debug, Serialize)]
pub struct Block<T> {
    pub locals: Vec<HIRLocal<T>>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HIRLocal<T> {
    pub id: LocalId,
    pub ty: T,
    pub name: S<String>,
}

#[derive(Debug, Serialize)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    Expr(ExprId),
    Return(ExprId),
}

#[derive(Debug, Serialize)]
pub struct VariableDeclaration {
    pub local_id: LocalId,
    pub expr_id: ExprId,
}

// --- Expression-related --- //

#[derive(Debug, Default, Serialize)]
pub struct ExprArena<T>(pub Vec<S<Expr<T>>>);

impl<T> ExprArena<T> {
    pub fn join(self, other: Self) -> Self {
        let mut v = self.0;
        v.extend(other.0);
        Self(v)
    }

    pub fn add(&mut self, expr: S<Expr<T>>) {
        self.0.push(expr);
    }
}

#[derive(Debug, Serialize)]
pub struct Expr<T> {
    pub id: ExprId,
    pub ty: T,
    pub kind: ExprKind<T>,
}

#[derive(Debug, Serialize)]
pub enum ExprKind<T> {
    Literal(Literal),
    Local(LocalId),
    FunctionCall(FunctionCall),
    Block(Block<T>),
}

#[derive(Debug, Serialize)]
pub struct FunctionCall {
    pub func_id: FuncId,
    pub args: Vec<ExprId>,
}
