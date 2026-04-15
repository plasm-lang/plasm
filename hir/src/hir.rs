use std::collections::HashMap;

use bimap::BiHashMap;
use serde::Serialize;

use ast::ast::Literal;
use diagnostic::{MaybeSpanned, Spanned};
use utils::ids::{ExprId, FuncId, HIRTypeId, LocalId};

use super::types::TypeArena;

type S<T> = Spanned<T>;
type MS<T> = MaybeSpanned<T>;

/// Optionally typed HIR
pub type OptTyped = Option<S<HIRTypeId>>;
pub type OptHIR = HIR<OptTyped>;

/// Typed HIR
pub type Typed = MS<HIRTypeId>;
pub type THIR = HIR<Typed>;

/// High-level Intermediate Representation
/// TODO: Later rename to HIRModule, and make HIR be a collection of modules
/// For now, we only support one module, so HIR and HIRModule are the same
#[derive(Debug, Default, Serialize)]
pub struct HIR<T> {
    pub items: Vec<Item<T>>,
    pub funcs_map: BiHashMap<FuncId, S<String>>,
    pub type_arena: TypeArena,
}

impl<T> HIR<T> {
    pub fn empty() -> Self {
        Self {
            items: Vec::new(),
            funcs_map: BiHashMap::new(),
            type_arena: TypeArena::new(),
        }
    }
}

#[derive(Debug, Serialize)]
pub enum Item<T> {
    Function(Function<T>),
}

#[derive(Debug, Serialize)]
pub enum Function<T> {
    Internal(InternalFunction<T>),
    External(ExternalFunction),
}

impl<T> Function<T> {
    pub fn signature(&self) -> &FunctionSignature {
        match self {
            Function::Internal(func) => &func.signature,
            Function::External(func) => &func.signature,
        }
    }
}

#[derive(Debug, Serialize)]
pub struct InternalFunction<T> {
    pub signature: FunctionSignature,
    pub body: Block<T>,
    pub expr_arena: ExprArena<T>,
}

#[derive(Debug, Serialize)]
pub struct ExternalFunction {
    pub signature: FunctionSignature,
}

#[derive(Debug, Clone, Serialize)]
pub struct FunctionSignature {
    pub id: FuncId,
    pub name: S<String>,
    pub args: Vec<S<Argument>>,
    pub ret_ty: MS<HIRTypeId>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Argument {
    pub name: S<String>,
    pub local_id: LocalId,
    pub ty: S<HIRTypeId>,
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
pub struct ExprArena<T>(pub HashMap<ExprId, S<Expr<T>>>);

impl<T> ExprArena<T> {
    pub fn join(self, other: Self) -> Self {
        let mut v = self.0;
        v.extend(other.0);
        Self(v)
    }

    pub fn insert(&mut self, id: ExprId, expr: S<Expr<T>>) {
        self.0.insert(id, expr);
    }

    pub fn get(&self, id: ExprId) -> Option<&S<Expr<T>>> {
        self.0.get(&id)
    }
}

#[derive(Debug, Serialize)]
pub struct Expr<T> {
    pub ty: T,
    pub kind: ExprKind<T>,
}

#[derive(Debug, Serialize)]
pub enum ExprKind<T> {
    Literal(Literal),
    Local(LocalId),
    FunctionCall(FunctionCall),
    Block(Block<T>),
    // Binary(Binary<T>),
}

#[derive(Debug, Serialize)]
pub struct FunctionCall {
    pub func_id: FuncId,
    pub args: Vec<ExprId>,
}

// #[derive(Debug, Serialize)]
// pub struct Binary<T> {

// }
