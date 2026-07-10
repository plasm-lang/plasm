mod ast_translator;
mod error;
mod hir;
mod hir_display;
mod type_inference;
mod types;

pub use ast::Literal;
pub use ast_translator::ast_to_hir;
use diagnostic::MaybeSpanned;
pub use hir::{
    Block, Expr, ExprKind, ExternalFunction, FunctionSignature, Item, Statement,
    THIR,
};
use hir::{ExprArena, Function, InternalFunction, Typed};
pub use types::{HIRType, HIRTypeArena};
use utils::ids::HIRTypeId;

pub type TypedFunction = Function<Typed>;
pub type TypedInternalFunction = InternalFunction<Typed>;
pub type TypedExprArena = ExprArena<MaybeSpanned<HIRTypeId>>;
pub type TypedExprKind = ExprKind<Typed>;
