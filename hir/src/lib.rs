mod ast_translator;
mod error;
mod hir;
mod hir_display;
mod type_annotator;
mod types;

pub use ast::Literal;
use diagnostic::MaybeSpanned;
use utils::ids::HIRTypeId;

pub use ast_translator::ast_to_hir;
pub use hir::{Expr, ExternalFunction, FunctionSignature, Item, Statement, THIR};
use hir::{ExprArena, ExprKind, Function, InternalFunction, Typed};
pub use types::{HIRType, TypeArena};

pub type TypedFunction = Function<Typed>;
pub type TypedInternalFunction = InternalFunction<Typed>;
pub type TypedExprArena = ExprArena<MaybeSpanned<HIRTypeId>>;
pub type TypedExprKind = ExprKind<Typed>;
