mod ast_translator;
mod error;
mod hir;
mod hir_display;
mod type_annotator;

pub use ast::Literal;
use diagnostic::MaybeSpanned;

pub use ast_translator::ast_to_hir;
pub use hir::{Expr, ExternalFunction, FunctionSignature, HIRType, Item, Statement, THIR};
use hir::{ExprArena, ExprKind, Function, InternalFunction, Typed};

pub type TypedFunction = Function<Typed>;
pub type TypedInternalFunction = InternalFunction<Typed>;
pub type TypedExprArena = ExprArena<MaybeSpanned<HIRType>>;
pub type TypedExprKind = ExprKind<Typed>;
