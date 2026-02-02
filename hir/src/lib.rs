mod ast_translator;
mod error;
mod hir;
mod hir_display;
mod type_annotator;

pub use ast_translator::ast_to_hir;
use hir::{Function, Typed};
pub use hir::{HIRType, Item, Statement, THIR};

pub type TypedFunction = Function<Typed>;
