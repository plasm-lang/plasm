//! Algorithm W (Milner, Hindley–Milner inference)

mod annotator;
mod func_ctx;
mod solver;
mod type_var;

pub use annotator::opt_hir_to_t_hir;
pub use type_var::TyClass;
