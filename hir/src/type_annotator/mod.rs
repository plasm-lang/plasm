//! Algorithm W (Milner, Hindley–Milner inference)
//!
//! The usage flow is:
//! hir::Function<OT> -> func_ctx::FunctionCtx -> Constraints -> solver::Solver -> annotator::FunctionAnnotator -> hir::Function<T>

mod annotator;
mod func_ctx;
mod solver;
mod type_var;

pub use annotator::opt_hir_to_t_hir;
pub use type_var::TyClass;
