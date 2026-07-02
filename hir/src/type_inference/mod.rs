// mod annotator;
// mod func_ctx;
mod engine;
mod type_solver;
// mod solver_old;
mod constraint_gen;
mod error;
mod type_class;
mod type_var;
mod unifier;

pub use engine::opt_hir_to_t_hir;
pub use type_class::TypeClass;
