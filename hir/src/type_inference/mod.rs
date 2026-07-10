mod annotator;
mod constraint_gen;
mod engine;
mod error;
mod type_class;
mod type_solver;
mod type_var;
mod unifier;

pub use engine::opt_hir_to_t_hir;
pub use error::TypeInferenceError;
