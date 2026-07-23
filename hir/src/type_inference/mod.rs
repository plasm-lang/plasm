//! This is how the data flows between
//! type inference components:
//! ```text
//!              OptHIR
//!                |
//!                V
//! +------------------------------+
//! |     TypeInferenceEngine      |
//! +------------------------------+
//! |                              |
//! |  InternalFunction<OptTyped>  |
//! |              |               |
//! |              V               |
//! |  +-----------------------+   |
//! |  | FunctionConstraintGen |   |
//! |  +-----------------------+   |
//! |              |               |
//! |         Constraints          |
//! |              |               |
//! |              V               |
//! |    +--------------------+    |
//! |    | FunctionTypeSolver |    |
//! |    +--------------------+    |
//! |              |               |
//! |           Solution           |
//! |              |               |
//! |              V               |
//! |     +-------------------+    |
//! |     | FunctionAnnotator |    |
//! |     +-------------------+    |
//! |              |               |
//! |              V               |
//! |   InternalFunction<Typed>    |
//! |                              |
//! +------------------------------+
//!                |
//!                V
//!              THIR
//! ```

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
