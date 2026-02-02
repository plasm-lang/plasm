//! MIR - Mid-level Intermediate Representation,
//! represented as CFG (control flow graph) in SSA (static single assignment) form.

mod hir_lowering;
mod mir;
mod mir_display;
mod types;

pub use hir_lowering::hir_to_mir;
