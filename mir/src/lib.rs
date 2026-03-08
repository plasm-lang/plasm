//! MIR - Mid-level Intermediate Representation,
//! represented as CFG (control flow graph) in SSA (static single assignment) form.

mod hir_lowering;
mod mir;
mod mir_display;
mod types;

pub use hir_lowering::hir_to_mir;
pub use mir::{
    BasicBlock, Constant, ConstantValue, ExternalFunction, Function, FunctionSignature,
    Instruction, InternalFunction, MIR, Module, Operand, RValue, Terminator,
};
pub use types::{MIRType, TypeArena};
