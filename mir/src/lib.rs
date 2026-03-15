//! MIR - Middle-level Intermediate Representation.
//!
//! It's represented as CFG (control flow graph) in SSA (static single assignment) form without
//! Phi functions, but with alloca, store and load instructions instead.
//!
//! The main purpose of introducing custom MIR instead of transforming HIR directly to LLVM IR is
//! dependency inversion. By having our own MIR, we can implement optimizations and transformations
//! on it without relying on LLVM's optimization passes. This allows us to have more control over
//! the compilation process and potentially achieve better performance for our specific use case.
//! The MIR itself is designed as a subset of LLVM IR - it is not intended to be a full-fledged
//! universal IR, but rather a simplified version that captures the essential features needed for
//! our compiler.

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
