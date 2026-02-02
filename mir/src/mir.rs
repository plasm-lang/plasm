use serde::Serialize;

use utils::binop::BinaryOp;
use utils::ids::{TypeId, ValueId};

use super::types::{MIRType, TypeArena};

#[derive(Debug, Default, Serialize)]
pub struct MIR {
    pub modules: Vec<Module>,
}

#[derive(Debug, Default, Serialize)]
pub struct Module {
    pub globals: Vec<Global>,
    pub functions: Vec<Function>,
}

#[derive(Debug, Serialize)]
pub enum Function {
    External(ExternalFunction),
    Internal(InternalFunction),
}

#[derive(Debug, Serialize)]
pub struct ExternalFunction {
    pub name: String,
}

/// Consider move alloca rvalue to an independent stack slots vec,
/// because allocas should always be called at the beginning of a function
/// and never in loops or conditionals
#[derive(Debug, Serialize)]
pub struct InternalFunction {
    pub name: String,
    pub blocks: Vec<BasicBlock>,
}

#[derive(Debug, Serialize)]
pub struct Global {
    pub name: String,
    pub type_id: TypeId,
    pub raw_data: Vec<u8>,
}

pub type BlockLabel = String;

#[derive(Debug, Serialize)]
pub struct BasicBlock {
    pub label: BlockLabel,
    pub phis: Vec<Phi>,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

#[derive(Debug, Serialize)]
pub struct Phi {
    pub result: ValueId,
    pub incoming: Vec<(Operand, BlockLabel)>,
}

#[derive(Debug, Serialize)]
pub enum Terminator {
    GoTo(BlockLabel),
    Return(Operand),
    Branch {
        cond: Operand,
        then_block: BlockLabel,
        else_block: BlockLabel,
    },
    Switch {
        discr: Operand,
        targets: Vec<(u128, BlockLabel)>,
        fallback: BlockLabel,
    },
    Unreachable,
}

#[derive(Debug, Serialize)]
pub enum Instruction {
    Assign(ValueId, RValue),
    Store { value: Operand, ptr: Operand },
}

/// Operand represents a atomic value used in expressions
#[derive(Debug, Serialize)]
pub enum Operand {
    Use(ValueId), // Local variable
    Constant(Constant),
}

#[derive(Debug, Serialize)]
pub enum RValue {
    /// Allocate stack slot
    Alloca(TypeId),
    /// *ptr
    /// load %ptr in llvm ir
    Load(ValueId),
    /// get pointer ()
    GetElementPtr(ValueId),
    /// Function call
    Call(Call),
    /// Binary operations only for primitive types,
    /// for other types (like structs), function calls must be used
    BinaryOp(BinaryOp, Operand, Operand),
}

#[derive(Debug, Serialize)]
pub struct Call {
    pub function: usize,
    pub args: Vec<ValueId>,
}

#[derive(Debug, Serialize)]
pub struct Constant {}
