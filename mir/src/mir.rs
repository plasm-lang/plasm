use bimap::BiHashMap;
use serde::Serialize;

use diagnostic::Spanned;
use utils::binop::BinaryOp;
use utils::ids::{TypeId, ValueId, FuncId};

use super::types::{MIRType, TypeArena};

#[derive(Debug, Default, Serialize)]
pub struct MIR {
    pub modules: Vec<Module>,
    pub type_arena: TypeArena,
    pub funcs_map: BiHashMap<FuncId, Spanned<String>>,
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
    pub signature: FunctionSignature,
}

/// Consider move alloca rvalue to an independent stack slots vec,
/// because allocas should always be called at the beginning of a function
/// and never in loops or conditionals
#[derive(Debug, Serialize)]
pub struct InternalFunction {
    pub signature: FunctionSignature,
    pub blocks: Vec<BasicBlock>,
}

#[derive(Debug, Serialize)]
pub struct FunctionSignature {
    pub id: FuncId,
    pub name: String,
    // pub args: Vec<TypeId>,
    pub ret_ty: TypeId,
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
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

#[derive(Debug, Serialize)]
pub enum Terminator {
    // None,
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
    pub function: FuncId,
    pub args: Vec<Operand>,
}

#[derive(Debug, Serialize)]
pub struct Constant {
    pub type_id: TypeId,
    pub value: ConstantValue,
}

impl Constant {
    pub fn bool(type_id: TypeId, value: bool) -> Self {
        Constant {
            type_id,
            value: ConstantValue::Int(if value {
                "1".to_string()
            } else {
                "0".to_string()
            }),
        }
    }

    pub fn int(type_id: TypeId, value: String) -> Self {
        Constant {
            type_id,
            value: ConstantValue::Int(value),
        }
    }

    pub fn float(type_id: TypeId, value: String) -> Self {
        Constant {
            type_id,
            value: ConstantValue::Float(value),
        }
    }

    pub fn void(type_id: TypeId) -> Self {
        Constant {
            type_id,
            value: ConstantValue::Void,
        }
    }
}

#[derive(Debug, Serialize)]
pub enum ConstantValue {
    Int(String),
    Float(String),
    Void,
}
