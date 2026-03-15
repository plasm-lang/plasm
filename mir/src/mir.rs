use std::collections::HashMap;

use bimap::BiHashMap;
use serde::Serialize;

use diagnostic::Spanned;
use utils::binop::BinaryOp;
use utils::ids::{FuncId, TypeId, ValueId};

use super::types::TypeArena;

#[derive(Debug, Default, Serialize)]
pub struct MIR {
    pub modules: Vec<Module>,
}

#[derive(Debug, Default, Serialize)]
pub struct Module {
    pub globals: Vec<Global>,
    pub functions: Vec<Function>,
    pub funcs_map: BiHashMap<FuncId, Spanned<String>>,
    pub type_arena: TypeArena,
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
    pub metainfo: MetaInfo,
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
    Load(TypeId, ValueId),
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

#[derive(Debug, Serialize, Default)]
pub struct MetaInfo {
    pub variable_names: HashMap<String, Vec<ValueId>>,
}

impl MetaInfo {
    pub fn add_variable_name(&mut self, name: String, value_id: ValueId) {
        self.variable_names
            .entry(name)
            .or_insert_with(Vec::new)
            .push(value_id);
    }

    pub fn get_variable_name(&self, value_id: ValueId) -> String {
        for (name, ids) in &self.variable_names {
            if ids.contains(&value_id) {
                return name.clone();
            }
        }
        value_id.to_string()
    }

    pub fn bind_values(&mut self, existing_id: ValueId, new_id: ValueId) {
        for (_, ids) in &mut self.variable_names {
            if ids.contains(&existing_id) {
                if !ids.contains(&new_id) {
                    ids.push(new_id);
                }
                return;
            }
        }
    }
}
