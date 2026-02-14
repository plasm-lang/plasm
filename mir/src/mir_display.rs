use std::fmt::{Display, Formatter, Result as FmtResult};

use super::mir::{
    Constant, Function, Global, Instruction, MIR, Module, Operand, RValue, Terminator, ConstantValue
};
use super::types::MIRType;
use super::types::TypeArena;

impl Display for MIR {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        for module in &self.modules {
            write!(f, "{}", format_module(module, &self))?;
        }
        Ok(())
    }
}

fn format_module(module: &Module, mir: &MIR) -> String {
    let mut result = String::new();
    for global in &module.globals {
        if let Some(global_string) = format_global(global, &mir.type_arena) {
            result.push_str(global_string.as_str());
        }
    }
    for (i, function) in module.functions.iter().enumerate() {
        if i > 0 {
            result.push_str("\n");
        }

        result.push_str(format_function(function, mir).as_str());
    }
    result
}

fn format_global(global: &Global, type_arena: &TypeArena) -> Option<String> {
    let ty: &MIRType = type_arena.get(global.type_id)?;
    Some(format!(
        "let {}: {} = {:?}",
        global.name, ty, global.raw_data
    ))
}

impl Display for MIRType {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            MIRType::Primitive(p) => write!(f, "{p}"),
        }
    }
}

fn format_function(func: &Function, mir: &MIR) -> String {
    match func {
        Function::External(func) => {
            let ret_ty = mir.type_arena.get(func.signature.ret_ty).unwrap();
            format!("external fn {}() -> {}", func.signature.name, ret_ty)
        }
        Function::Internal(func) => {
            let ret_ty = mir.type_arena.get(func.signature.ret_ty).unwrap();
            let mut result = format!("fn {}() -> {} {{\n", func.signature.name, ret_ty);

            for block in &func.blocks {
                result.push_str(&format!("    {} {{\n", block.label));
                for instruction in &block.instructions {
                    result.push_str(&format!("        {}\n", format_instruction(instruction, mir)));
                }

                result.push_str(&format!(
                    "        {}\n",
                    format_terminator(&block.terminator, mir)
                ));

                result.push_str("    }\n");
            }

            result.push_str("}\n");
            result
        }
    }
}

fn format_terminator(terminator: &super::mir::Terminator, mir: &MIR) -> String {
    match terminator {
        Terminator::GoTo(label) => {
            format!("goto {}", label)
        }
        Terminator::Return(operand) => {
            format!("return {}", format_operand(operand, mir))
        }
        _ => unimplemented!(),
    }
}

fn format_instruction(instruction: &Instruction, mir: &MIR) -> String {
    match instruction {
        Instruction::Assign(id, rvalue) => {
            format!("%{} = {}", id, format_rvalue(rvalue, &mir))
        }
        Instruction::Store { value, ptr } => {
            format!("store {}, ptr {}", format_operand(value, mir), format_operand(ptr, mir))
        }
    }
}

fn format_rvalue(rvalue: &RValue, mir: &MIR) -> String {
    match rvalue {
        RValue::Alloca(ty_id) => {
            let ty = mir.type_arena.get(*ty_id).unwrap();
            format!("alloca {}", ty)
        },
        RValue::Load(ptr) => format!("load ptr %{}", ptr),
        RValue::GetElementPtr(val_id) => format!("getelementptr {}", val_id),
        RValue::Call(call) => {
            let func_name = mir.funcs_map.get_by_left(&call.function).unwrap().node.as_str();
            format!("call @{func_name}()")
        }
        RValue::BinaryOp(op, left, right) => {
            format!("{} {} {}", format_operand(left, mir), op, format_operand(right, mir))
        }
    }
}

fn format_operand(operand: &Operand, mir: &MIR) -> String {
    match operand {
        Operand::Use(val_id) => format!("%{}", val_id),
        Operand::Constant(constant) => {
            let ty = mir.type_arena.get(constant.type_id).unwrap();
            format!("{} {}", ty, constant)
        }
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match &self.value {
            ConstantValue::Int(v) => write!(f, "{}", v),
            ConstantValue::Float(v) => write!(f, "{}", v),
            ConstantValue::Void => write!(f, "void"),
        }
    }
}
