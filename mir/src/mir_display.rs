use std::fmt::{Display, Formatter, Result as FmtResult};

use super::mir::{
    Constant, ConstantValue, Function, Global, Instruction, MIR, Module, Operand, RValue,
    Terminator,
};
use super::types::MIRType;
use super::types::TypeArena;

impl Display for MIR {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        for module in &self.modules {
            write!(f, "{}", format_module(module))?;
        }
        Ok(())
    }
}

fn format_module(module: &Module) -> String {
    let mut result = String::new();
    for global in &module.globals {
        if let Some(global_string) = format_global(global, &module.type_arena) {
            result.push_str(global_string.as_str());
        }
    }
    for (i, function) in module.functions.iter().enumerate() {
        if i > 0 {
            result.push_str("\n");
        }

        result.push_str(format_function(function, module).as_str());
    }
    result
}

fn format_global(global: &Global, type_arena: &TypeArena) -> Option<String> {
    let ty = type_arena.get(global.type_id)?;
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

fn format_function(func: &Function, module: &Module) -> String {
    match func {
        Function::External(func) => {
            let ret_ty = module.type_arena.get(func.signature.ret_ty).unwrap();
            format!("external fn {}() -> {}", func.signature.name, ret_ty)
        }
        Function::Internal(func) => {
            let ret_ty = module.type_arena.get(func.signature.ret_ty).unwrap();
            let mut result = format!("fn {}() -> {} {{\n", func.signature.name, ret_ty);

            for block in &func.blocks {
                result.push_str(&format!("    {} {{\n", block.label));
                for instruction in &block.instructions {
                    result.push_str(&format!(
                        "        {}\n",
                        format_instruction(instruction, module)
                    ));
                }

                result.push_str(&format!(
                    "        {}\n",
                    format_terminator(&block.terminator, module)
                ));

                result.push_str("    }\n");
            }

            result.push_str("}\n");
            result
        }
    }
}

fn format_terminator(terminator: &Terminator, module: &Module) -> String {
    match terminator {
        Terminator::GoTo(label) => {
            format!("goto {}", label)
        }
        Terminator::Return(operand) => {
            format!("return {}", format_operand(operand, module))
        }
        _ => unimplemented!(),
    }
}

fn format_instruction(instruction: &Instruction, module: &Module) -> String {
    match instruction {
        Instruction::Assign(id, rvalue) => {
            format!("%{} = {}", id, format_rvalue(rvalue, module))
        }
        Instruction::Store { value, ptr } => {
            format!(
                "store {}, ptr {}",
                format_operand(value, module),
                format_operand(ptr, module)
            )
        }
    }
}

fn format_rvalue(rvalue: &RValue, module: &Module) -> String {
    match rvalue {
        RValue::Alloca(ty_id) => {
            let ty = module.type_arena.get(*ty_id).unwrap();
            format!("alloca {}", ty)
        }
        RValue::Load(type_id, ptr) => {
            let ty = module.type_arena.get(*type_id).unwrap();
            format!("load {}, ptr {}", ty, ptr)
        }
        RValue::GetElementPtr(val_id) => format!("getelementptr {}", val_id),
        RValue::Call(call) => {
            let func_name = module
                .funcs_map
                .get_by_left(&call.function)
                .unwrap()
                .node
                .as_str();
            format!("call @{func_name}()")
        }
        RValue::BinaryOp(op, left, right) => {
            format!(
                "{} {} {}",
                format_operand(left, module),
                op,
                format_operand(right, module)
            )
        }
    }
}

fn format_operand(operand: &Operand, module: &Module) -> String {
    match operand {
        Operand::Use(val_id) => format!("%{}", val_id),
        Operand::Constant(constant) => {
            let ty = module.type_arena.get(constant.type_id).unwrap();
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
