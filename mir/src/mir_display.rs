use std::fmt::{Display, Formatter, Result as FmtResult};

use super::mir::{
    Call, Constant, ConstantValue, Function, FunctionSignature, Global, Instruction, MIR, MetaInfo,
    Module, Operand, RValue, Terminator,
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
            let mut result = format_signature(&func.signature, module, &func.metainfo);
            result.push_str("\n");
            result
        }
        Function::Internal(func) => {
            let mut result = format_signature(&func.signature, module, &func.metainfo);
            result.push_str(" {\n");

            for block in &func.blocks {
                result.push_str(&format!("    {} {{\n", block.label));
                for instruction in &block.instructions {
                    result.push_str(&format!(
                        "        {}\n",
                        format_instruction(instruction, module, &func.metainfo)
                    ));
                }

                result.push_str(&format!(
                    "        {}\n",
                    format_terminator(&block.terminator, module, &func.metainfo)
                ));

                result.push_str("    }\n");
            }

            result.push_str("}\n");
            result
        }
    }
}

fn format_signature(signature: &FunctionSignature, module: &Module, metainfo: &MetaInfo) -> String {
    let ret_ty = module.type_arena.get(signature.ret_ty).unwrap();

    let args = signature
        .args
        .iter()
        .map(|(arg_ty_id, value_id)| {
            let arg_name = metainfo.get_variable_name(*value_id);
            let arg_type = module.type_arena.get(*arg_ty_id).unwrap();
            format!("%{}: {}", arg_name, arg_type)
        })
        .collect::<Vec<_>>()
        .join(", ");

    format!("fn {}({}) -> {}", signature.name, args, ret_ty)
}

fn format_terminator(terminator: &Terminator, module: &Module, metainfo: &MetaInfo) -> String {
    match terminator {
        Terminator::GoTo(label) => {
            format!("goto {}", label)
        }
        Terminator::Return(operand) => {
            format!("return {}", format_operand(operand, module, metainfo))
        }
        _ => unimplemented!(),
    }
}

fn format_instruction(instruction: &Instruction, module: &Module, metainfo: &MetaInfo) -> String {
    match instruction {
        Instruction::Assign(id, rvalue) => {
            format!(
                "%{} = {}",
                metainfo.get_variable_name(*id),
                format_rvalue(rvalue, module, metainfo)
            )
        }
        Instruction::Call(call) => format_call(call, module, metainfo),
        Instruction::Store { value, ptr } => {
            format!(
                "store {}, ptr {}",
                format_operand(value, module, metainfo),
                format_operand(ptr, module, metainfo)
            )
        }
    }
}

fn format_rvalue(rvalue: &RValue, module: &Module, metainfo: &MetaInfo) -> String {
    match rvalue {
        RValue::Alloca(ty_id) => {
            let ty = module.type_arena.get(*ty_id).unwrap();
            format!("alloca {}", ty)
        }
        RValue::Load(type_id, ptr) => {
            let ty = module.type_arena.get(*type_id).unwrap();
            let ptr_str = format!("%{}", metainfo.get_variable_name(*ptr));
            format!("load {}, ptr {}", ty, ptr_str)
        }
        RValue::GetElementPtr(val_id) => format!("getelementptr {}", val_id),
        RValue::Call(call) => format_call(call, module, metainfo),
        RValue::BinaryOp(op, left, right) => {
            format!(
                "{} {} {}",
                format_operand(left, module, metainfo),
                op,
                format_operand(right, module, metainfo)
            )
        }
    }
}

fn format_call(call: &Call, module: &Module, metainfo: &MetaInfo) -> String {
    let func_name = module
        .funcs_map
        .get_by_left(&call.function)
        .unwrap()
        .node
        .as_str();
    let args = call
        .args
        .iter()
        .map(|arg| format_operand(arg, module, metainfo))
        .collect::<Vec<_>>()
        .join(", ");
    format!("call @{func_name}({args})")
}

fn format_operand(operand: &Operand, module: &Module, metainfo: &MetaInfo) -> String {
    match operand {
        Operand::Use(val_id) => format!("%{}", metainfo.get_variable_name(*val_id)),
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
