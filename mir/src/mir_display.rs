use std::fmt::{Display, Formatter, Result as FmtResult};

use super::mir::{Function, Global, Instruction, MIR, Module, Operand, RValue, Constant};
use super::types::MIRType;
use super::types::TypeArena;

impl Display for MIR {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        for module in &self.modules {
            writeln!(f, "{}", module)?;
        }
        Ok(())
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        for global in &self.globals {
            let global_string = format_global(global, &TypeArena::new());
            if let Some(global_string) = global_string {
                writeln!(f, "{}", global_string)?;
            }
        }
        for function in &self.functions {
            writeln!(f, "{}", function)?;
        }
        Ok(())
    }
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

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Function::External(func) => {
                writeln!(f, "external fn {}()", func.name)
            }
            Function::Internal(func) => {
                writeln!(f, "fn {}() {{", func.name)?;
                // for local in &int.locals {
                //     writeln!(f, "    let {};", local.name)?;
                // }

                for block in &func.blocks {
                    writeln!(f, "    {} {{", block.label)?;
                    for instruction in &block.instructions {
                        writeln!(f, "        {}", format_instruction(instruction))?;
                    }
                    writeln!(f, "    }}")?;
                }

                writeln!(f, "}}")
            }
        }
    }
}

fn format_instruction(instruction: &Instruction) -> String {
    match instruction {
        Instruction::Assign(id, rvalue) => {
            format!("{} = {}", id, format_rvalue(rvalue),)
        }
        Instruction::Store { value, ptr } => {
            format!("store *{} = {}", format_operand(ptr), format_operand(value),)
        }
    }
}

fn format_rvalue(rvalue: &RValue) -> String {
    match rvalue {
        RValue::Alloca(ty_id) => format!("alloca {:?}", ty_id),
        RValue::Load(ptr) => format!("*{}", ptr),
        RValue::GetElementPtr(val_id) => format!("getelementptr {}", val_id),
        RValue::Call(call) => {
            todo!()
        }
        RValue::BinaryOp(op, left, right) => {
            format!("{} {} {}", format_operand(left), op, format_operand(right))
        }
    }
}

fn format_operand(operand: &Operand) -> String {
    match operand {
        Operand::Use(val_id) => format!("{}", val_id),
        Operand::Constant(constant) => format!("{}", constant),
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "*CONST*")
    }
}
