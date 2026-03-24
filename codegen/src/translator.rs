use std::collections::HashMap;

use inkwell::OptimizationLevel;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StringRadix};
use inkwell::values::{BasicValue, BasicValueEnum};

use mir::MIR;
use utils::ids::ValueId;
use utils::primitive_types::PrimitiveType;

pub fn mir_to_llvm_ir_string(mir: MIR) -> String {
    let context = Context::create();
    let mir_module = mir.modules.into_iter().next().unwrap();
    MIRModuleTranslator::new(&context, mir_module).translate_to_llvm_ir_string()
}

pub fn mir_to_asm_string(mir: MIR) -> String {
    let context = Context::create();
    let mir_module = mir.modules.into_iter().next().unwrap();
    let target_machine = get_target_machine();
    MIRModuleTranslator::new(&context, mir_module).translate_to_asm_string(target_machine)
}

fn get_target_machine() -> TargetMachine {
    Target::initialize_all(&InitializationConfig::default());
    let target_triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&target_triple).unwrap();
    let cpu = TargetMachine::get_host_cpu_name().to_string();
    let features = TargetMachine::get_host_cpu_features().to_string();

    target
        .create_target_machine(
            &target_triple,
            &cpu,
            &features,
            OptimizationLevel::Default,
            RelocMode::Default,
            CodeModel::Default,
        )
        .unwrap()
}

pub struct MIRModuleTranslator<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    mir_module: mir::Module,
}

impl<'ctx> MIRModuleTranslator<'ctx> {
    pub fn new(context: &'ctx Context, mir_module: mir::Module) -> Self {
        let module = context.create_module("main");
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
            mir_module,
        }
    }

    pub fn translate_to_asm_string(self, target_machine: TargetMachine) -> String {
        let llvm_module = self.translate();
        llvm_module.verify().unwrap();

        let memory_buffer = target_machine
            .write_to_memory_buffer(&llvm_module, FileType::Assembly)
            .unwrap();
        let asm_bytes = memory_buffer.as_slice();
        let asm_string = String::from_utf8_lossy(asm_bytes).into_owned();
        asm_string
    }

    pub fn translate_to_llvm_ir_string(self) -> String {
        let llvm_module = self.translate();
        llvm_module.verify().unwrap();
        llvm_module.print_to_string().to_string()
    }

    fn translate(self) -> Module<'ctx> {
        // First pass: register functions (LLVM declare)
        for func in self.mir_module.functions.iter() {
            self.register_function(func);
        }
        // Second pass: translate function bodies
        for func in self.mir_module.functions.iter() {
            self.translate_function(func);
        }
        self.module
    }

    fn register_function(&self, func: &mir::Function) {
        let signature = match func {
            mir::Function::External(ext_func) => &ext_func.signature,
            mir::Function::Internal(int_func) => &int_func.signature,
        };

        let metainfo = match func {
            mir::Function::External(ext_func) => &ext_func.metainfo,
            mir::Function::Internal(int_func) => &int_func.metainfo,
        };

        let ret_ty = self.mir_module.type_arena.get(signature.ret_ty).unwrap();
        let arg_types: Vec<BasicMetadataTypeEnum> = signature
            .args
            .iter()
            .map(|(arg_ty_id, _)| {
                let arg_ty = self.mir_module.type_arena.get(*arg_ty_id).unwrap();
                self.get_llvm_type(arg_ty).unwrap().into()
            })
            .collect();

        let fn_type = match self.get_llvm_type(ret_ty) {
            Some(basic_ret_ty) => basic_ret_ty.fn_type(&arg_types, false),
            None => self.context.void_type().fn_type(&arg_types, false),
        };

        let linkage = match func {
            mir::Function::External(_) => Some(Linkage::External),
            mir::Function::Internal(_) => None,
        };

        let llvm_fn = self.module.add_function(&signature.name, fn_type, linkage);

        for (i, (_, value_id)) in signature.args.iter().enumerate() {
            let arg_name = metainfo.get_variable_name(*value_id);
            let arg = llvm_fn.get_nth_param(i as u32).unwrap();
            arg.set_name(&arg_name);
        }
    }

    fn translate_function(&self, func: &mir::Function) {
        match func {
            mir::Function::Internal(int_func) => self.translate_internal_function(int_func),
            mir::Function::External(_ext_func) => {} // No body to translate for external functions
        }
    }

    fn translate_internal_function(&self, func: &mir::InternalFunction) {
        let function = self.module.get_function(&func.signature.name).unwrap();

        let mut bb_map: HashMap<&str, BasicBlock<'ctx>> = HashMap::new();
        for block in &func.blocks {
            let basic_block = self.context.append_basic_block(function, &block.label);
            bb_map.insert(block.label.as_str(), basic_block);
        }

        let mut value_map: HashMap<ValueId, BasicValueEnum<'ctx>> = HashMap::new();

        for (i, (_, value_id)) in func.signature.args.iter().enumerate() {
            let arg = function.get_nth_param(i as u32).unwrap();
            let arg_name = func.metainfo.get_variable_name(*value_id);
            arg.set_name(&arg_name);
            value_map.insert(*value_id, arg);
        }

        for block in &func.blocks {
            self.builder.position_at_end(bb_map[block.label.as_str()]);
            self.translate_basic_block(block, &bb_map, &mut value_map, &func.metainfo);
        }
    }

    fn translate_basic_block(
        &self,
        block: &mir::BasicBlock,
        bb_map: &HashMap<&str, BasicBlock<'ctx>>,
        value_map: &mut HashMap<ValueId, BasicValueEnum<'ctx>>,
        metainfo: &mir::MetaInfo,
    ) {
        for instruction in &block.instructions {
            self.translate_instruction(instruction, value_map, metainfo);
        }
        self.translate_terminator(&block.terminator, bb_map, value_map);
    }

    fn translate_instruction(
        &self,
        instruction: &mir::Instruction,
        value_map: &mut HashMap<ValueId, BasicValueEnum<'ctx>>,
        metainfo: &mir::MetaInfo,
    ) {
        match instruction {
            mir::Instruction::Assign(id, rvalue) => {
                let name = metainfo.get_variable_name(*id);
                let llvm_value = self.translate_rvalue(rvalue, value_map, &name);
                value_map.insert(*id, llvm_value);
            }
            mir::Instruction::Store { value, ptr } => {
                let llvm_val = self.translate_operand(value, value_map).unwrap();
                let llvm_ptr = self
                    .translate_operand(ptr, value_map)
                    .unwrap()
                    .into_pointer_value();

                self.builder.build_store(llvm_ptr, llvm_val).unwrap();
            }
        }
    }

    fn translate_terminator(
        &self,
        terminator: &mir::Terminator,
        bb_map: &HashMap<&str, BasicBlock<'ctx>>,
        value_map: &HashMap<ValueId, BasicValueEnum<'ctx>>,
    ) {
        match terminator {
            mir::Terminator::GoTo(label) => {
                let target_bb = bb_map[label.as_str()];
                self.builder.build_unconditional_branch(target_bb).unwrap();
            }
            mir::Terminator::Return(operand) => {
                let llvm_value = self.translate_operand(operand, value_map);
                let return_value = llvm_value.as_ref().map(|v| v as &dyn BasicValue);
                self.builder.build_return(return_value).unwrap();
            }
            mir::Terminator::Branch {
                cond,
                then_block,
                else_block,
            } => unimplemented!(),
            mir::Terminator::Switch {
                discr,
                targets,
                fallback,
            } => unimplemented!(),
            mir::Terminator::Unreachable => {
                self.builder.build_unreachable().unwrap();
            }
        };
    }

    fn translate_operand(
        &self,
        operand: &mir::Operand,
        value_map: &HashMap<ValueId, BasicValueEnum<'ctx>>,
    ) -> Option<BasicValueEnum<'ctx>> {
        match operand {
            mir::Operand::Use(value_id) => value_map.get(value_id).cloned(),
            mir::Operand::Constant(constant) => self.translate_constant(constant),
        }
    }

    fn translate_rvalue(
        &self,
        rvalue: &mir::RValue,
        value_map: &HashMap<ValueId, BasicValueEnum<'ctx>>,
        name: &str,
    ) -> BasicValueEnum<'ctx> {
        match rvalue {
            mir::RValue::Alloca(type_id) => {
                let mir_ty = self.mir_module.type_arena.get(*type_id).unwrap();
                let llvm_ty = self.get_llvm_type(mir_ty).unwrap();
                self.builder.build_alloca(llvm_ty, name).unwrap().into()
            }
            mir::RValue::Load(type_id, ptr_id) => {
                let ptr_val = value_map.get(ptr_id).unwrap().into_pointer_value();
                let mir_ty = self.mir_module.type_arena.get(*type_id).unwrap();
                let llvm_ty = self.get_llvm_type(mir_ty).unwrap();
                let load_res = self.builder.build_load(llvm_ty, ptr_val, name).unwrap();
                load_res
            }
            mir::RValue::GetElementPtr(value_id) => todo!(),
            mir::RValue::Call(call) => {
                let func_name = self
                    .mir_module
                    .funcs_map
                    .get_by_left(&call.function)
                    .unwrap()
                    .node
                    .as_str();
                let llvm_func = self.module.get_function(func_name).unwrap();

                let args = call
                    .args
                    .iter()
                    .map(|arg| self.translate_operand(arg, value_map).unwrap().into())
                    .collect::<Vec<_>>();

                let call_site = self.builder.build_call(llvm_func, &args, name).unwrap();
                call_site.set_tail_call(true);
                call_site.try_as_basic_value().unwrap_basic()
            }
            mir::RValue::BinaryOp(binary_op, operand, operand1) => todo!(),
        }
    }

    fn translate_constant(&self, constant: &mir::Constant) -> Option<BasicValueEnum<'ctx>> {
        let mir_ty = self.mir_module.type_arena.get(constant.type_id).unwrap();
        match &constant.value {
            mir::ConstantValue::Int(val_str) => {
                let llvm_ty = self.get_llvm_type(mir_ty).unwrap().into_int_type();
                llvm_ty
                    .const_int_from_string(val_str, StringRadix::Decimal)
                    .map(|v| v.into())
            }
            mir::ConstantValue::Float(val_str) => {
                let llvm_ty = self.get_llvm_type(mir_ty).unwrap().into_float_type();
                Some(unsafe { llvm_ty.const_float_from_string(val_str) }.into())
            }
            mir::ConstantValue::Void => None,
        }
    }

    fn get_llvm_type(&self, ty: &mir::MIRType) -> Option<BasicTypeEnum<'ctx>> {
        use PrimitiveType::*;
        match ty {
            mir::MIRType::Primitive(p) => match p {
                Void => None,
                Bool => Some(self.context.bool_type().into()),
                I8 | U8 => Some(self.context.i8_type().into()),
                I16 | U16 => Some(self.context.i16_type().into()),
                I32 | U32 => Some(self.context.i32_type().into()),
                I64 | U64 => Some(self.context.i64_type().into()),
                I128 | U128 => Some(self.context.i128_type().into()),
                I256 | U256 => Some(self.context.custom_width_int_type(256).into()),
                I512 | U512 => Some(self.context.custom_width_int_type(512).into()),
                I1024 | U1024 => Some(self.context.custom_width_int_type(1024).into()),
                F8 => unimplemented!(),
                F16 => Some(self.context.f16_type().into()),
                F32 => Some(self.context.f32_type().into()),
                F64 => Some(self.context.f64_type().into()),
                F128 => Some(self.context.f128_type().into()),
                F256 => unimplemented!(),
                F512 => unimplemented!(),
                F1024 => unimplemented!(),
            },
        }
    }
}
