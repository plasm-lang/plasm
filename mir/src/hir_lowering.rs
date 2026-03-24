use std::collections::HashMap;

use bimap::BiHashMap;

use hir::THIR;
use utils::ids::{ExprId, LocalId, ValueId};

use super::mir::{
    BasicBlock, BlockLabel, Call, Constant, ExternalFunction, Function, FunctionSignature,
    Instruction, InternalFunction, MIR, MetaInfo, Module, Operand, RValue, Terminator,
};
use super::types::{MIRType, TypeArena};

pub fn hir_to_mir(hir: THIR) -> MIR {
    let translator = HIRTranslator::new();
    translator.translate(hir)
}

struct HIRTranslator {
    mir: MIR,
}

impl HIRTranslator {
    fn new() -> Self {
        HIRTranslator {
            mir: MIR {
                modules: vec![Module {
                    globals: vec![],
                    functions: vec![],
                    funcs_map: BiHashMap::new(),
                    type_arena: TypeArena::new(),
                }],
            },
        }
    }

    fn translate(mut self, hir: THIR) -> MIR {
        self.mir.modules[0].funcs_map = hir.funcs_map;
        for item in hir.items {
            match item {
                hir::Item::Function(func) => {
                    let func = self.translate_function(func);
                    self.mir.modules[0].functions.push(func);
                }
            }
        }
        self.mir
    }

    fn translate_function(&mut self, func: hir::TypedFunction) -> Function {
        let translator = HIRFunctionTranslator::new(&mut self.mir.modules[0]);
        translator.translate(func)
    }
}

struct HIRFunctionTranslator<'a> {
    vreg_counter: ValueId,
    blocks: Vec<BasicBlock>,
    current_block_idx: usize,
    metainfo: MetaInfo,

    stack_slot_ptrs: HashMap<LocalId, ValueId>,
    module: &'a mut Module,
}

impl<'a> HIRFunctionTranslator<'a> {
    fn new(module: &'a mut Module) -> Self {
        let entry_block = BasicBlock {
            label: "entry".into(),
            instructions: vec![],
            terminator: Terminator::Unreachable,
        };
        Self {
            blocks: vec![entry_block],
            current_block_idx: 0,
            vreg_counter: ValueId::one(),
            stack_slot_ptrs: HashMap::new(),
            metainfo: MetaInfo::default(),
            module,
        }
    }

    fn next_vreg(&mut self) -> ValueId {
        let id = self.vreg_counter;
        self.vreg_counter = self.vreg_counter.increment();
        id
    }

    fn new_block(&mut self, label: BlockLabel) -> usize {
        let block = BasicBlock {
            label,
            instructions: vec![],
            terminator: Terminator::Unreachable,
        };
        self.blocks.push(block);
        self.blocks.len() - 1
    }

    fn emit_instruction(&mut self, instruction: Instruction) {
        self.blocks[self.current_block_idx]
            .instructions
            .push(instruction);
    }

    fn set_terminator(&mut self, terminator: Terminator) {
        self.blocks[self.current_block_idx].terminator = terminator;
    }

    fn lower_expr(&mut self, expr_id: ExprId, expr_arena: &hir::TypedExprArena) -> Operand {
        let expr = expr_arena.get(expr_id).unwrap().as_ref();
        let ty = MIRType::from_hir(expr.ty.node.clone());
        let type_id = self.module.type_arena.get_or_insert(ty);

        match &expr.kind {
            hir::TypedExprKind::Literal(lit) => match lit {
                hir::Literal::Bool(v) => Operand::Constant(Constant::bool(type_id, *v)),
                hir::Literal::Integer(v) => Operand::Constant(Constant::int(type_id, v.clone())),
                hir::Literal::Float(v) => Operand::Constant(Constant::float(type_id, v.clone())),
                hir::Literal::Void => Operand::Constant(Constant::void(type_id)),
            },
            hir::TypedExprKind::FunctionCall(hir_call) => {
                let value_id = self.next_vreg();

                let args = hir_call
                    .args
                    .iter()
                    .map(|arg_expr_id| self.lower_expr(*arg_expr_id, expr_arena))
                    .collect();

                let rvalue = RValue::Call(Call {
                    function: hir_call.func_id,
                    args,
                });
                self.emit_instruction(Instruction::Assign(value_id, rvalue));
                Operand::Use(value_id)
            }
            hir::TypedExprKind::Local(local_id) => {
                let stack_ptr = *self.stack_slot_ptrs.get(local_id).unwrap();
                let value_id = self.next_vreg();
                let rvalue = RValue::Load(type_id, stack_ptr);
                self.emit_instruction(Instruction::Assign(value_id, rvalue));
                Operand::Use(value_id)
            }
            _ => unimplemented!("Expression kind not supported yet: {:?}", expr.kind),
        }
    }

    fn lower_statement(&mut self, statement: hir::Statement, expr_arena: &hir::TypedExprArena) {
        match statement {
            hir::Statement::VariableDeclaration(decl) => {
                let stack_ptr = *self.stack_slot_ptrs.get(&decl.local_id).unwrap();
                let operand = self.lower_expr(decl.expr_id, expr_arena);

                let instruction = Instruction::Store {
                    value: operand,
                    ptr: Operand::Use(stack_ptr),
                };
                self.emit_instruction(instruction);
            }
            hir::Statement::Expr(expr_id) => {
                self.lower_expr(expr_id, expr_arena);
            }
            hir::Statement::Return(expr_id) => {
                let operand = self.lower_expr(expr_id, expr_arena);
                self.set_terminator(Terminator::Return(operand));
            }
        }
    }

    fn translate(self, func: hir::TypedFunction) -> Function {
        match func {
            hir::TypedFunction::Internal(internal) => {
                Function::Internal(self.translate_internal_func(internal))
            }
            hir::TypedFunction::External(external) => {
                Function::External(self.translate_external_func(external))
            }
        }
    }

    fn translate_signature(&mut self, signature: hir::FunctionSignature) -> FunctionSignature {
        let return_type = MIRType::from_hir(signature.ret_ty.node);
        let return_type_id = self.module.type_arena.get_or_insert(return_type);

        let args = signature
            .args
            .into_iter()
            .map(|arg| {
                let ty = MIRType::from_hir(arg.node.ty.node);
                let type_id = self.module.type_arena.get_or_insert(ty);
                let value_id = self.next_vreg();
                self.metainfo
                    .add_variable_name(arg.node.name.node, value_id);
                (type_id, value_id)
            })
            .collect();

        FunctionSignature {
            id: signature.id,
            name: signature.name.node.clone(),
            ret_ty: return_type_id,
            args,
        }
    }

    fn emit_store_instructions_for_args(
        &mut self,
        hir_signature: &hir::FunctionSignature,
        mir_signature: &FunctionSignature,
    ) {
        for (hir_arg, (_, value_id)) in hir_signature.args.iter().zip(mir_signature.args.iter()) {
            let ptr = *self.stack_slot_ptrs.get(&hir_arg.node.local_id).unwrap();
            let instruction = Instruction::Store {
                value: Operand::Use(*value_id),
                ptr: Operand::Use(ptr),
            };
            self.emit_instruction(instruction);
        }
    }

    fn translate_internal_func(mut self, func: hir::TypedInternalFunction) -> InternalFunction {
        let hir_signature = func.signature;

        for local in func.body.locals {
            // Alloca for each local variable
            let ty = MIRType::from_hir(local.ty.node);
            let type_id = self.module.type_arena.get_or_insert(ty);
            let stack_ptr = self.next_vreg();

            self.metainfo
                .add_variable_name(format!("{}_ptr", local.name.node), stack_ptr);

            let rvalue = RValue::Alloca(type_id);
            let instruction = Instruction::Assign(stack_ptr, rvalue);
            self.emit_instruction(instruction);
            self.stack_slot_ptrs.insert(local.id, stack_ptr);
        }

        let signature = self.translate_signature(hir_signature.clone());
        self.emit_store_instructions_for_args(&hir_signature, &signature);

        for statement in func.body.statements {
            self.lower_statement(statement, &func.expr_arena);
        }

        InternalFunction {
            signature,
            blocks: self.blocks,
            metainfo: self.metainfo,
        }
    }

    fn translate_external_func(mut self, func: hir::ExternalFunction) -> ExternalFunction {
        let signature = self.translate_signature(func.signature);
        ExternalFunction {
            signature,
            metainfo: self.metainfo,
        }
    }
}
