use std::collections::HashMap;

use bimap::BiHashMap;

use hir::{THIR, TypeArena as HIRTypeArena};
use utils::ids::{ExprId, HIRTypeId, LocalId, MIRTypeId, ValueId};
use utils::primitive_types::PrimitiveType;

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
        let hir_type_arena = hir.type_arena;
        for item in hir.items {
            match item {
                hir::Item::Function(func) => {
                    let func = self.translate_function(func, &hir_type_arena);
                    self.mir.modules[0].functions.push(func);
                }
            }
        }
        self.mir
    }

    fn translate_function(
        &mut self,
        func: hir::TypedFunction,
        hir_type_arena: &HIRTypeArena,
    ) -> Function {
        let translator = HIRFunctionTranslator::new(&mut self.mir.modules[0], hir_type_arena);
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
    hir_type_arena: &'a HIRTypeArena,
}

impl<'a> HIRFunctionTranslator<'a> {
    fn new(module: &'a mut Module, hir_type_arena: &'a HIRTypeArena) -> Self {
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
            hir_type_arena,
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

    fn is_void_type(&self, type_id: MIRTypeId) -> bool {
        matches!(
            self.module.type_arena.get(type_id),
            Some(MIRType::Primitive(PrimitiveType::Void))
        )
    }

    fn lower_hir_type_id(&mut self, id: HIRTypeId) -> MIRTypeId {
        let hir_ty = self
            .hir_type_arena
            .get_by_id(id)
            .expect("HIRTypeId not found in arena. Internal error.")
            .clone();
        let mir_ty = MIRType::from_hir(hir_ty);
        self.module.type_arena.get_or_insert(mir_ty)
    }

    fn lower_expr_value(&mut self, expr_id: ExprId, expr_arena: &hir::TypedExprArena) -> Operand {
        let expr = expr_arena.get(expr_id).unwrap().as_ref();
        let type_id = self.lower_hir_type_id(expr.ty.node);

        match &expr.kind {
            hir::TypedExprKind::Literal(lit) => match lit {
                hir::Literal::Bool(v) => Operand::Constant(Constant::bool(type_id, *v)),
                hir::Literal::Integer(v) => Operand::Constant(Constant::int(type_id, v.clone())),
                hir::Literal::Float(v) => Operand::Constant(Constant::float(type_id, v.clone())),
                hir::Literal::Void => Operand::Constant(Constant::void(type_id)),
            },
            hir::TypedExprKind::FunctionCall(hir_call) => {
                if self.is_void_type(type_id) {
                    panic!("void expression cannot be lowered in value context. Internal error.");
                }

                let value_id = self.next_vreg();

                let args = hir_call
                    .args
                    .iter()
                    .map(|arg_expr_id| self.lower_expr_value(*arg_expr_id, expr_arena))
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

    fn lower_expr_stmt(&mut self, expr_id: ExprId, expr_arena: &hir::TypedExprArena) {
        let expr = expr_arena.get(expr_id).unwrap().as_ref();

        match &expr.kind {
            hir::TypedExprKind::FunctionCall(hir_call) => {
                let args = hir_call
                    .args
                    .iter()
                    .map(|arg_expr_id| self.lower_expr_value(*arg_expr_id, expr_arena))
                    .collect();

                self.emit_instruction(Instruction::Call(Call {
                    function: hir_call.func_id,
                    args,
                }));
            }
            // Expression statements without side effects should not materialize vregs.
            hir::TypedExprKind::Literal(_) | hir::TypedExprKind::Local(_) => {}
            _ => {
                unimplemented!(
                    "Statement expression lowering not implemented for: {:?}",
                    expr.kind
                );
            }
        }
    }

    fn lower_statement(&mut self, statement: hir::Statement, expr_arena: &hir::TypedExprArena) {
        match statement {
            hir::Statement::VariableDeclaration(decl) => {
                let stack_ptr = *self.stack_slot_ptrs.get(&decl.local_id).unwrap();
                let operand = self.lower_expr_value(decl.expr_id, expr_arena);

                let instruction = Instruction::Store {
                    value: operand,
                    ptr: Operand::Use(stack_ptr),
                };
                self.emit_instruction(instruction);
            }
            hir::Statement::Expr(expr_id) => {
                self.lower_expr_stmt(expr_id, expr_arena);
            }
            hir::Statement::Return(expr_id) => {
                let operand = self.lower_expr_value(expr_id, expr_arena);
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
        let return_type_id = self.lower_hir_type_id(signature.ret_ty.node);

        let args = signature
            .args
            .into_iter()
            .map(|arg| {
                let type_id = self.lower_hir_type_id(arg.node.ty.node);
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
            let type_id = self.lower_hir_type_id(local.ty.node);
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
