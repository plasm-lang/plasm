use std::borrow::Cow;
use std::collections::HashMap;

use bimap::BiHashMap;
use hir::{HIRTypeArena, THIR};
use utils::ids::{ExprId, HIRTypeId, LocalId, MIRTypeId, ValueId};
use utils::primitive_types::PrimitiveType;

use super::mir::{
    BasicBlock, BlockLabel, Call, Constant, ExternalFunction, Function,
    FunctionSignature, Instruction, InternalFunction, MIR, MetaInfo, Module,
    Operand, RValue, Terminator,
};
use super::types::{MIRType, MIRTypeArena};

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
                    type_arena: MIRTypeArena::new(),
                }],
            },
        }
    }

    fn translate(mut self, hir: THIR) -> MIR {
        self.mir.modules[0].funcs_map = hir.funcs_map;
        let hir_type_arena = hir.type_arena;
        self.register_types(&hir_type_arena);
        for item in hir.items {
            match item {
                hir::Item::Function(func) => {
                    let func = self.translate_function(func, &hir_type_arena);
                    self.mir.modules[0].functions.push(func);
                }
                hir::Item::TypeDefinition(def) => {
                    let name = def.name.node;
                    let hir_ty =
                        hir_type_arena.get_by_id(def.ty.node).unwrap().clone();
                    let mir_ty = MIRType::from_hir(&hir_ty, &hir_type_arena);

                    match mir_ty {
                        MIRType::Named(_, tuple) | MIRType::Tuple(tuple) => {
                            self.mir.modules[0].type_arena.insert_named(name, tuple);
                        }
                        _ => {}
                    }
                }
            }
        }
        self.mir
    }

    fn register_types(&mut self, hir_type_arena: &HIRTypeArena) {
        for ty in hir_type_arena.types.right_values() {
            let mir_ty = MIRType::from_hir(ty, hir_type_arena);
            self.mir.modules[0].type_arena.insert(mir_ty);
        }
    }

    fn translate_function(
        &mut self,
        func: hir::TypedFunction,
        hir_type_arena: &HIRTypeArena,
    ) -> Function {
        let translator =
            HIRFunctionTranslator::new(&mut self.mir.modules[0], hir_type_arena);
        translator.translate(func)
    }
}

struct HIRFunctionTranslator<'a> {
    vreg_counter: ValueId,
    blocks: Vec<BasicBlock>,
    current_block_idx: usize,
    metainfo: MetaInfo,
    stack_slot_ptrs: HashMap<LocalId, ValueId>,
    pending_allocas: Vec<Instruction>,
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
            pending_allocas: Vec::new(),
            module,
            hir_type_arena,
        }
    }

    fn next_vreg(&mut self) -> ValueId {
        let id = self.vreg_counter;
        self.vreg_counter = self.vreg_counter.increment();
        id
    }

    fn alloca(&mut self, type_id: MIRTypeId) -> ValueId {
        let ptr = self.next_vreg();
        self.pending_allocas
            .push(Instruction::Assign(ptr, RValue::Alloca(type_id)));
        ptr
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

    fn emit_gep(
        &mut self,
        type_id: MIRTypeId,
        ptr: ValueId,
        index: usize,
        name: String,
    ) -> ValueId {
        let field_ptr = self.next_vreg();
        self.metainfo.add_variable_name(name, field_ptr);
        self.emit_instruction(Instruction::Assign(
            field_ptr,
            RValue::GetElementPtr {
                type_id,
                ptr,
                index,
            },
        ));
        field_ptr
    }

    fn set_terminator(&mut self, terminator: Terminator) {
        self.blocks[self.current_block_idx].terminator = terminator;
    }

    fn is_void_type(&self, type_id: MIRTypeId) -> bool {
        matches!(
            self.module.type_arena.get_by_id(type_id),
            Some(MIRType::Primitive(PrimitiveType::Void))
        )
    }

    fn lower_hir_type_id(&mut self, id: HIRTypeId) -> MIRTypeId {
        let hir_ty = self.hir_type_arena.get_by_id(id).unwrap();
        let mir_ty = MIRType::from_hir(hir_ty, self.hir_type_arena);
        self.module.type_arena.get_or_insert(mir_ty)
    }

    fn lower_expr_value(
        &mut self,
        expr_id: ExprId,
        expr_arena: &hir::TypedExprArena,
    ) -> Operand {
        let expr = expr_arena.get(expr_id).unwrap().as_ref();
        let type_id = self.lower_hir_type_id(expr.ty.node);

        /// Helper to build a value name from a pointer name.
        /// For example:
        /// `pos_ptr` -> `pos`,
        /// `pos_ptr.x_ptr_1` -> `pos_ptr.x`,
        /// `literal_tmp_ptr` -> `literal_tmp`.
        fn ptr_string_to_val_string(s: Cow<'_, str>) -> String {
            let s = s.as_ref();
            if let Some(base) = s.strip_suffix("_ptr") {
                return base.to_string();
            }
            if let Some((head, idx)) = s.rsplit_once('_')
                && !idx.is_empty()
                && idx.chars().all(|c| c.is_ascii_digit())
                && head.ends_with("_ptr")
            {
                return head.trim_end_matches("_ptr").to_string();
            }
            format!("{s}_val")
        }

        match &expr.kind {
            hir::TypedExprKind::Literal(lit) => match lit {
                hir::Literal::Bool(v) => {
                    Operand::Constant(Constant::bool(type_id, *v))
                }
                hir::Literal::Integer(v) => {
                    Operand::Constant(Constant::int(type_id, v.clone()))
                }
                hir::Literal::Float(v) => {
                    Operand::Constant(Constant::float(type_id, v.clone()))
                }
                hir::Literal::Void => Operand::Constant(Constant::void(type_id)),
            },
            hir::TypedExprKind::FunctionCall(hir_call) => {
                if self.is_void_type(type_id) {
                    panic!(
                        "void expression cannot be lowered in value context. \
                         Internal error."
                    );
                }

                let args = hir_call
                    .args
                    .iter()
                    .map(|arg_expr_id| {
                        self.lower_expr_value(*arg_expr_id, expr_arena)
                    })
                    .collect();

                let rvalue = RValue::Call(Call {
                    function: hir_call.func_id,
                    args,
                });

                let value_id = self.next_vreg();
                let func_name = self
                    .module
                    .funcs_map
                    .get_by_left(&hir_call.func_id)
                    .unwrap()
                    .node
                    .clone();
                let metainfo_name = format!("{}_res", func_name);
                self.metainfo.add_variable_name(metainfo_name, value_id);

                self.emit_instruction(Instruction::Assign(value_id, rvalue));
                Operand::Use(value_id)
            }
            hir::TypedExprKind::Local(local_id) => {
                let stack_ptr = *self.stack_slot_ptrs.get(local_id).unwrap();
                let rvalue = RValue::Load(type_id, stack_ptr);
                let value_id = self.next_vreg();
                let metainfo_name = ptr_string_to_val_string(
                    self.metainfo.get_variable_name(stack_ptr),
                );
                self.metainfo.add_variable_name(metainfo_name, value_id);
                self.emit_instruction(Instruction::Assign(value_id, rvalue));
                Operand::Use(value_id)
            }
            hir::TypedExprKind::StructLiteral(_)
            | hir::TypedExprKind::FieldAccess(_) => {
                let ptr = self.lower_expr_place(expr_id, expr_arena);
                let res = self.next_vreg();
                let metainfo_name =
                    ptr_string_to_val_string(self.metainfo.get_variable_name(ptr));
                self.metainfo.add_variable_name(metainfo_name, res);
                self.emit_instruction(Instruction::Assign(
                    res,
                    RValue::Load(type_id, ptr),
                ));
                Operand::Use(res)
            }
            _ => {
                unimplemented!("Expression kind not supported yet: {:?}", expr.kind)
            }
        }
    }

    fn lower_expr_into_place(
        &mut self,
        expr_id: ExprId,
        destination: ValueId,
        expr_arena: &hir::TypedExprArena,
    ) {
        let expr = expr_arena.get(expr_id).unwrap().as_ref();

        match &expr.kind {
            hir::TypedExprKind::StructLiteral(lit) => {
                self.lower_struct_literal_into_place(
                    expr.ty.node,
                    lit,
                    destination,
                    expr_arena,
                );
            }
            _ => {
                let type_id = self.lower_hir_type_id(expr.ty.node);
                if self.is_void_type(type_id) {
                    self.lower_expr_stmt(expr_id, expr_arena);
                    return;
                }
                let operand = self.lower_expr_value(expr_id, expr_arena);
                self.emit_instruction(Instruction::Store {
                    value: operand,
                    ptr: Operand::Use(destination),
                });
            }
        }
    }

    fn lower_struct_literal_into_place(
        &mut self,
        hir_type_id: HIRTypeId,
        literal: &hir::StructLiteral,
        destination_ptr: ValueId,
        expr_arena: &hir::TypedExprArena,
    ) {
        let mir_type_id = self.lower_hir_type_id(hir_type_id);
        let struct_type = self.hir_struct_type(hir_type_id).clone();

        let lit_fields: HashMap<&str, ExprId> = literal
            .fields
            .iter()
            .map(|f| (f.name.node.as_str(), f.value))
            .collect();

        for (index, field) in struct_type.fields.iter().enumerate() {
            let field_ptr = self.emit_gep(
                mir_type_id,
                destination_ptr,
                index,
                format!(
                    "{}.{}_ptr",
                    self.metainfo.get_variable_name(destination_ptr),
                    field.name.node,
                ),
            );
            self.lower_expr_into_place(
                lit_fields[field.name.node.as_str()],
                field_ptr,
                expr_arena,
            );
        }
    }

    fn lower_expr_place(
        &mut self,
        expr_id: ExprId,
        expr_arena: &hir::TypedExprArena,
    ) -> ValueId {
        let expr = expr_arena.get(expr_id).unwrap().as_ref();
        match &expr.kind {
            hir::TypedExprKind::Local(local_id) => {
                *self.stack_slot_ptrs.get(local_id).unwrap()
            }
            hir::TypedExprKind::FieldAccess(access) => {
                let base_ptr = self.lower_expr_place(access.base, expr_arena);
                let base_expr = expr_arena.get(access.base).unwrap().as_ref();
                let base_type_id = self.lower_hir_type_id(base_expr.ty.node);
                let index =
                    self.field_index(base_expr.ty.node, &access.field_name.node);
                self.emit_gep(
                    base_type_id,
                    base_ptr,
                    index,
                    format!(
                        "{}.{}_ptr",
                        self.metainfo.get_variable_name(base_ptr),
                        access.field_name.node
                    ),
                )
            }
            _ => {
                let type_id = self.lower_hir_type_id(expr.ty.node);
                let tmp_ptr = self.alloca(type_id);
                let hint = match &expr.kind {
                    hir::TypedExprKind::StructLiteral(_) => {
                        "literal_tmp_ptr".to_string()
                    }
                    hir::TypedExprKind::FunctionCall(_) => {
                        "call_tmp_ptr".to_string()
                    }
                    _ => "tmp_ptr".to_string(),
                };
                self.metainfo.add_variable_name(hint, tmp_ptr);
                self.lower_expr_into_place(expr_id, tmp_ptr, expr_arena);
                tmp_ptr
            }
        }
    }

    fn field_index(&self, hir_type_id: HIRTypeId, field_name: &str) -> usize {
        self.hir_struct_type(hir_type_id)
            .fields
            .iter()
            .position(|f| f.name.node == field_name)
            .expect("field not found; internal typecheck bug")
    }

    fn hir_struct_type(&self, hir_type_id: HIRTypeId) -> &hir::StructType {
        match self
            .hir_type_arena
            .get_by_id(hir_type_id)
            .unwrap()
            .peel_named()
        {
            hir::HIRType::Struct(s) => s,
            other => {
                unreachable!("InternalError: Expected struct type, got {:?}", other)
            }
        }
    }

    fn lower_expr_stmt(
        &mut self,
        expr_id: ExprId,
        expr_arena: &hir::TypedExprArena,
    ) {
        let expr = expr_arena.get(expr_id).unwrap().as_ref();

        match &expr.kind {
            hir::TypedExprKind::FunctionCall(hir_call) => {
                let args = hir_call
                    .args
                    .iter()
                    .map(|arg_expr_id| {
                        self.lower_expr_value(*arg_expr_id, expr_arena)
                    })
                    .collect();

                self.emit_instruction(Instruction::Call(Call {
                    function: hir_call.func_id,
                    args,
                }));
            }
            hir::TypedExprKind::StructLiteral(lit) => {
                for field in &lit.fields {
                    self.lower_expr_stmt(field.node.value, expr_arena);
                }
            }
            hir::TypedExprKind::FieldAccess(access) => {
                self.lower_expr_stmt(access.base, expr_arena);
            }
            // Expression statements without side effects should not materialize
            // vregs.
            hir::TypedExprKind::Literal(_) | hir::TypedExprKind::Local(_) => {}
            _ => {
                unimplemented!(
                    "Statement expression lowering not implemented for: {:?}",
                    expr.kind
                );
            }
        }
    }

    fn lower_statement(
        &mut self,
        statement: hir::Statement,
        expr_arena: &hir::TypedExprArena,
    ) {
        match statement {
            hir::Statement::VariableDeclaration(decl) => {
                let destination = *self.stack_slot_ptrs.get(&decl.local_id).unwrap();
                self.lower_expr_into_place(decl.expr_id, destination, expr_arena);
            }
            hir::Statement::Assignment(lhs, rhs) => {
                let destination = self.lower_expr_place(lhs, expr_arena);
                self.lower_expr_into_place(rhs, destination, expr_arena);
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

    fn translate_signature(
        &mut self,
        signature: hir::FunctionSignature,
    ) -> FunctionSignature {
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
            name: signature.name.node,
            ret_ty: return_type_id,
            args,
        }
    }

    fn emit_store_instructions_for_args(
        &mut self,
        hir_signature: &hir::FunctionSignature,
        mir_signature: &FunctionSignature,
    ) {
        for (hir_arg, (_, value_id)) in
            hir_signature.args.iter().zip(mir_signature.args.iter())
        {
            let ptr = *self.stack_slot_ptrs.get(&hir_arg.node.local_id).unwrap();
            let instruction = Instruction::Store {
                value: Operand::Use(*value_id),
                ptr: Operand::Use(ptr),
            };
            self.emit_instruction(instruction);
        }
    }

    fn translate_internal_func(
        mut self,
        func: hir::TypedInternalFunction,
    ) -> InternalFunction {
        let hir_signature = func.signature;

        let hir::ExprKind::Block(func_body) =
            func.expr_arena.get(func.body).unwrap().node.kind.clone()
        else {
            unreachable!(
                "Impossible Invariant: function body must be a block expression."
            );
        };

        for local in func_body.locals {
            // Alloca for each local variable
            let type_id = self.lower_hir_type_id(local.ty.node);

            let stack_ptr = self.alloca(type_id);

            self.metainfo
                .add_variable_name(format!("{}_ptr", local.name.node), stack_ptr);
            self.stack_slot_ptrs.insert(local.id, stack_ptr);
        }

        let signature = self.translate_signature(hir_signature.clone());
        self.emit_store_instructions_for_args(&hir_signature, &signature);

        for statement in func_body.statements {
            self.lower_statement(statement, &func.expr_arena);
        }

        let allocas = std::mem::take(&mut self.pending_allocas);
        // TODO: Consider change Vec to VecDeque to avoid shifting elements when
        // inserting at the front.
        self.blocks[0].instructions.splice(0..0, allocas);

        InternalFunction {
            signature,
            blocks: self.blocks,
            metainfo: self.metainfo,
        }
    }

    fn translate_external_func(
        mut self,
        func: hir::ExternalFunction,
    ) -> ExternalFunction {
        let signature = self.translate_signature(func.signature);
        ExternalFunction {
            signature,
            metainfo: self.metainfo,
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::parse;
    use hir::ast_to_hir;
    use indoc::indoc;
    use tokenizer::tokenize;

    use super::*;

    fn check_by_display(code: &str, expected_display: &str) {
        let (ast, parse_errors) = parse(&mut tokenize(code.char_indices()));
        assert!(
            parse_errors.is_empty(),
            "Parsing errors: {:?}",
            parse_errors
        );
        let (hir, translation_errors) = ast_to_hir(ast);
        assert!(
            translation_errors.is_empty(),
            "HIR translation errors: {:?}",
            translation_errors
        );
        let mir = hir_to_mir(hir);
        assert_eq!(mir.to_string(), expected_display,);
    }

    #[test]
    fn test_external_function() {
        let code = indoc! {"
            fn add(a: I32, b: I32) -> I32
        "};
        let expected = indoc! {"
            fn add(%a: I32, %b: I32) -> I32
        "};
        check_by_display(code, expected);
    }

    #[test]
    fn test_named_type_definitions() {
        let code = indoc! {"
            type Pos = struct {
                x: I32,
                y: I32,
            }

            type Pos2 = Pos

            type Id = U32

            type Id2 = Id

            type Point = struct {
                pos: Pos,
                z: I32,
            }
        "};
        let expected = indoc! {"
            type Pos = (I32, I32)
            type Pos2 = (I32, I32)
            type Point = (%Pos, I32)
        "};
        check_by_display(code, expected);
    }

    #[test]
    fn test_struct_literal() {
        let code = indoc! {"
            type Pos = struct {
                x: I32,
                y: I32,
            }

            fn main() {
                let pos2d: Pos = { x: 1, y: 2 }
                let pos3d = { x: 1, y: 2, z: 3 }
            }
        "};
        let expected = indoc! {"
            type Pos = (I32, I32)

            fn main() -> Void {
                entry {
                    %pos2d_ptr = alloca %Pos
                    %pos3d_ptr = alloca (I32, I32, I32)
                    %pos2d_ptr.x_ptr = getelementptr %Pos, ptr %pos2d_ptr, index 0
                    store I32 1, ptr %pos2d_ptr.x_ptr
                    %pos2d_ptr.y_ptr = getelementptr %Pos, ptr %pos2d_ptr, index 1
                    store I32 2, ptr %pos2d_ptr.y_ptr
                    %pos3d_ptr.x_ptr = getelementptr (I32, I32, I32), ptr %pos3d_ptr, index 0
                    store I32 1, ptr %pos3d_ptr.x_ptr
                    %pos3d_ptr.y_ptr = getelementptr (I32, I32, I32), ptr %pos3d_ptr, index 1
                    store I32 2, ptr %pos3d_ptr.y_ptr
                    %pos3d_ptr.z_ptr = getelementptr (I32, I32, I32), ptr %pos3d_ptr, index 2
                    store I32 3, ptr %pos3d_ptr.z_ptr
                    return Void
                }
            }
        "};
        check_by_display(code, expected);
    }

    #[test]
    fn test_field_access() {
        let code = indoc! {"
            fn main() {
                let pos = { x: 1, y: 2 }
                pos.y = pos.x
            }
        "};
        let expected = indoc! {"
            fn main() -> Void {
                entry {
                    %pos_ptr = alloca (I32, I32)
                    %pos_ptr.x_ptr = getelementptr (I32, I32), ptr %pos_ptr, index 0
                    store I32 1, ptr %pos_ptr.x_ptr
                    %pos_ptr.y_ptr = getelementptr (I32, I32), ptr %pos_ptr, index 1
                    store I32 2, ptr %pos_ptr.y_ptr
                    %pos_ptr.y_ptr_1 = getelementptr (I32, I32), ptr %pos_ptr, index 1
                    %pos_ptr.x_ptr_1 = getelementptr (I32, I32), ptr %pos_ptr, index 0
                    %pos_ptr.x = load I32, ptr %pos_ptr.x_ptr_1
                    store %pos_ptr.x, ptr %pos_ptr.y_ptr_1
                    return Void
                }
            }
        "};
        check_by_display(code, expected);
    }
}
