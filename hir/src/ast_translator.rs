use std::collections::HashMap;

use diagnostic::{MaybeSpanned, Span, Spanned};
use utils::bin_op::BinaryOp;
use utils::ids::{ExprId, FuncId, HIRTypeId, LocalId};
use utils::primitive_types::PrimitiveType;

use super::error::Error;
use super::hir::{
    Argument, Block, Expr, ExprArena, ExprKind, ExternalFunction, FieldAccess,
    Function, FunctionCall, FunctionSignature, HIRLocal, InternalFunction, Item,
    OptHIR, Statement, StructLiteral, StructLiteralField, THIR, TypeDefinition,
    VariableDeclaration,
};
use super::type_inference::{TypeInferenceError, opt_hir_to_t_hir};
use super::types::{HIRType, StructField, StructType};

/// For brevity
type OT = Option<S<HIRTypeId>>;
type S<T> = Spanned<T>;
type MaybeS<T> = MaybeSpanned<T>;

pub fn ast_to_hir(ast: ast::AST) -> (THIR, Vec<S<Error>>) {
    let (opt_hir, translation_errors) = ast_to_opt_hir(ast);

    if !translation_errors.is_empty() {
        return (THIR::empty(), translation_errors);
    }

    let (t_hir, annotation_errors) = opt_hir_to_t_hir(opt_hir);
    (t_hir, annotation_errors)
}

fn ast_to_opt_hir(ast: ast::AST) -> (OptHIR, Vec<S<Error>>) {
    let translator = ASTTranslator::new();
    translator.translate(ast)
}

struct ASTTranslator {
    hir: OptHIR,
    errors: Vec<S<Error>>,
    next_func_id: FuncId,
    next_local_id: LocalId,
    next_expr_id: ExprId,
    /// Raw AST types collected in the first pass, keyed by type name.
    type_defs_registry: HashMap<String, ast::Type>,
    /// Cache of Named type resolution results.
    /// Some(id) = successfully resolved, None = failed (circular or unknown).
    resolved_cache: HashMap<String, Option<HIRTypeId>>,
    // binary_functions: HardcodedBinaryFunctions,
}

impl ASTTranslator {
    pub fn new() -> Self {
        Self {
            hir: OptHIR::default(),
            errors: Vec::new(),
            next_func_id: FuncId::one(),
            next_local_id: LocalId::one(),
            next_expr_id: ExprId::one(),
            type_defs_registry: HashMap::new(),
            resolved_cache: HashMap::new(),
            // binary_functions: HardcodedBinaryFunctions::new(),
        }
    }

    fn get_next_func_id(&mut self) -> FuncId {
        let id = self.next_func_id;
        self.next_func_id = self.next_func_id.increment();
        id
    }

    fn get_next_local_id(&mut self) -> LocalId {
        let id = self.next_local_id;
        self.next_local_id = self.next_local_id.increment();
        id
    }

    fn get_next_expr_id(&mut self) -> ExprId {
        let id = self.next_expr_id;
        self.next_expr_id = self.next_expr_id.increment();
        id
    }

    fn get_binary_function(&mut self, op: BinaryOp, ty: PrimitiveType) -> FuncId {
        todo!()
        // if let Some(func_id) = self.binary_functions.get(op, ty) {
        //     func_id
        // } else {
        //     let func_id = self.get_next_func_id();
        //     let external_func = self.binary_functions.create(op, ty,
        // func_id);     self.hir
        //         .items
        //         .push(Item::Function(Function::External(external_func)));
        //     func_id
        // }
    }

    pub fn translate(mut self, ast: ast::AST) -> (OptHIR, Vec<S<Error>>) {
        // First pass: register root definitions
        for item in ast.items.iter() {
            match item {
                ast::Item::Function(func) => {
                    let id = self.get_next_func_id();

                    // Check for duplicate function definitions
                    if let Some(prev) =
                        self.hir.funcs_map.get_by_right(&func.signature().name)
                    {
                        let first = self
                            .hir
                            .funcs_map
                            .get_by_left(prev)
                            .cloned()
                            .expect("BiHashMap invariant broken");

                        let error = Error::FunctionMultipleDefinitions {
                            first,
                            second: func.signature().name.clone(),
                        };
                        self.errors.push(S::new(error, func.signature().name.span));
                        continue;
                    }

                    self.hir.funcs_map.insert(id, func.signature().name.clone());
                }
                ast::Item::TypeDefinition(ty_def) => {
                    self.type_defs_registry
                        .insert(ty_def.name.node.clone(), ty_def.ty.node.clone());
                }
            }
        }

        // Second pass: translate items
        for item in ast.items {
            match item {
                ast::Item::Function(func) => {
                    self.translate_function(func);
                }
                ast::Item::TypeDefinition(ty_def) => {
                    self.translate_type_definition(ty_def);
                }
            }
        }

        (self.hir, self.errors)
    }

    fn translate_type_definition(&mut self, ty_def: ast::TypeDefinition) {
        let name = ty_def.name.node.clone();
        // If already in cache (processed as a dependency of another type), skip to
        // avoid duplicate errors.
        if self.resolved_cache.contains_key(&name) {
            return;
        }
        let (ast_type, ast_type_span) = ty_def.ty.unwrap();
        let mut resolving = Vec::new();
        let Some((ty_id, _ty)) =
            self.translate_type(ast_type, ast_type_span, &mut resolving)
        else {
            return;
        };
        let hir_ty_def = TypeDefinition {
            name: ty_def.name,
            ty: S::new(ty_id, ast_type_span),
        };
        self.hir.items.push(Item::TypeDefinition(hir_ty_def));
    }

    fn translate_function(&mut self, func: ast::Function) {
        match func {
            ast::Function::Internal(func) => self.translate_internal_function(func),
            ast::Function::External(func) => self.translate_external_function(func),
        }
    }

    fn translate_signature(
        &mut self,
        signature: ast::FunctionSignature,
    ) -> (FunctionSignature, Vec<HIRLocal<OT>>) {
        let func_id = self
            .hir
            .funcs_map
            .get_by_right(&signature.name)
            .cloned()
            .unwrap();
        let ret_ty_id = match signature.return_type {
            None => MaybeS::new(self.hir.type_arena.void_id()),
            Some(ty_spanned) => {
                let span = ty_spanned.span;
                let ty = ty_spanned.node;
                let mut resolving = Vec::new();
                match self.translate_type(ty, span, &mut resolving) {
                    Some((id, _)) => MaybeS {
                        node: id,
                        span: Some(span),
                    },
                    None => MaybeS::new(self.hir.type_arena.void_id()),
                }
            }
        };

        let mut locals: Vec<HIRLocal<OT>> = Vec::new();

        // Translate arguments

        // TODO: Handle duplicate argument names
        let mut args = Vec::new();
        for ast_arg in signature.args.into_iter() {
            let (hir_arg, local) = self.translate_arg(ast_arg);
            args.push(hir_arg);
            locals.push(local);
        }

        let signature = FunctionSignature {
            id: func_id,
            name: signature.name,
            args,
            ret_ty: ret_ty_id,
        };

        (signature, locals)
    }

    fn translate_internal_function(&mut self, func: ast::InternalFunction) {
        let (signature, locals) = self.translate_signature(func.signature);

        // We wrap body's `Block` into `Expr::Block` because in current design
        // `self.translate_block` doesn't generate ExprId, it's
        // `self.translate_expr`'s responsibility, but `self.translate_expr` expects
        // `S<ast::Expr>`.
        let body_span = func
            .body
            .iter()
            .map(|stmt| stmt.span)
            .reduce(|acc, span| acc.join(span))
            .unwrap_or(signature.name.span);
        let opt_ret_ty = signature.ret_ty.into_option_spanned();
        let (body_expr_id, expr_arena) = self.translate_expr(
            S::new(ast::Expr::Block(func.body), body_span),
            &locals,
            opt_ret_ty,
        );

        let hir_func = Function::Internal(InternalFunction {
            signature,
            body: body_expr_id,
            expr_arena,
        });
        self.hir.items.push(Item::Function(hir_func));
    }

    fn translate_external_function(&mut self, func: ast::ExternalFunction) {
        let (signature, _) = self.translate_signature(func.signature);
        let hir_func = Function::External(ExternalFunction { signature });
        self.hir.items.push(Item::Function(hir_func));
    }

    fn translate_arg(
        &mut self,
        ast_arg: S<ast::Argument>,
    ) -> (S<Argument>, HIRLocal<OT>) {
        let local_id = self.get_next_local_id();
        let (ast_arg, arg_span) = ast_arg.unwrap();
        let ty_span = ast_arg.ty.span;
        let hir_ty: Option<S<HIRTypeId>> = {
            let mut resolving = Vec::new();
            self.translate_type(ast_arg.ty.node, ty_span, &mut resolving)
                .map(|(id, _)| S::new(id, ty_span))
        };
        let hir_ty_for_arg =
            hir_ty.unwrap_or_else(|| S::new(self.hir.type_arena.void_id(), ty_span));

        let hir_arg = Argument {
            name: ast_arg.name.clone(),
            local_id,
            ty: hir_ty_for_arg,
        };

        let local = HIRLocal {
            id: local_id,
            ty: hir_ty,
            name: ast_arg.name,
        };

        (S::new(hir_arg, arg_span), local)
    }

    fn translate_type(
        &mut self,
        ty: ast::Type,
        span: Span,
        resolving: &mut Vec<String>,
    ) -> Option<(HIRTypeId, HIRType)> {
        let hir_ty = match ty {
            ast::Type::Primitive(p) => HIRType::Primitive(p),
            ast::Type::Struct(s) => {
                let mut fields = Vec::new();
                for ast_field in s.fields.into_iter() {
                    let (ast_field, field_span) = ast_field.unwrap();
                    let (ast_field_ty, field_ty_span) = ast_field.ty.unwrap();
                    let (field_ty_id, _field_ty) =
                        self.translate_type(ast_field_ty, field_ty_span, resolving)?;
                    let field = StructField {
                        name: ast_field.name,
                        ty_id: S::new(field_ty_id, field_ty_span),
                    };
                    fields.push(S::new(field, field_span));
                }
                HIRType::Struct(StructType { fields })
            }
            ast::Type::Named(name) => {
                // Check resolution cache first
                if let Some(cached) = self.resolved_cache.get(&name).copied() {
                    return match cached {
                        Some(type_id) => {
                            let hir_ty = self
                                .hir
                                .type_arena
                                .get_by_id(type_id)
                                .unwrap()
                                .clone();
                            Some((type_id, hir_ty))
                        }
                        None => {
                            // Previously failed - report as unknown
                            self.errors.push(S::new(
                                Error::TypeInferenceError(
                                    TypeInferenceError::UnknownTypeName { name },
                                ),
                                span,
                            ));
                            None
                        }
                    };
                }

                // Not defined at all
                if !self.type_defs_registry.contains_key(&name) {
                    self.resolved_cache.insert(name.clone(), None);
                    self.errors.push(S::new(
                        Error::TypeInferenceError(
                            TypeInferenceError::UnknownTypeName { name },
                        ),
                        span,
                    ));
                    return None;
                }

                // TODO: Move cycle detection to a different structure or module.
                // Cycle detection
                if resolving.contains(&name) {
                    let start = resolving.iter().position(|n| n == &name).unwrap();
                    let cycle = resolving[start..].to_vec();
                    self.resolved_cache.insert(name, None);
                    self.errors.push(S::new(
                        Error::TypeInferenceError(
                            TypeInferenceError::CircularTypeDefinition { cycle },
                        ),
                        span,
                    ));
                    return None;
                }

                // Resolve recursively
                let ast_ty = self.type_defs_registry.get(&name).unwrap().clone();
                resolving.push(name.clone());
                let result = self.translate_type(ast_ty, span, resolving);
                resolving.pop();

                match result {
                    Some((_, underlying_hir)) => {
                        let hir_ty =
                            HIRType::Named(name.clone(), Box::new(underlying_hir));
                        let type_id =
                            self.hir.type_arena.get_or_insert(hir_ty.clone());
                        self.resolved_cache.insert(name, Some(type_id));
                        hir_ty
                    }
                    None => {
                        self.resolved_cache.insert(name, None);
                        return None;
                    }
                }
            }
        };
        let id = self.hir.type_arena.get_or_insert(hir_ty.clone());
        Some((id, hir_ty))
    }

    fn translate_block(
        &mut self,
        ast_block: ast::Block,
        locals: &[HIRLocal<OT>],
        opt_ty: OT,
    ) -> (Block<OT>, ExprArena<OT>) {
        let mut locals = locals.to_vec();
        let mut expr_arena = ExprArena::<OT>::default();
        let mut statements = Vec::new();
        for ast_stmt in ast_block.into_iter() {
            let hir_stmt = match ast_stmt.node {
                ast::Statement::VariableDeclaration(variable_declaration) => {
                    // Build HIRLocal
                    // TODO: Handle duplicate variable names
                    let local_id = self.get_next_local_id();
                    let name = variable_declaration.name.clone();
                    let opt_ty = variable_declaration.ty.and_then(|ty_spanned| {
                        let (ty, span) = ty_spanned.unwrap();
                        let mut resolving = Vec::new();
                        self.translate_type(ty, span, &mut resolving)
                            .map(|(id, _)| S::new(id, span))
                    });
                    let local = HIRLocal {
                        id: local_id,
                        ty: opt_ty,
                        name,
                    };
                    locals.push(local);

                    // Translate Expr and build VariableDeclaration
                    let (expr_id, local_expr_arena) = self.translate_expr(
                        variable_declaration.value,
                        &locals,
                        opt_ty,
                    );
                    expr_arena = expr_arena.join(local_expr_arena);

                    Statement::VariableDeclaration(VariableDeclaration {
                        local_id,
                        expr_id,
                    })
                }
                ast::Statement::Expr(expr) => {
                    let (expr_id, local_expr_arena) = self.translate_expr(
                        S::new(expr, ast_stmt.span),
                        &locals,
                        None,
                    );
                    expr_arena = expr_arena.join(local_expr_arena);
                    Statement::Expr(expr_id)
                }
                ast::Statement::Return(opt_expr) => {
                    let (expr_id, local_expr_arena) = if let Some(expr) = opt_expr {
                        self.translate_expr(expr, &locals, opt_ty)
                    } else {
                        let void_expr = ast::Expr::Literal(ast::Literal::Void);
                        self.translate_expr(
                            S::new(void_expr, ast_stmt.span),
                            &locals,
                            opt_ty,
                        )
                    };
                    expr_arena = expr_arena.join(local_expr_arena);
                    Statement::Return(expr_id)
                }
            };

            statements.push(hir_stmt);
        }

        // If return type is Void, ensure there is at least one return statement.
        // If not, add `return Void` at the end of the function.
        let is_void = opt_ty
            .map(|type_id| type_id.node == self.hir.type_arena.void_id())
            .unwrap_or(true);
        let has_return = statements
            .last()
            .map(|s| matches!(s, Statement::Return(_)))
            .unwrap_or(false);
        if is_void && !has_return {
            let expr_id = self.get_next_expr_id();
            let return_expr = Expr::<OT> {
                ty: None,
                kind: ExprKind::Literal(ast::Literal::Void),
            };
            // TODO: Replace stub zero span with actual span of the function
            // signature return type.
            let span = Span::zero();
            expr_arena.insert(expr_id, S::new(return_expr, span));
            statements.push(Statement::Return(expr_id));
        }

        let block = Block { locals, statements };

        (block, expr_arena)
    }

    fn translate_expr(
        &mut self,
        expr: S<ast::Expr>,
        locals: &Vec<HIRLocal<OT>>,
        opt_ty: OT,
    ) -> (ExprId, ExprArena<OT>) {
        let expr_id = self.get_next_expr_id();
        let mut expr_arena = ExprArena::<OT>::default();
        match expr.node {
            ast::Expr::Literal(lit) => {
                let hir_expr = Expr::<OT> {
                    ty: opt_ty,
                    kind: ExprKind::Literal(lit),
                };
                expr_arena.insert(expr_id, S::new(hir_expr, expr.span));
            }
            ast::Expr::Variable(var) => {
                // Look up local_id
                // TODO: Change Vec to HashMap or BiHashMap for efficiency
                let local_id = locals
                    .iter()
                    .find(|local| local.name.node == var)
                    .map(|local| local.id);

                if let Some(local_id) = local_id {
                    let hir_expr = Expr::<OT> {
                        ty: opt_ty,
                        kind: ExprKind::Local(local_id),
                    };
                    expr_arena.insert(expr_id, S::new(hir_expr, expr.span));
                } else {
                    let err = Error::UnknownVariable { name: var };
                    self.errors.push(S::new(err, expr.span));
                }
            }
            ast::Expr::FunctionCall(func_call) => {
                let (name, span) = func_call.name.unwrap();
                let func_id = self.hir.funcs_map.get_by_right(&name).cloned();
                if let Some(func_id) = func_id {
                    // Translate arguments into ExprIds
                    let mut args = Vec::new();
                    for ast_arg in func_call.args.into_iter() {
                        let (arg_expr_id, local_expr_arena) =
                            self.translate_expr(ast_arg.value, locals, None);
                        expr_arena = expr_arena.join(local_expr_arena);
                        args.push(arg_expr_id);
                    }

                    // Build FunctionCall and Expr
                    let func_call = FunctionCall { func_id, args };
                    let hir_expr = Expr::<OT> {
                        ty: opt_ty,
                        kind: ExprKind::FunctionCall(func_call),
                    };
                    expr_arena.insert(expr_id, S::new(hir_expr, expr.span));
                } else {
                    let err = Error::UnknownFunction { name };
                    self.errors.push(S::new(err, span));
                }
            }
            ast::Expr::Block(block) => {
                let (hir_block, local_expr_arena) =
                    self.translate_block(block, locals, opt_ty);
                expr_arena = expr_arena.join(local_expr_arena);
                let hir_expr = Expr::<OT> {
                    ty: opt_ty,
                    kind: ExprKind::Block(hir_block),
                };
                expr_arena.insert(expr_id, S::new(hir_expr, expr.span));
            }
            ast::Expr::Binary(bi_expr) => {
                let (left_expr_id, left_expr_arena) =
                    self.translate_expr(*bi_expr.left, locals, None);
                let (right_expr_id, right_expr_arena) =
                    self.translate_expr(*bi_expr.right, locals, None);
                expr_arena = expr_arena.join(left_expr_arena).join(right_expr_arena);

                // Translate BinaryOp into function call.
                // Any binary operation is represented as a common built-in function
                // ob HIR level. It's needed for consistency sake of
                // Traits.
                let ty = PrimitiveType::I32; // TODO: Infer type properly
                let func_id = self.get_binary_function(bi_expr.op, ty);
                let func_call = FunctionCall {
                    func_id,
                    args: vec![left_expr_id, right_expr_id],
                };
                let hir_expr = Expr::<OT> {
                    ty: opt_ty,
                    kind: ExprKind::FunctionCall(func_call),
                };
                expr_arena.insert(expr_id, S::new(hir_expr, expr.span));
            }
            ast::Expr::Unary(expr) => {
                todo!()
            }
            ast::Expr::StructLiteral(struct_lit) => {
                let mut fields = Vec::new();
                for ast_field in struct_lit.fields.into_iter() {
                    let (ast_field, field_span) = ast_field.unwrap();
                    let (ast_field_expr_id, local_expr_arena) =
                        self.translate_expr(ast_field.value, locals, None);
                    expr_arena = expr_arena.join(local_expr_arena);
                    let field = StructLiteralField {
                        name: ast_field.name,
                        value: ast_field_expr_id,
                    };
                    fields.push(S::new(field, field_span));
                }
                let struct_lit = StructLiteral { fields };
                let hir_expr = Expr::<OT> {
                    ty: opt_ty,
                    kind: ExprKind::StructLiteral(struct_lit),
                };
                expr_arena.insert(expr_id, S::new(hir_expr, expr.span));
            }
            ast::Expr::FieldAccess(field_access) => {
                let (struct_expr_id, struct_expr_arena) =
                    self.translate_expr(*field_access.base, locals, None);
                expr_arena = expr_arena.join(struct_expr_arena);
                let field_access = FieldAccess {
                    base: struct_expr_id,
                    field_name: field_access.field_name,
                };
                let hir_expr = Expr::<OT> {
                    ty: opt_ty,
                    kind: ExprKind::FieldAccess(field_access),
                };
                expr_arena.insert(expr_id, S::new(hir_expr, expr.span));
            }
        }
        (expr_id, expr_arena)
    }
}

// struct HardcodedBinaryFunctions {
//     funcs: HashMap<(BinaryOp, PrimitiveType), FuncId>,
// }

// impl HardcodedBinaryFunctions {
//     fn new() -> Self {
//         Self {
//             funcs: HashMap::new(),
//         }
//     }

//     fn get(&mut self, op: BinaryOp, ty: PrimitiveType) -> Option<FuncId> {
//         self.funcs.get(&(op, ty)).cloned()
//     }

//     fn create(&mut self, op: BinaryOp, ty: PrimitiveType, func_id: FuncId) ->
// ExternalFunction {         let name = format!("{}_{}", op.name(), ty);
//         let ret_ty = HIRType::Primitive(ty);

//         // Left argument
//         let left_arg_local_id = LocalId::one();
//         let left_arg = Argument {
//             name: S::zero("left".to_string()),
//             local_id: left_arg_local_id,
//             ty: S::zero(HIRType::Primitive(ty)),
//         };

//         // Right argument
//         let right_arg_local_id = left_arg_local_id.increment();
//         let right_arg = Argument {
//             name: S::zero("right".to_string()),
//             local_id: right_arg_local_id,
//             ty: S::zero(HIRType::Primitive(ty)),
//         };

//         let signature = FunctionSignature {
//             id: func_id,
//             name: S::zero(name),
//             args: vec![S::zero(left_arg), S::zero(right_arg)],
//             ret_ty: MaybeS::new(ret_ty),
//         };

//         self.funcs.insert((op, ty), func_id);

//         ExternalFunction { signature }
//     }
// }

#[cfg(test)]
mod tests {
    use ast::parse;
    use indoc::indoc;
    use tokenizer::tokenize;

    use super::*;

    fn check_by_display(code: &str, expected_display: &str) {
        let (ast, errors) = parse(&mut tokenize(code.char_indices()));
        assert!(errors.is_empty());
        let (hir, errors) = ast_to_hir(ast);
        assert!(errors.is_empty());
        assert_eq!(hir.to_string(), expected_display);
    }

    /// Parses `code`, runs ast_to_hir, and asserts that the error sub-types
    /// match `expected_subtypes` exactly (order-insensitive, duplicates
    /// counted).
    fn check_errors(code: &str, expected_subtypes: &[&str]) {
        use diagnostic::ErrorType;

        let (ast, parse_errors) = parse(&mut tokenize(code.char_indices()));
        assert!(parse_errors.is_empty(), "parse errors: {parse_errors:?}");

        let (_hir, errors) = ast_to_hir(ast);
        let mut got: Vec<&str> =
            errors.iter().map(|e| e.node.error_sub_type()).collect();
        got.sort_unstable();

        let mut expected: Vec<&str> = expected_subtypes.to_vec();
        expected.sort_unstable();

        assert_eq!(
            got, expected,
            "error sub-types mismatch\ngot:      {got:?}\nexpected: \
             {expected:?}\nfull errors: {errors:?}"
        );
    }

    #[test]
    fn basic_types_inference_test() {
        let code = indoc! {"
            fn print(x: I32) {}
            fn main() {
                let a = 5
                let b = a
                let c = b
                print(c)
            }"};
        let expected_hir_display = indoc! {"
            fn print(x: I32) -> Void {
                return Void
            }

            fn main() -> Void {
                let a: I32 = 5
                let b: I32 = a
                let c: I32 = b
                print(c)
                return Void
            }
        "};
        check_by_display(code, expected_hir_display);
    }

    #[test]
    fn bool_type_inference_test() {
        let code = indoc! {"
            fn is_true(x: Bool) {}
            fn main() {
                let a = true
                let b = a
                is_true(a)
                is_true(b)
                is_true(false)
            }"};
        let expected_hir_display = indoc! {"
            fn is_true(x: Bool) -> Void {
                return Void
            }

            fn main() -> Void {
                let a: Bool = true
                let b: Bool = a
                is_true(a)
                is_true(b)
                is_true(false)
                return Void
            }
        "};
        check_by_display(code, expected_hir_display);
    }

    #[test]
    fn void_type_inference_test() {
        let code = indoc! {"
            fn do_nothing() {}
            fn main() {
                let a = do_nothing()
                let b = a
            }"};
        let expected_hir_display = indoc! {"
            fn do_nothing() -> Void {
                return Void
            }

            fn main() -> Void {
                let a: Void = do_nothing()
                let b: Void = a
                return Void
            }
        "};
        check_by_display(code, expected_hir_display);
    }

    #[test]
    fn external_function_translation_test() {
        let code = indoc! {"
            fn print(x: I32)
            fn main() {
                print(5)
            }
        "};
        let expected_hir_display = indoc! {"
            fn print(x: I32) -> Void

            fn main() -> Void {
                print(5)
                return Void
            }
        "};
        check_by_display(code, expected_hir_display);
    }

    #[test]
    fn struct_type_definition_test() {
        let code = indoc! {"
            type Pos = struct {
                x: I32,
                y: I32,
            }
        "};
        let expected_hir_display = indoc! {"
            type Pos = struct {
                x: I32,
                y: I32,
            }
        "};
        check_by_display(code, expected_hir_display);
    }

    #[test]
    fn nested_struct_type_definition_test() {
        let code = indoc! {"
            type Transform = struct {
                pos: struct {
                    x: I32,
                    y: I32,
                },
            }
        "};
        let expected_hir_display = indoc! {"
            type Transform = struct {
                pos: struct {
                    x: I32,
                    y: I32,
                },
            }
        "};
        check_by_display(code, expected_hir_display);
    }

    #[test]
    fn struct_type_definition_with_function_test() {
        let code = indoc! {"
            fn func(arg: struct { x: I32, y: I32 }) {}
        "};
        let expected_hir_display = indoc! {"
            fn func(arg: struct {
                x: I32,
                y: I32,
            }) -> Void {
                return Void
            }
        "};
        check_by_display(code, expected_hir_display);
    }

    #[test]
    fn inline_struct_type_inference_test() {
        let code = indoc! {"
            fn main() {
                let a = { pos: { x: 1, y: 2 } }
                let b = a
            }
        "};
        let expected_hir_display = indoc! {"
            fn main() -> Void {
                let a: struct {
                    pos: struct {
                        x: I32,
                        y: I32,
                    },
                } = {
                    pos: {
                        x: 1,
                        y: 2,
                    },
                }
                let b: struct {
                    pos: struct {
                        x: I32,
                        y: I32,
                    },
                } = a
                return Void
            }
        "};
        check_by_display(code, expected_hir_display);
    }

    #[test]
    fn named_struct_literal_infers_named_type() {
        let code = indoc! {"
            type Pos = struct {
                x: I32,
                y: I32,
            }
            fn get_pos() -> Pos {
                let pos = { x: 5, y: 15 }
                return pos
            }
        "};
        let expected_hir_display = indoc! {"
            type Pos = struct {
                x: I32,
                y: I32,
            }

            fn get_pos() -> Pos {
                let pos: Pos = {
                    x: 5,
                    y: 15,
                }
                return pos
            }
        "};
        check_by_display(code, expected_hir_display);
    }

    #[test]
    fn chained_named_alias_resolves() {
        let code = indoc! {"
            type Id = U32
            type MyId = Id
            fn identity(x: MyId) -> MyId {
                return x
            }
        "};
        let expected_hir_display = indoc! {"
            type Id = U32

            type MyId = Id

            fn identity(x: MyId) -> MyId {
                return x
            }
        "};
        check_by_display(code, expected_hir_display);
    }

    #[test]
    fn unknown_type_name_is_error() {
        let code = indoc! {"
            fn f(x: Nonexistent) {}
        "};
        check_errors(code, &["UnknownTypeName"]);
    }

    #[test]
    fn circular_type_direct_is_error() {
        let code = indoc! {"
            type A = A
        "};
        check_errors(code, &["CircularTypeDefinition"]);
    }

    #[test]
    fn circular_type_indirect_is_error() {
        let code = indoc! {"
            type A = B
            type B = A
        "};
        check_errors(code, &["CircularTypeDefinition"]);
    }

    #[test]
    fn circular_type_does_not_register() {
        let code = indoc! {"
            type A = A
            fn f(x: A) {}
        "};
        // CircularTypeDefinition for the type def, UnknownTypeName for
        // the usage in `f`
        check_errors(code, &["CircularTypeDefinition", "UnknownTypeName"]);
    }

    #[test]
    fn field_access_on_struct_literal() {
        let code = indoc! {"
            fn main() {
                let a = { x: 5, y: 10 }
                let x = a.x
                let y = a.y
            }
        "};
        let expected_hir_display = indoc! {"
            fn main() -> Void {
                let a: struct {
                    x: I32,
                    y: I32,
                } = {
                    x: 5,
                    y: 10,
                }
                let x: I32 = a.x
                let y: I32 = a.y
                return Void
            }
        "};
        check_by_display(code, expected_hir_display);
    }

    #[test]
    fn struct_shape_fields_mismatch() {
        let code = indoc! {"
            type Pos = struct {
                x: I32,
                y: I32,
            }

            fn get_pos() -> Pos {
                return { x: true, y: 15 }
            }
        "};
        check_errors(code, &["IncompatibleTypeClass"]);
    }
}
