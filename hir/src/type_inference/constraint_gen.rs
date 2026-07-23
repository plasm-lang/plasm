use std::collections::HashMap;

use diagnostic::Spanned;
use utils::ids::{ExprId, HIRTypeId, LocalId, TypeVarId};

use super::engine::ModuleCtx;
use super::type_class::TypeClass;
use super::type_var::InferType;
use crate::hir::{Block, ExprKind, HIRLocal, InternalFunction, OptTyped, Statement};
use crate::types::{HIRType, HIRTypeArena};

// For brevity
type S<T> = Spanned<T>;

pub fn generate_function_constraints(
    func: &InternalFunction<OptTyped>,
    module_ctx: &ModuleCtx,
    type_arena: &HIRTypeArena,
) -> Constraints {
    FunctionConstraintGen::new(func, module_ctx, type_arena).generate_constraints()
}

#[derive(Debug, Default)]
pub struct Constraints {
    pub equalities: Vec<Equality>,
    pub obligations: Vec<Obligation>,
    pub maps: InferTypeMaps,
}

#[derive(Debug)]
pub struct Equality(pub S<InferType>, pub S<InferType>);

#[derive(Debug)]
pub enum Obligation {
    InClass(S<InferType>, TypeClass),
    /// Generated for struct literals. It means that `InferType` must be a
    /// struct, must have the given fields with the given types wth the given
    /// names. The order of fields doesn't matter for the obligation
    /// checking, but matters in fallback case.
    StructShape(S<InferType>, Vec<(S<String>, S<InferType>)>),
}

#[derive(Debug, Default)]
pub struct InferTypeMaps {
    pub expr_ty: HashMap<ExprId, S<InferType>>,
    pub local_ty: HashMap<LocalId, S<InferType>>,
}

struct FunctionConstraintGen<'a> {
    next_type_var_id: TypeVarId,
    result: Constraints,
    func: &'a InternalFunction<OptTyped>,
    module_ctx: &'a ModuleCtx,
    type_arena: &'a HIRTypeArena,
}

impl<'a> FunctionConstraintGen<'a> {
    pub fn new(
        func: &'a InternalFunction<OptTyped>,
        module_ctx: &'a ModuleCtx,
        type_arena: &'a HIRTypeArena,
    ) -> Self {
        FunctionConstraintGen {
            next_type_var_id: TypeVarId::one(),
            result: Constraints::default(),
            func,
            module_ctx,
            type_arena,
        }
    }

    pub fn generate_constraints(mut self) -> Constraints {
        let func_body_type_infer = self.process_expr(self.func.body);

        // Bind function return type with the type of the function body
        let return_type_infer = self
            .func
            .signature
            .ret_ty
            .map(|type_id| self.type_id_to_infer_type(type_id))
            // If return span doesn't exist (it's void), use function name's span.
            .into_spanned_or(self.func.signature.name.span);
        let eq = Equality(return_type_infer, func_body_type_infer);
        self.result.equalities.push(eq);

        self.result
    }

    fn process_expr(&mut self, expr_id: ExprId) -> S<InferType> {
        let expr = self.func.expr_arena.get(expr_id).unwrap();

        // If expression type id is known, get InferType using
        // `type_id_to_infer_type` saving type span.
        // If expression type id is unknown, generate a fresh type variable and save
        // it with expression span.
        let infer_type = match expr.ty {
            Some(s_ty) => s_ty.map(|ty| self.type_id_to_infer_type(ty)),
            None => S::new(self.fresh_type_var(), expr.span),
        };

        self.result.maps.expr_ty.insert(expr_id, infer_type.clone());

        use ExprKind::*;
        match &expr.kind {
            // For literals add `Obligation::InClass`.
            Literal(ast::Literal::Void) => self
                .result
                .obligations
                .push(Obligation::InClass(infer_type.clone(), TypeClass::Void)),
            Literal(ast::Literal::Bool(_)) => self
                .result
                .obligations
                .push(Obligation::InClass(infer_type.clone(), TypeClass::Bool)),
            Literal(ast::Literal::Integer(_)) => self
                .result
                .obligations
                .push(Obligation::InClass(infer_type.clone(), TypeClass::Int)),
            Literal(ast::Literal::Float(_)) => self
                .result
                .obligations
                .push(Obligation::InClass(infer_type.clone(), TypeClass::Float)),

            Local(local_id) => {
                let local_infer_type =
                    self.result.maps.local_ty.get(local_id).unwrap().clone();
                let eq = Equality(infer_type.clone(), local_infer_type);
                self.result.equalities.push(eq);
            }
            FunctionCall(func_call) => {
                let signature = self
                    .module_ctx
                    .func_signatures
                    .get(&func_call.func_id)
                    .unwrap()
                    .clone();

                // Bind the return type with the type of current expression.
                let return_infer_type =
                    self.type_id_to_infer_type(signature.ret_ty.node);
                let return_span =
                    signature.ret_ty.span.unwrap_or(signature.name.span);
                let eq = Equality(
                    infer_type.clone(),
                    S::new(return_infer_type, return_span),
                );
                self.result.equalities.push(eq);

                // Bind arguments.
                for (arg_expr_id, arg) in
                    func_call.args.iter().zip(signature.args.iter())
                {
                    let arg_expr_infer_type = self.process_expr(*arg_expr_id);

                    let arg_infer_type =
                        arg.ty.map(|ty| self.type_id_to_infer_type(ty));
                    let eq = Equality(arg_expr_infer_type, arg_infer_type);
                    self.result.equalities.push(eq);
                }
            }
            Block(block) => {
                let block_infer_type =
                    self.process_block(&S::new(block.clone(), expr.span));
                let eq = Equality(infer_type.clone(), block_infer_type);
                self.result.equalities.push(eq);
            }
            StructLiteral(struct_literal) => {
                let mut infer_type_fields = Vec::new();
                for field in struct_literal.fields.iter() {
                    let field_infer_type = self.process_expr(field.value);
                    infer_type_fields.push((field.name.clone(), field_infer_type));
                }
                let obligation =
                    Obligation::StructShape(infer_type.clone(), infer_type_fields);
                self.result.obligations.push(obligation);
            }
            FieldAccess(field_access) => todo!(),
        };
        infer_type
    }

    fn process_block(&mut self, block: &S<Block<OptTyped>>) -> S<InferType> {
        let infer_type = S::new(self.fresh_type_var(), block.span);

        for local in block.locals.iter() {
            self.process_local(local);
        }

        for stmt in block.statements.iter() {
            match stmt {
                Statement::VariableDeclaration(var_decl) => {
                    let expr_infer_type = self.process_expr(var_decl.expr_id);
                    let local_infer_type = self
                        .result
                        .maps
                        .local_ty
                        .get(&var_decl.local_id)
                        .unwrap()
                        .clone();
                    let eq = Equality(expr_infer_type, local_infer_type);
                    self.result.equalities.push(eq);
                }
                Statement::Return(expr_id) => {
                    let expr_infer_type = self.process_expr(*expr_id);
                    let eq = Equality(infer_type.clone(), expr_infer_type);
                    self.result.equalities.push(eq);
                }
                Statement::Expr(expr_id) => {
                    self.process_expr(*expr_id);
                }
            }
        }

        infer_type
    }

    fn process_local(&mut self, local: &HIRLocal<OptTyped>) -> S<InferType> {
        // Check if local is already processed.
        if let Some(infer_type) = self.result.maps.local_ty.get(&local.id) {
            return infer_type.clone();
        }

        let local_infer_type = match local.ty {
            Some(s_ty) => s_ty.map(|ty| self.type_id_to_infer_type(ty)),
            None => S::new(self.fresh_type_var(), local.name.span),
        };
        self.result
            .maps
            .local_ty
            .insert(local.id, local_infer_type.clone());

        local_infer_type
    }

    fn type_id_to_infer_type(&mut self, type_id: HIRTypeId) -> InferType {
        let ty_opt = self.type_arena.get_by_id(type_id);

        let Some(ty) = ty_opt else {
            return self.fresh_type_var();
        };

        match ty {
            HIRType::Primitive(_) => InferType::Scalar(type_id),
            HIRType::Struct(struct_type) => {
                let infer_type_fields = struct_type
                    .fields
                    .iter()
                    .map(|field| {
                        let name = field.name.clone();
                        let infer_type = field
                            .ty_id
                            .map(|ty_id| self.type_id_to_infer_type(ty_id));
                        (name, infer_type)
                    })
                    .collect();
                InferType::Struct(infer_type_fields)
            }
            HIRType::Named(_name, _sub_type) => {
                todo!()
            }
        }
    }

    fn next_type_var_id(&mut self) -> TypeVarId {
        let id = self.next_type_var_id;
        self.next_type_var_id = self.next_type_var_id.increment();
        id
    }

    fn fresh_type_var(&mut self) -> InferType {
        InferType::Var(self.next_type_var_id())
    }
}
