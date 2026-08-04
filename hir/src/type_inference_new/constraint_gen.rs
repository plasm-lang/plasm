use std::collections::HashMap;

use diagnostic::Spanned;
use utils::ids::{ExprId, LocalId, TypeVarId};

use super::engine::ModuleCtx;
use super::type_class::TypeClass;
use super::type_var::InferType;
use crate::hir::{Block, ExprKind, HIRLocal, InternalFunction, OptTyped, Statement};

// For brevity
type S<T> = Spanned<T>;

pub fn generate_function_constraints(
    func: &InternalFunction<OptTyped>,
    module_ctx: &ModuleCtx,
) -> ConstraintSet {
    FunctionConstraintGen::new(func, module_ctx).generate_constraints()
}

#[derive(Debug, Default)]
pub struct ConstraintSet {
    pub constraints: Vec<Constraint>,
    pub maps: InferTypeMaps,
}

pub enum Constraint {
    Equality(S<InferType>, S<InferType>),
    InClass(S<InferType>, TypeClass),
    /// Generated for struct literals. It means that `InferType` must be a
    /// struct, must have the given fields with the given types wth the given
    /// names. The order of fields doesn't matter for the obligation
    /// checking, but matters in fallback case.
    StructShape(S<InferType>, Vec<(S<String>, S<InferType>)>),
    HasField {
        base: S<InferType>,
        field_name: S<String>,
        field_type: S<InferType>,
    },
}

impl std::fmt::Debug for Constraint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constraint::Equality(a, b) => write!(f, "{:?} == {:?}", a.node, b.node),
            Constraint::InClass(a, class) => write!(f, "{:?} ∈ {:?}", a.node, class),
            Constraint::StructShape(base, fields) => {
                let fields_str: String = fields
                    .iter()
                    .map(|(name, ty)| format!("{:?}: {:?}", name.node, ty.node))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{:?} === {{ {} }}", base.node, fields_str,)
            }
            Constraint::HasField {
                base,
                field_name,
                field_type,
            } => write!(
                f,
                "{:?} must have field {:?} of type {:?}",
                base.node, field_name.node, field_type.node
            ),
        }
    }
}

#[derive(Debug, Default)]
pub struct InferTypeMaps {
    pub expr_ty: HashMap<ExprId, S<InferType>>,
    pub local_ty: HashMap<LocalId, S<InferType>>,
}

struct FunctionConstraintGen<'a> {
    next_type_var_id: TypeVarId,
    result: ConstraintSet,
    func: &'a InternalFunction<OptTyped>,
    module_ctx: &'a ModuleCtx,
}

impl<'a> FunctionConstraintGen<'a> {
    pub fn new(
        func: &'a InternalFunction<OptTyped>,
        module_ctx: &'a ModuleCtx,
    ) -> Self {
        FunctionConstraintGen {
            next_type_var_id: TypeVarId::one(),
            result: ConstraintSet::default(),
            func,
            module_ctx,
        }
    }

    pub fn generate_constraints(mut self) -> ConstraintSet {
        let func_body_type_infer = self.process_expr(self.func.body);

        // Bind function return type with the type of the function body
        let return_type_infer = self
            .func
            .signature
            .ret_ty
            .map(InferType::Known)
            // If return span doesn't exist (it's void), use function name's span.
            .into_spanned_or(self.func.signature.name.span);
        let eq = Constraint::Equality(return_type_infer, func_body_type_infer);
        self.result.constraints.push(eq);

        self.result
    }

    fn process_expr(&mut self, expr_id: ExprId) -> S<InferType> {
        let expr = self.func.expr_arena.get(expr_id).unwrap();

        // If expression type id is known, get InferType using
        // `type_id_to_infer_type` saving type span.
        // If expression type id is unknown, generate a fresh type variable and save
        // it with expression span.
        let infer_type = match expr.ty {
            Some(s_ty) => s_ty.map(InferType::Known),
            None => S::new(self.fresh_type_var(), expr.span),
        };

        self.result.maps.expr_ty.insert(expr_id, infer_type);

        use ExprKind::*;
        match &expr.kind {
            // Literal types must belong to their corresponding type classes.
            Literal(ast::Literal::Void) => self
                .result
                .constraints
                .push(Constraint::InClass(infer_type, TypeClass::Void)),
            Literal(ast::Literal::Bool(_)) => self
                .result
                .constraints
                .push(Constraint::InClass(infer_type, TypeClass::Bool)),
            Literal(ast::Literal::Integer(_)) => self
                .result
                .constraints
                .push(Constraint::InClass(infer_type, TypeClass::Int)),
            Literal(ast::Literal::Float(_)) => self
                .result
                .constraints
                .push(Constraint::InClass(infer_type, TypeClass::Float)),

            Local(local_id) => {
                let local_infer_type =
                    *self.result.maps.local_ty.get(local_id).unwrap();
                let eq = Constraint::Equality(infer_type, local_infer_type);
                self.result.constraints.push(eq);
            }
            FunctionCall(func_call) => {
                let signature = self
                    .module_ctx
                    .func_signatures
                    .get(&func_call.func_id)
                    .unwrap()
                    .clone();

                // Bind the return type with the type of current expression.
                let return_infer_type = InferType::Known(signature.ret_ty.node);
                let return_span =
                    signature.ret_ty.span.unwrap_or(signature.name.span);
                let eq = Constraint::Equality(
                    infer_type,
                    S::new(return_infer_type, return_span),
                );
                self.result.constraints.push(eq);

                // Bind arguments.
                for (arg_expr_id, arg) in
                    func_call.args.iter().zip(signature.args.iter())
                {
                    let arg_expr_infer_type = self.process_expr(*arg_expr_id);

                    let arg_infer_type = arg.ty.map(InferType::Known);
                    let eq =
                        Constraint::Equality(arg_expr_infer_type, arg_infer_type);
                    self.result.constraints.push(eq);
                }
            }
            Block(block) => {
                let block_infer_type =
                    self.process_block(&S::new(block.clone(), expr.span));
                let eq = Constraint::Equality(infer_type, block_infer_type);
                self.result.constraints.push(eq);
            }
            StructLiteral(struct_literal) => {
                let mut infer_type_fields = Vec::new();
                for field in struct_literal.fields.iter() {
                    let field_infer_type = self.process_expr(field.value);
                    infer_type_fields.push((field.name.clone(), field_infer_type));
                }
                let constraint =
                    Constraint::StructShape(infer_type, infer_type_fields);
                self.result.constraints.push(constraint);
            }
            FieldAccess(field_access) => {
                let base_infer_type = self.process_expr(field_access.base);
                let constraint = Constraint::HasField {
                    base: base_infer_type,
                    field_name: field_access.field_name.clone(),
                    field_type: infer_type,
                };
                self.result.constraints.push(constraint);
            }
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
                    let local_infer_type =
                        *self.result.maps.local_ty.get(&var_decl.local_id).unwrap();
                    let eq = Constraint::Equality(expr_infer_type, local_infer_type);
                    self.result.constraints.push(eq);
                }
                Statement::Assignment(lhs, rhs) => {
                    let lhs_infer_type = self.process_expr(*lhs);
                    let rhs_infer_type = self.process_expr(*rhs);
                    let eq = Constraint::Equality(lhs_infer_type, rhs_infer_type);
                    self.result.constraints.push(eq);
                }
                Statement::Return(expr_id) => {
                    let expr_infer_type = self.process_expr(*expr_id);
                    let eq = Constraint::Equality(infer_type, expr_infer_type);
                    self.result.constraints.push(eq);
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
            return *infer_type;
        }

        let local_infer_type = match local.ty {
            Some(s_ty) => s_ty.map(InferType::Known),
            None => S::new(self.fresh_type_var(), local.name.span),
        };
        self.result.maps.local_ty.insert(local.id, local_infer_type);

        local_infer_type
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
