use std::collections::HashMap;

use diagnostic::Spanned;
use utils::ids::{ExprId, LocalId};

use super::engine::ModuleCtx;
use super::type_class::TypeClass;
use super::type_var::{Binding, InferType, TypeVarId};
use crate::hir::{
    self, Block, ExprKind, FunctionSignature, HIRLocal, InternalFunction, OptTyped,
    OptTypedCallee, Statement,
};
use crate::types::HIRTypeArena;

// For brevity
type S<T> = Spanned<T>;

pub fn generate_function_constraints(
    func: &InternalFunction<OptTyped>,
    module_ctx: &ModuleCtx,
) -> ConstraintSet {
    FunctionConstraintGen::new(func, module_ctx).generate_constraints()
}

#[derive(Default)]
pub struct ConstraintSet {
    pub constraints: Vec<Constraint>,
    pub maps: InferTypeMaps,
}

pub enum Constraint {
    Equality(S<InferType>, S<InferType>),
    /// Generated for literals, adds knowledge about the type.
    Binding(S<InferType>, Binding),
    Access(Access),
}

/// Field or index access. It's frozen until the binding of the base type is
/// specific enough to find the accessed type.
pub enum Access {
    Field {
        base: S<InferType>,
        field_name: S<String>,
        field_type: S<InferType>,
    },
    Index {
        base: S<InferType>,
        index: S<usize>,
        element_type: S<InferType>,
    },
    Method {
        base: S<InferType>,
        expr_id: ExprId,
        method_name: S<String>,
        args: Vec<S<InferType>>,
        return_type: S<InferType>,
    },
}

impl Access {
    pub fn base(&self) -> S<InferType> {
        match self {
            Access::Field { base, .. }
            | Access::Index { base, .. }
            | Access::Method { base, .. } => *base,
        }
    }

    pub fn accessed_types(&self) -> Vec<S<InferType>> {
        match self {
            Access::Field { field_type, .. } => vec![*field_type],
            Access::Index { element_type, .. } => vec![*element_type],
            Access::Method {
                args, return_type, ..
            } => {
                let mut types = args.clone();
                types.push(*return_type);
                types
            }
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
            Literal(literal) => {
                let class = match literal {
                    ast::Literal::Void => TypeClass::Void,
                    ast::Literal::Bool(_) => TypeClass::Bool,
                    ast::Literal::Integer(_) => TypeClass::Int,
                    ast::Literal::Float(_) => TypeClass::Float,
                };
                let constraint =
                    Constraint::Binding(infer_type, Binding::Class(class));
                self.result.constraints.push(constraint);
            }

            Local(local_id) => {
                let local_infer_type =
                    *self.result.maps.local_ty.get(local_id).unwrap();
                let eq = Constraint::Equality(infer_type, local_infer_type);
                self.result.constraints.push(eq);
            }
            FunctionCall(hir::FunctionCall {
                callee: OptTypedCallee::Func(func_id),
                args,
            }) => {
                let signature =
                    self.module_ctx.func_signatures.get(func_id).unwrap();

                let arg_infer_types = args
                    .iter()
                    .map(|arg_expr_id| self.process_expr(*arg_expr_id));
                let constraints =
                    signature_eq_constraints(signature, arg_infer_types, infer_type);
                self.result.constraints.extend(constraints);
            }
            FunctionCall(hir::FunctionCall {
                callee: OptTypedCallee::Method(name),
                args,
            }) => {
                let base_infer_type = self.process_expr(args[0]);
                let arg_infer_types = args[1..]
                    .iter()
                    .map(|&arg_expr_id| self.process_expr(arg_expr_id))
                    .collect::<Vec<_>>();
                let return_infer_type = infer_type;
                let method_access = Access::Method {
                    base: base_infer_type,
                    expr_id,
                    method_name: name.clone(),
                    args: arg_infer_types,
                    return_type: return_infer_type,
                };
                let constraint = Constraint::Access(method_access);
                self.result.constraints.push(constraint);
            }
            Block(block) => {
                let block_infer_type =
                    self.process_block(&S::new(block.clone(), expr.span));
                let eq = Constraint::Equality(infer_type, block_infer_type);
                self.result.constraints.push(eq);
            }
            StructLiteral(struct_literal) => {
                let mut infer_type_fields =
                    Vec::with_capacity(struct_literal.fields.len());
                for field in struct_literal.fields.iter() {
                    let field_infer_type = self.process_expr(field.value);
                    infer_type_fields.push((field.name.clone(), field_infer_type));
                }
                let constraint = Constraint::Binding(
                    infer_type,
                    Binding::StructShape(infer_type_fields),
                );
                self.result.constraints.push(constraint);
            }
            TupleLiteral(tuple_literal) => {
                let mut infer_type_elements =
                    Vec::with_capacity(tuple_literal.0.len());
                for element_expr_id in tuple_literal.0.iter() {
                    let element_infer_type = self.process_expr(*element_expr_id);
                    infer_type_elements.push(element_infer_type);
                }
                let constraint = Constraint::Binding(
                    infer_type,
                    Binding::TupleShape(infer_type_elements),
                );
                self.result.constraints.push(constraint);
            }
            FieldAccess(field_access) => {
                let base_infer_type = self.process_expr(field_access.base);
                let constraint = Constraint::Access(Access::Field {
                    base: base_infer_type,
                    field_name: field_access.field_name.clone(),
                    field_type: infer_type,
                });
                self.result.constraints.push(constraint);
            }
            IndexAccess(index_access) => {
                let base_infer_type = self.process_expr(index_access.base);
                let constraint = Constraint::Access(Access::Index {
                    base: base_infer_type,
                    index: index_access.index,
                    element_type: infer_type,
                });
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

pub fn signature_eq_constraints(
    signature: &FunctionSignature,
    arg_infer_types: impl Iterator<Item = S<InferType>>,
    return_infer_type: S<InferType>,
) -> Vec<Constraint> {
    let mut constraints = Vec::new();

    let eq = Constraint::Equality(
        return_infer_type,
        S::new(
            InferType::Known(signature.ret_ty.node),
            signature.ret_ty.span.unwrap_or(signature.name.span),
        ),
    );
    constraints.push(eq);

    // Bind arguments.
    for (arg_infer_type, arg) in arg_infer_types.zip(signature.args.iter()) {
        let eq = Constraint::Equality(arg_infer_type, arg.ty.map(InferType::Known));
        constraints.push(eq);
    }

    constraints
}

// --- Debug Helpers --- //

impl Constraint {
    #[allow(dead_code)]
    #[cfg(debug_assertions)]
    pub fn format(&self, arena: &HIRTypeArena) -> String {
        match self {
            Constraint::Equality(a, b) => {
                format!("{} == {}", a.node.format(arena), b.node.format(arena))
            }
            Constraint::Binding(a, Binding::Known(type_id)) => {
                let ty = InferType::Known(*type_id);
                format!("{} == {}", a.node.format(arena), ty.format(arena))
            }
            Constraint::Binding(a, Binding::Class(class)) => {
                format!("{} ∈ {:?}", a.node.format(arena), class)
            }
            Constraint::Binding(base, Binding::StructShape(fields)) => {
                let fields_str: String = fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty.node.format(arena)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{} === {{ {} }}", base.node.format(arena), fields_str,)
            }
            Constraint::Binding(base, Binding::TupleShape(elements)) => {
                let elements_str: String = elements
                    .iter()
                    .map(|ty| ty.node.format(arena).to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{} === ( {} )", base.node.format(arena), elements_str,)
            }
            Constraint::Access(Access::Field {
                base,
                field_name,
                field_type,
            }) => format!(
                "{} must have field \"{}\" of type {}",
                base.node.format(arena),
                field_name,
                field_type.node.format(arena)
            ),
            Constraint::Access(Access::Index {
                base,
                index,
                element_type,
            }) => format!(
                "{} must have index {} of type {}",
                base.node.format(arena),
                index,
                element_type.node.format(arena)
            ),
            Constraint::Access(Access::Method {
                base,
                expr_id: _,
                method_name,
                args,
                return_type,
            }) => format!(
                "{} must have method \"{}\" with args ({}) returning type {}",
                base.node.format(arena),
                method_name,
                args.iter()
                    .map(|arg| arg.node.format(arena))
                    .collect::<Vec<_>>()
                    .join(", "),
                return_type.node.format(arena)
            ),
        }
    }
}
