use std::collections::HashMap;

use diagnostic::{MaybeSpanned, Span, Spanned};

use crate::error::Error;
use crate::hir::{
    Block, Expr, ExprArena, ExprKind, Function, FunctionSignature, HIRLocal, HIRType, Statement,
};
use crate::ids::{ExprId, FuncId, LocalId, TypeVarId};

use super::solver::Solver;
use super::type_var::{Constraint, TyClass, TypeVar};

/// For brevity
type OT = Option<S<HIRType>>;
type S<T> = Spanned<T>;
type MaybeS<T> = MaybeSpanned<T>;

/// Temporary function context for generating type constraints
pub struct FunctionCtx<'a> {
    next_type_var_id: TypeVarId,
    constraints: Vec<Constraint>, // TODO remove
    func: &'a Function<OT>,
    used_func_signatures: HashMap<FuncId, FunctionSignature>,
    expr_ty: HashMap<ExprId, S<TypeVar>>,
    local_ty: HashMap<LocalId, S<TypeVar>>,
}

impl<'a> FunctionCtx<'a> {
    pub fn from_function(func: &'a Function<OT>) -> Self {
        let mut ctx = Self {
            next_type_var_id: TypeVarId::one(),
            constraints: Vec::new(),
            func,
            used_func_signatures: HashMap::new(),
            expr_ty: HashMap::new(),
            local_ty: HashMap::new(),
        };

        // Expr arena

        for expr in func.expr_arena.0.iter() {
            ctx.add_expr(expr);
        }

        // Block

        for local in &func.body.locals {
            ctx.add_hir_local(local);
        }

        for stmt in func.body.statements.iter() {
            match stmt {
                Statement::VariableDeclaration(var_decl) => {
                    let expr_ty = ctx.ty_of_expr(var_decl.expr_id).clone();
                    let local_ty = ctx.ty_of_local(var_decl.local_id).clone();
                    ctx.constraints.push(Constraint::Eq(expr_ty, local_ty));
                }
                _ => {}
            }
        }

        ctx
    }

    // In the future should be `with_imports` or `with_externals`
    pub fn with_signatures(self, func_signatures: HashMap<FuncId, FunctionSignature>) -> Self {
        Self {
            used_func_signatures: func_signatures,
            ..self
        }
    }

    pub fn into_solver(mut self) -> (Solver, Vec<S<Error>>) {
        let mut constraints = self.expr_arena_to_constraints(&self.func.expr_arena);

        let return_span = self
            .func
            .signature
            .ret_ty
            .span
            .unwrap_or_else(|| self.func.signature.name.span);
        let block_constraints = self.block_to_constraints(
            &self.func.body,
            S::new(
                TypeVar::Known(self.func.signature.ret_ty.clone()),
                return_span,
            ),
        );
        constraints.extend(block_constraints);

        Solver::new(constraints, self.expr_ty, self.local_ty)
    }

    fn block_to_constraints(
        &mut self,
        block: &Block<OT>,
        return_type_var: S<TypeVar>,
    ) -> Vec<Constraint> {
        for local in &block.locals {
            self.add_hir_local(local);
        }

        let mut constraints = Vec::<Constraint>::new();
        for stmt in block.statements.iter() {
            match stmt {
                Statement::VariableDeclaration(var_decl) => {
                    let expr_ty = self.ty_of_expr(var_decl.expr_id).clone();
                    let local_ty = self.ty_of_local(var_decl.local_id).clone();
                    constraints.push(Constraint::Eq(expr_ty, local_ty));
                }
                Statement::Return(expr_id) => {
                    let expr_ty = self.ty_of_expr(*expr_id).clone();
                    constraints.push(Constraint::Eq(expr_ty, return_type_var.clone()));
                }
                _ => {}
            }
        }
        constraints
    }

    fn expr_arena_to_constraints(&mut self, arena: &ExprArena<OT>) -> Vec<Constraint> {
        let mut constraints = Vec::<Constraint>::new();
        for expr in arena.0.iter() {
            let expr_type_var = self.ty_of_expr(expr.id).clone();
            match &expr.kind {
                ExprKind::Literal(lit) => match lit {
                    ast::Literal::Integer(_) => {
                        constraints.push(Constraint::InClass(expr_type_var, TyClass::Int))
                    }
                    ast::Literal::Float(_) => {
                        constraints.push(Constraint::InClass(expr_type_var, TyClass::Float))
                    }
                },
                ExprKind::Local(lid) => {
                    let local_ty = self.ty_of_local(*lid).clone();
                    constraints.push(Constraint::Eq(expr_type_var, local_ty));
                }
                ExprKind::FunctionCall(func_call) => {
                    let signature = self
                        .used_func_signatures
                        .get(&func_call.func_id)
                        .unwrap()
                        .clone();
                    let return_type_var = TypeVar::Known(signature.ret_ty.clone());
                    let return_type_var_spanned = Spanned::new(return_type_var, expr.span);
                    constraints.push(Constraint::Eq(expr_type_var, return_type_var_spanned));

                    for (i, arg_expr_id) in func_call.args.iter().enumerate() {
                        let arg_expr_ty_var_spanned = self.ty_of_expr(*arg_expr_id).clone();
                        let param_ty = signature.args[i].ty.clone();
                        let param_ty_var = TypeVar::Known(param_ty.clone().into_maybe());
                        let param_ty_var_spanned = Spanned::new(param_ty_var, param_ty.span);
                        constraints.push(Constraint::Eq(
                            arg_expr_ty_var_spanned,
                            param_ty_var_spanned,
                        ));
                    }
                }
                ExprKind::Block(block) => {
                    let block_constraints = self.block_to_constraints(block, expr_type_var);
                    constraints.extend(block_constraints);
                }
            }
        }
        constraints
    }

    fn fresh_type_var(&mut self, span: Span) -> S<TypeVar> {
        let id = self.next_type_var_id;
        self.next_type_var_id = self.next_type_var_id.increment();
        S::new(TypeVar::Var(id), span)
    }

    fn ty_of_local(&mut self, lid: LocalId) -> &S<TypeVar> {
        self.local_ty.get(&lid).unwrap()
    }

    fn add_hir_local(&mut self, local: &HIRLocal<OT>) -> S<TypeVar> {
        if let Some(ty) = self.local_ty.get(&local.id) {
            ty.clone()
        } else {
            let span = local.name.span;
            let ty = local
                .ty
                .clone()
                .map(|t| t.into_maybe())
                .map(TypeVar::Known)
                .map(|t| Spanned::new(t, span))
                .unwrap_or_else(|| self.fresh_type_var(span));
            self.local_ty.insert(local.id, ty.clone());
            ty
        }
    }

    fn ty_of_expr(&mut self, eid: ExprId) -> &S<TypeVar> {
        self.expr_ty.get(&eid).unwrap()
    }

    fn add_expr(&mut self, expr: &S<Expr<OT>>) -> S<TypeVar> {
        if let Some(ty) = self.expr_ty.get(&expr.id) {
            ty.clone()
        } else {
            let span = expr.span;
            let ty = expr
                .ty
                .clone()
                .map(|t| t.into_maybe())
                .map(TypeVar::Known)
                .map(|t| Spanned::new(t, span))
                .unwrap_or_else(|| self.fresh_type_var(span));
            self.expr_ty.insert(expr.id, ty.clone());
            ty
        }
    }
}
