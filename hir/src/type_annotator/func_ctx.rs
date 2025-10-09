use std::collections::HashMap;

use diagnostic::{Span, Spanned};

use crate::error::Error;
use crate::hir::{
    Block, Expr, ExprArena, ExprKind, Function, FunctionSignature, HIRLocal, HIRType, Statement,
};
use crate::ids::{ExprId, FuncId, LocalId, TypeVarId};

use super::solver::Solver;
use super::type_var::{Constraint, TyClass, TypeVar};

// For brevity
type OT = Option<S<HIRType>>;
type S<T> = Spanned<T>;

/// Temporary function context for generating type constraints
pub struct FunctionCtx<'a> {
    next_type_var_id: TypeVarId,
    func: &'a Function<OT>,
    used_func_signatures: &'a HashMap<FuncId, FunctionSignature>,
    expr_ty: HashMap<ExprId, S<TypeVar>>,
    local_ty: HashMap<LocalId, S<TypeVar>>,
}

impl<'a> FunctionCtx<'a> {
    pub fn from_function(
        func: &'a Function<OT>,
        func_signatures: &'a HashMap<FuncId, FunctionSignature>,
    ) -> Self {
        let mut ctx = Self {
            next_type_var_id: TypeVarId::one(),
            func,
            used_func_signatures: func_signatures,
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

        ctx
    }

    pub fn into_solver(mut self) -> (Solver, Vec<S<Error>>) {
        let (mut constraints, constraints_errors) =
            self.expr_arena_to_constraints(&self.func.expr_arena);

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

        let (solver, solver_errors) = Solver::new(constraints, self.expr_ty, self.local_ty);

        (
            solver,
            constraints_errors
                .into_iter()
                .chain(solver_errors)
                .collect(),
        )
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

    fn expr_arena_to_constraints(
        &mut self,
        arena: &ExprArena<OT>,
    ) -> (Vec<Constraint>, Vec<S<Error>>) {
        let mut constraints = Vec::<Constraint>::new();
        let mut errors = Vec::<S<Error>>::new();
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
                    let return_type_var_spanned = S::new(return_type_var, expr.span);
                    constraints.push(Constraint::Eq(expr_type_var, return_type_var_spanned));

                    if func_call.args.len() != signature.args.len() {
                        let err = Error::ArgumentCountMismatch {
                            found: func_call.args.len(),
                            expected: signature.args.len(),
                        };
                        errors.push(S::new(err, expr.span));
                        continue;
                    }

                    for (i, arg_expr_id) in func_call.args.iter().enumerate() {
                        let arg_expr_ty_var_spanned = self.ty_of_expr(*arg_expr_id).clone();
                        let param_ty = signature.args[i].ty.clone();
                        let param_ty_var = TypeVar::Known(param_ty.clone().into_maybe());
                        let param_ty_var_spanned = S::new(param_ty_var, param_ty.span);
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
        (constraints, errors)
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
                .map(|t| S::new(t, span))
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
                .map(|t| S::new(t, span))
                .unwrap_or_else(|| self.fresh_type_var(span));
            self.expr_ty.insert(expr.id, ty.clone());
            ty
        }
    }
}

// --------------------- TEST HOOK (tests only) ---------------------
#[cfg(test)]
impl<'a> FunctionCtx<'a> {
    /// Collect all constraints for the function (arena + body) without creating a Solver.
    pub fn collect_constraints_for_tests(&mut self) -> (Vec<Constraint>, Vec<S<Error>>) {
        let (mut constraints, errors) = self.expr_arena_to_constraints(&self.func.expr_arena);

        let return_span = self
            .func
            .signature
            .ret_ty
            .span
            .unwrap_or_else(|| self.func.signature.name.span);

        let block_constraints = self.block_to_constraints(
            &self.func.body,
            Spanned::new(
                TypeVar::Known(self.func.signature.ret_ty.clone()),
                return_span,
            ),
        );
        constraints.extend(block_constraints);
        (constraints, errors)
    }
}

/// I generated these tests using ChatGPT as I'm a lazy bitch^-^
/// I checked it and it looks correct so I keep it. Just FYI.
#[cfg(test)]
mod tests {
    use super::*;
    use ast::ast::{Literal, PrimitiveType};
    use diagnostic::{Span, Spanned};
    use std::collections::HashMap;

    // ---------- helpers ----------

    fn s<T>(node: T) -> Spanned<T> {
        Spanned {
            node,
            span: Span { start: 0, end: 0 },
        }
    }

    fn ms<T: Clone>(node: T) -> diagnostic::MaybeSpanned<T> {
        diagnostic::MaybeSpanned { node, span: None }
    }

    fn i32_ty() -> HIRType {
        HIRType::Primitive(PrimitiveType::I32)
    }

    fn u8_ty() -> HIRType {
        HIRType::Primitive(PrimitiveType::U8)
    }

    fn void_ty() -> HIRType {
        HIRType::Primitive(PrimitiveType::Void)
    }

    fn local(
        id: LocalId,
        name: &str,
        ty: Option<Spanned<HIRType>>,
    ) -> HIRLocal<Option<Spanned<HIRType>>> {
        HIRLocal {
            id,
            ty,
            name: s(name.to_string()),
        }
    }

    fn expr_literal(eid: ExprId, lit: Literal) -> Spanned<Expr<OT>> {
        s(Expr {
            id: eid,
            ty: None,
            kind: ExprKind::Literal(lit),
        })
    }

    fn expr_local(eid: ExprId, lid: LocalId) -> Spanned<Expr<OT>> {
        s(Expr {
            id: eid,
            ty: None,
            kind: ExprKind::Local(lid),
        })
    }

    fn expr_call(eid: ExprId, fid: FuncId, args: Vec<ExprId>) -> Spanned<Expr<OT>> {
        s(Expr {
            id: eid,
            ty: None,
            kind: ExprKind::FunctionCall(crate::hir::FunctionCall { func_id: fid, args }),
        })
    }

    fn expr_block(eid: ExprId, block: Block<OT>) -> Spanned<Expr<OT>> {
        s(Expr {
            id: eid,
            ty: None,
            kind: ExprKind::Block(block),
        })
    }

    fn signature(
        id: FuncId,
        name: &str,
        params: Vec<(&str, HIRType)>,
        ret: HIRType,
    ) -> FunctionSignature {
        let args = params
            .into_iter()
            .enumerate()
            .map(|(i, (n, t))| {
                let lid = LocalId::new(std::num::NonZeroU32::new((i + 1) as u32).unwrap());
                s(crate::hir::Argument {
                    name: s(n.to_string()),
                    local_id: lid,
                    ty: s(t),
                })
            })
            .collect::<Vec<_>>();
        FunctionSignature {
            id,
            name: s(name.to_string()),
            args,
            ret_ty: ms(ret),
        }
    }

    fn make_func(
        signature: FunctionSignature,
        locals: Vec<HIRLocal<OT>>,
        stmts: Vec<Statement>,
        exprs: Vec<Spanned<Expr<OT>>>,
    ) -> Function<OT> {
        Function {
            signature,
            body: Block {
                locals,
                statements: stmts,
            },
            expr_arena: ExprArena(exprs),
        }
    }

    // Predicates without `matches!` guards:
    fn count_inclass(constraints: &[Constraint], class: TyClass) -> usize {
        let mut n = 0;
        for c in constraints {
            if let Constraint::InClass(_, c2) = c {
                if *c2 == class {
                    n += 1;
                }
            }
        }
        n
    }

    fn any_eq_known(constraints: &[Constraint], ty: &HIRType) -> bool {
        for c in constraints {
            if let Constraint::Eq(a, b) = c {
                match (&a.node, &b.node) {
                    (TypeVar::Known(k), _) if &k.node == ty => return true,
                    (_, TypeVar::Known(k)) if &k.node == ty => return true,
                    _ => {}
                }
            }
        }
        false
    }

    fn any_eq_var_var(constraints: &[Constraint]) -> bool {
        for c in constraints {
            if let Constraint::Eq(a, b) = c {
                if matches!((&a.node, &b.node), (TypeVar::Var(_), TypeVar::Var(_))) {
                    return true;
                }
            }
        }
        false
    }

    // ---------- tests ----------

    #[test]
    fn literal_int_assigned_to_annotated_local_produces_inclass_and_eq_to_known() {
        // fn main() { let a: i32 = 5 }
        let fid = FuncId::one();
        let sig = signature(fid, "main", vec![], void_ty());
        let lid_a = LocalId::one();
        let eid_lit = ExprId::one();

        let locals = vec![local(lid_a, "a", Some(s(i32_ty())))];
        let exprs = vec![expr_literal(eid_lit, Literal::Integer("5".to_string()))];
        let stmts = vec![Statement::VariableDeclaration(
            crate::hir::VariableDeclaration {
                local_id: lid_a,
                expr_id: eid_lit,
            },
        )];

        let fun = make_func(sig.clone(), locals, stmts, exprs);
        let mut sigs = HashMap::new();
        sigs.insert(sig.id, sig.clone());

        let mut ctx = FunctionCtx::from_function(&fun, &sigs);
        let (constraints, errors) = ctx.collect_constraints_for_tests();

        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
        assert_eq!(
            count_inclass(&constraints, TyClass::Int),
            1,
            "exactly one InClass(Int) for the integer literal expected"
        );
        assert!(
            any_eq_known(&constraints, &i32_ty()),
            "expected at least one Eq(_, Known(i32)) relating literal and annotated local"
        );
    }

    #[test]
    fn assignment_creates_eq_between_rhs_expr_var_and_lhs_local_var() {
        // fn main() { let a: i32 = 5; let b = a }
        let fid = FuncId::one();
        let sig = signature(fid, "main", vec![], void_ty());
        let lid_a = LocalId::one();
        let lid_b = LocalId::new(std::num::NonZeroU32::new(2).unwrap());
        let eid_lit = ExprId::one();
        let eid_use_a = ExprId::new(std::num::NonZeroU32::new(2).unwrap());

        let locals = vec![
            local(lid_a, "a", Some(s(i32_ty()))),
            local(lid_b, "b", None),
        ];
        let exprs = vec![
            expr_literal(eid_lit, Literal::Integer("5".to_string())),
            expr_local(eid_use_a, lid_a),
        ];
        let stmts = vec![
            Statement::VariableDeclaration(crate::hir::VariableDeclaration {
                local_id: lid_a,
                expr_id: eid_lit,
            }),
            Statement::VariableDeclaration(crate::hir::VariableDeclaration {
                local_id: lid_b,
                expr_id: eid_use_a,
            }),
        ];

        let fun = make_func(sig.clone(), locals, stmts, exprs);
        let mut sigs = HashMap::new();
        sigs.insert(sig.id, sig.clone());

        let mut ctx = FunctionCtx::from_function(&fun, &sigs);
        let (constraints, errors) = ctx.collect_constraints_for_tests();

        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
        assert!(
            any_eq_var_var(&constraints),
            "expected at least one Eq(Var, Var) for `b = a`"
        );
        assert_eq!(count_inclass(&constraints, TyClass::Int), 1);
        assert!(any_eq_known(&constraints, &i32_ty()));
    }

    #[test]
    fn return_equates_expr_with_function_return_type() {
        // fn main() -> i32 { let x = 1; return x }
        let fid = FuncId::one();
        let sig = signature(fid, "main", vec![], i32_ty());
        let lid_x = LocalId::one();
        let eid_lit = ExprId::one();
        let eid_use_x = ExprId::new(std::num::NonZeroU32::new(2).unwrap());

        let locals = vec![local(lid_x, "x", None)];
        let exprs = vec![
            expr_literal(eid_lit, Literal::Integer("1".to_string())),
            expr_local(eid_use_x, lid_x),
        ];
        let stmts = vec![
            Statement::VariableDeclaration(crate::hir::VariableDeclaration {
                local_id: lid_x,
                expr_id: eid_lit,
            }),
            Statement::Return(eid_use_x),
        ];

        let fun = make_func(sig.clone(), locals, stmts, exprs);
        let mut sigs = HashMap::new();
        sigs.insert(sig.id, sig.clone());

        let mut ctx = FunctionCtx::from_function(&fun, &sigs);
        let (constraints, errors) = ctx.collect_constraints_for_tests();

        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
        assert!(
            any_eq_known(&constraints, &i32_ty()),
            "expected Eq(_, Known(i32)) from `return` to the function return type"
        );
        assert_eq!(count_inclass(&constraints, TyClass::Int), 1);
    }

    #[test]
    fn function_call_yields_eq_to_ret_and_args_bound_to_param_types() {
        // print(x: i32) -> void; main { let a: i32 = 5; print(a) }
        let fid_print = FuncId::one();
        let sig_print = signature(fid_print, "print", vec![("x", i32_ty())], void_ty());
        let fid_main = FuncId::new(std::num::NonZeroU32::new(2).unwrap());
        let sig_main = signature(fid_main, "main", vec![], void_ty());

        let lid_a = LocalId::one();
        let eid_lit = ExprId::one();
        let eid_use_a = ExprId::new(std::num::NonZeroU32::new(2).unwrap());
        let eid_call = ExprId::new(std::num::NonZeroU32::new(3).unwrap());

        let locals = vec![local(lid_a, "a", Some(s(i32_ty())))];
        let exprs = vec![
            expr_literal(eid_lit, Literal::Integer("5".to_string())),
            expr_local(eid_use_a, lid_a),
            expr_call(eid_call, fid_print, vec![eid_use_a]),
        ];
        let stmts = vec![
            Statement::VariableDeclaration(crate::hir::VariableDeclaration {
                local_id: lid_a,
                expr_id: eid_lit,
            }),
            Statement::Expr(eid_call),
        ];

        let fun = make_func(sig_main.clone(), locals, stmts, exprs);
        let mut sigs = HashMap::new();
        sigs.insert(sig_print.id, sig_print.clone());
        sigs.insert(sig_main.id, sig_main.clone());

        let mut ctx = FunctionCtx::from_function(&fun, &sigs);
        let (constraints, errors) = ctx.collect_constraints_for_tests();

        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
        assert!(
            any_eq_known(&constraints, &void_ty()),
            "expected Eq(_, Known(void)) for call result"
        );
        assert!(
            any_eq_known(&constraints, &i32_ty()),
            "expected Eq(_, Known(i32)) for the parameter binding"
        );
    }

    #[test]
    fn function_call_arity_mismatch_reports_error_only_from_ctx() {
        // print(x: i32) -> void; main { print(1, 2) }
        let fid_print = FuncId::one();
        let sig_print = signature(fid_print, "print", vec![("x", i32_ty())], void_ty());
        let fid_main = FuncId::new(std::num::NonZeroU32::new(2).unwrap());
        let sig_main = signature(fid_main, "main", vec![], void_ty());

        let eid_lit1 = ExprId::one();
        let eid_lit2 = ExprId::new(std::num::NonZeroU32::new(2).unwrap());
        let eid_call = ExprId::new(std::num::NonZeroU32::new(3).unwrap());

        let locals: Vec<HIRLocal<OT>> = vec![];
        let exprs = vec![
            expr_literal(eid_lit1, Literal::Integer("1".to_string())),
            expr_literal(eid_lit2, Literal::Integer("2".to_string())),
            expr_call(eid_call, fid_print, vec![eid_lit1, eid_lit2]), // 2 args instead of 1
        ];
        let stmts = vec![Statement::Expr(eid_call)];

        let fun = make_func(sig_main.clone(), locals, stmts, exprs);
        let mut sigs = HashMap::new();
        sigs.insert(sig_print.id, sig_print.clone());
        sigs.insert(sig_main.id, sig_main.clone());

        let mut ctx = FunctionCtx::from_function(&fun, &sigs);
        let (_constraints, errors) = ctx.collect_constraints_for_tests();

        assert!(!errors.is_empty(), "arity mismatch error expected");
        let has_mismatch = errors
            .iter()
            .any(|e| matches!(e.node, Error::ArgumentCountMismatch { .. }));
        assert!(
            has_mismatch,
            "expected Error::ArgumentCountMismatch, got: {:?}",
            errors
        );
    }

    #[test]
    fn block_expression_flows_from_inner_return_constraint() {
        // y = { let t: u8 = 1; return t }  => Eq(type(block_expr), Known(u8))
        let fid = FuncId::one();
        let sig = signature(fid, "main", vec![], void_ty());

        let lid_y = LocalId::one();
        let lid_t = LocalId::new(std::num::NonZeroU32::new(2).unwrap());

        let eid_lit = ExprId::one();
        let eid_use_t = ExprId::new(std::num::NonZeroU32::new(2).unwrap());
        let eid_block = ExprId::new(std::num::NonZeroU32::new(3).unwrap());

        // inner block
        let inner_locals = vec![local(lid_t, "t", Some(s(u8_ty())))];
        // let inner_exprs = vec![
        //     // these ExprIds must be present in the *function* arena, but
        //     // they reference locals of the inner block
        //     expr_literal(eid_lit, Literal::Integer("1".to_string())),
        //     expr_local(eid_use_t, lid_t),
        // ];
        let inner_stmts = vec![
            Statement::VariableDeclaration(crate::hir::VariableDeclaration {
                local_id: lid_t,
                expr_id: eid_lit,
            }),
            Statement::Return(eid_use_t),
        ];
        let inner_block = Block {
            locals: inner_locals,
            statements: inner_stmts,
        };

        // outer: let y = <block>
        let outer_locals = vec![local(lid_y, "y", None)];

        // IMPORTANT: put the Block expr *before* inner exprs so that
        // block_to_constraints runs first and registers inner locals.
        let all_exprs = vec![
            expr_block(eid_block, inner_block), // visit this first
            expr_literal(eid_lit, Literal::Integer("1".to_string())),
            expr_local(eid_use_t, lid_t),
        ];

        let stmts = vec![Statement::VariableDeclaration(
            crate::hir::VariableDeclaration {
                local_id: lid_y,
                expr_id: eid_block,
            },
        )];

        let fun = make_func(sig.clone(), outer_locals, stmts, all_exprs);
        let mut sigs = HashMap::new();
        sigs.insert(sig.id, sig.clone());

        let mut ctx = FunctionCtx::from_function(&fun, &sigs);
        let (constraints, errors) = ctx.collect_constraints_for_tests();

        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
        assert!(
            any_eq_known(&constraints, &u8_ty()),
            "expected Eq(_, Known(u8)) for block result type"
        );
        assert_eq!(count_inclass(&constraints, TyClass::Int), 1);
    }

    #[test]
    fn float_literal_adds_inclass_float_not_int() {
        // let f = 1.0  => InClass(Float); no InClass(Int)
        let fid = FuncId::one();
        let sig = signature(fid, "main", vec![], void_ty());

        let lid_f = LocalId::one();
        let eid_lit = ExprId::one();

        let locals = vec![local(lid_f, "f", None)];
        let exprs = vec![expr_literal(eid_lit, Literal::Float("1.0".to_string()))];
        let stmts = vec![Statement::VariableDeclaration(
            crate::hir::VariableDeclaration {
                local_id: lid_f,
                expr_id: eid_lit,
            },
        )];

        let fun = make_func(sig.clone(), locals, stmts, exprs);
        let mut sigs = HashMap::new();
        sigs.insert(sig.id, sig.clone());

        let mut ctx = FunctionCtx::from_function(&fun, &sigs);
        let (constraints, errors) = ctx.collect_constraints_for_tests();

        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
        assert_eq!(
            count_inclass(&constraints, TyClass::Float),
            1,
            "expected exactly one InClass(Float)"
        );
        assert_eq!(
            count_inclass(&constraints, TyClass::Int),
            0,
            "should not produce InClass(Int)"
        );
    }
}
