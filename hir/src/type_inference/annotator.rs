use std::collections::HashMap;

use diagnostic::{MaybeSpanned, Spanned};

use super::type_solver::Solution;
use crate::hir::{
    Block, Expr, ExprArena, ExprKind, HIRLocal, InternalFunction, OptTyped, Typed,
};
use crate::types::HIRTypeArena;

// For brevity
type S<T> = Spanned<T>;
type MS<T> = MaybeSpanned<T>;

pub fn annotate_function(
    func: InternalFunction<OptTyped>,
    solution: Solution,
    arena: &mut HIRTypeArena,
) -> InternalFunction<Typed> {
    FunctionAnnotator::new(solution).annotate(func, arena)
}

struct FunctionAnnotator {
    solution: Solution,
}

impl FunctionAnnotator {
    fn new(solution: Solution) -> Self {
        Self { solution }
    }

    fn annotate(
        mut self,
        in_func: InternalFunction<OptTyped>,
        arena: &mut HIRTypeArena,
    ) -> InternalFunction<Typed> {
        let annotated_expr_arena = self.annotate_expr_arena(in_func.expr_arena);
        InternalFunction {
            signature: in_func.signature,
            body: in_func.body,
            expr_arena: annotated_expr_arena,
        }
    }

    fn annotate_expr_arena(
        &mut self,
        expr_arena: ExprArena<OptTyped>,
    ) -> ExprArena<Typed> {
        let mut out_exprs = HashMap::with_capacity(expr_arena.0.len());
        for (expr_id, spanned_expr) in expr_arena.0.into_iter() {
            let (expr, span) = spanned_expr.unwrap();
            let ty = if let Some(ty) = expr.ty {
                ty.into_maybe()
            } else {
                *self.solution.expr_ty.get(&expr_id).unwrap()
            };
            let kind = match expr.kind {
                ExprKind::Block(block) => {
                    let block = self.annotate_block(block);
                    ExprKind::Block(block)
                }
                ExprKind::Literal(lit) => ExprKind::Literal(lit),
                ExprKind::Local(id) => ExprKind::Local(id),
                ExprKind::FunctionCall(call) => ExprKind::FunctionCall(call),
                ExprKind::StructLiteral(lit) => ExprKind::StructLiteral(lit),
                ExprKind::FieldAccess(field) => ExprKind::FieldAccess(field),
            };
            out_exprs.insert(expr_id, S::new(Expr { ty, kind }, span));
        }
        ExprArena(out_exprs)
    }

    fn annotate_block(&mut self, block: Block<OptTyped>) -> Block<Typed> {
        let mut locals = Vec::with_capacity(block.locals.len());
        for local in block.locals.into_iter() {
            let ty = if let Some(ty) = local.ty {
                ty.into_maybe()
            } else {
                *self.solution.local_ty.get(&local.id).unwrap()
            };
            let annotated_local = HIRLocal {
                id: local.id,
                name: local.name,
                ty,
            };
            locals.push(annotated_local);
        }
        Block {
            locals,
            statements: block.statements,
        }
    }
}
