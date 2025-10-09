use std::collections::HashMap;

use diagnostic::{MaybeSpanned, Spanned};

use crate::error::Error;
use crate::hir::{
    Block, Expr, ExprArena, ExprKind, Function, HIRLocal, HIRType, Item, OptHIR, THIR,
};

use super::func_ctx::FunctionCtx;
use super::solver::Solver;

// For brevity
type OT = Option<S<HIRType>>;
type S<T> = Spanned<T>;

pub fn opt_hir_to_t_hir(opt_hir: OptHIR) -> (THIR, Vec<S<Error>>) {
    let annotator = TypeAnnotator::new();
    annotator.annotate(opt_hir)
}

pub struct TypeAnnotator {}

impl TypeAnnotator {
    pub fn new() -> Self {
        TypeAnnotator {}
    }

    pub fn annotate(&self, opt_hir: OptHIR) -> (THIR, Vec<S<Error>>) {
        let mut out_items = Vec::new();
        let mut errors = Vec::new();

        let signatures = opt_hir
            .items
            .iter()
            .filter_map(|item| {
                if let Item::Function(func) = item {
                    Some((func.signature.id, func.signature.clone()))
                } else {
                    None
                }
            })
            .collect::<HashMap<_, _>>();

        for item in opt_hir.items {
            match item {
                Item::Function(func) => {
                    let (solver, constraint_errors) =
                        FunctionCtx::from_function(&func, &signatures).into_solver();
                    errors.extend(constraint_errors);

                    let annotator = FunctionAnnotator::new(solver);
                    let (annotated_func, func_errors) = annotator.annotate(func);
                    out_items.push(Item::Function(annotated_func));
                    errors.extend(func_errors);
                }
            }
        }

        (THIR { items: out_items }, errors)
    }
}

struct FunctionAnnotator {
    solver: Solver,
    errors: Vec<S<Error>>,
}

impl FunctionAnnotator {
    fn new(solver: Solver) -> Self {
        Self {
            solver,
            errors: Vec::new(),
        }
    }

    fn annotate(mut self, func: Function<OT>) -> (Function<MaybeSpanned<HIRType>>, Vec<S<Error>>) {
        let arena = self.annotate_expr_arena(func.expr_arena);
        let block = self.annotate_block(func.body);

        (
            Function {
                signature: func.signature,
                body: block,
                expr_arena: arena,
            },
            self.errors,
        )
    }

    fn annotate_expr_arena(&mut self, arena: ExprArena<OT>) -> ExprArena<MaybeSpanned<HIRType>> {
        let mut out_exprs = Vec::with_capacity(arena.0.len());
        for expr in arena.0.into_iter() {
            let (expr, span) = expr.unwrap();
            let ty = if let Some(ty) = expr.ty {
                ty.into_maybe()
            } else {
                match self.solver.resolve_expr(expr.id) {
                    Ok(t) => t,
                    Err(e) => {
                        self.errors.push(e);
                        continue;
                    }
                }
            };
            let kind = match expr.kind {
                ExprKind::Block(block) => {
                    let annotated_block = self.annotate_block(block);
                    ExprKind::Block(annotated_block)
                }
                ExprKind::Literal(lit) => ExprKind::Literal(lit),
                ExprKind::Local(lid) => ExprKind::Local(lid),
                ExprKind::FunctionCall(func_call) => ExprKind::FunctionCall(func_call),
            };
            out_exprs.push(S::new(
                Expr {
                    id: expr.id,
                    kind,
                    ty,
                },
                span,
            ));
        }
        ExprArena(out_exprs)
    }

    fn annotate_block(&mut self, block: Block<OT>) -> Block<MaybeSpanned<HIRType>> {
        let locals = block
            .locals
            .into_iter()
            .filter_map(|local| {
                let ty = if let Some(ty) = local.ty {
                    ty.into_maybe()
                } else {
                    match self.solver.resolve_local(local.id) {
                        Ok(t) => t,
                        Err(e) => {
                            self.errors.push(e);
                            return None;
                        }
                    }
                };
                Some(HIRLocal {
                    id: local.id,
                    name: local.name,
                    ty,
                })
            })
            .collect();
        Block {
            locals,
            statements: block.statements,
        }
    }
}
