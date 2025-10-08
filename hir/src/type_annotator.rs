//! Algorithm W (Milner, Hindley–Milner inference)
use diagnostic::Spanned;
use std::collections::{HashMap, HashSet};

use crate::hir::{Expr, ExprKind, FunctionSignature, HIRLocal, Item, Statement};
use crate::ids::FuncId;

use super::error::Error;
use super::hir::{Block, ExprArena, Function, HIRType, OptHIR, THIR};
use super::ids::{ExprId, LocalId, TypeVarId};

/// For brevity
type OT = Option<HIRType>;
type S<T> = Spanned<T>;

/// Type variable (either known type or a variable to be inferred)
#[derive(Clone, Debug)]
enum TypeVar {
    Known(HIRType),
    Var(TypeVarId),
}

/// Type class (for literals)
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum TyClass {
    Int,
    Float,
    // Bool,
    // String,
}

impl TyClass {
    fn fallback_type(&self) -> HIRType {
        match self {
            TyClass::Int => HIRType::Primitive(ast::PrimitiveType::I32),
            TyClass::Float => HIRType::Primitive(ast::PrimitiveType::F32),
            // TyClass::Bool => HIRType::Primitive(ast::PrimitiveType::Bool),
            // TyClass::String => HIRType::Primitive(ast::PrimitiveType::String),
        }
    }
}

/// Type variable constraint
#[derive(Clone, Debug)]
enum Constraint {
    Eq(TypeVar, TypeVar),
    InClass(TypeVar, TyClass),
}

/// Temporary function context for generating type constraints
struct FunctionCtx<'a> {
    next_type_var_id: TypeVarId,
    constraints: Vec<Constraint>, // TODO remove
    func: &'a Function<OT>,
    used_func_signatures: HashMap<FuncId, FunctionSignature>,
    expr_ty: HashMap<ExprId, TypeVar>,
    local_ty: HashMap<LocalId, TypeVar>,
}

impl<'a> FunctionCtx<'a> {
    fn from_function(func: &'a Function<OT>) -> Self {
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
                crate::hir::Statement::VariableDeclaration(var_decl) => {
                    let expr_ty = ctx.ty_of_expr(var_decl.expr_id);
                    let local_ty = ctx.ty_of_local(var_decl.local_id);
                    ctx.constraints.push(Constraint::Eq(expr_ty, local_ty));
                }
                crate::hir::Statement::Expr(eid) | crate::hir::Statement::Return(eid) => {
                    let _ = ctx.ty_of_expr(*eid);
                }
            }
        }

        ctx
    }

    // In the future should be `with_imports` or `with_externals`
    fn with_signatures(self, func_signatures: HashMap<FuncId, FunctionSignature>) -> Self {
        Self {
            used_func_signatures: func_signatures,
            ..self
        }
    }

    fn into_solver(mut self) -> Solver {
        let mut constraints = self.expr_arena_to_constraints(&self.func.expr_arena);

        let block_constraints = self.block_to_constraints(
            &self.func.body,
            TypeVar::Known(self.func.signature.ret_ty.clone()),
        );
        constraints.extend(block_constraints);

        Solver::new(constraints, self.expr_ty, self.local_ty)
    }

    fn block_to_constraints(
        &mut self,
        block: &Block<OT>,
        return_type_var: TypeVar,
    ) -> Vec<Constraint> {
        for local in &block.locals {
            self.add_hir_local(local);
        }

        let mut constraints = Vec::<Constraint>::new();
        for stmt in block.statements.iter() {
            match stmt {
                Statement::VariableDeclaration(var_decl) => {
                    let expr_ty = self.ty_of_expr(var_decl.expr_id);
                    let local_ty = self.ty_of_local(var_decl.local_id);
                    constraints.push(Constraint::Eq(expr_ty, local_ty));
                }
                Statement::Return(expr_id) => {
                    let expr_ty = self.ty_of_expr(*expr_id);
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
            let expr_type_var = self.ty_of_expr(expr.id);
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
                    let local_ty = self.ty_of_local(*lid);
                    constraints.push(Constraint::Eq(expr_type_var, local_ty));
                }
                ExprKind::FunctionCall(func_call) => {
                    let signature = self
                        .used_func_signatures
                        .get(&func_call.func_id)
                        .unwrap()
                        .clone();
                    let return_type_var = TypeVar::Known(signature.ret_ty.clone());
                    constraints.push(Constraint::Eq(expr_type_var, return_type_var));
                    for (i, arg_expr_id) in func_call.args.iter().enumerate() {
                        let arg_expr_ty = self.ty_of_expr(*arg_expr_id);
                        let param_ty = TypeVar::Known(signature.args[i].node.ty.node.clone());
                        constraints.push(Constraint::Eq(arg_expr_ty, param_ty));
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

    fn fresh_type_var(&mut self) -> TypeVar {
        let id = self.next_type_var_id;
        self.next_type_var_id = self.next_type_var_id.increment();
        TypeVar::Var(id)
    }

    fn ty_of_local(&mut self, lid: LocalId) -> TypeVar {
        if let Some(ty) = self.local_ty.get(&lid) {
            ty.clone()
        } else {
            let fresh = self.fresh_type_var();
            self.local_ty.insert(lid, fresh.clone());
            fresh
        }
    }

    fn add_hir_local(&mut self, local: &HIRLocal<OT>) -> TypeVar {
        if let Some(ty) = self.local_ty.get(&local.id) {
            ty.clone()
        } else {
            let ty = local
                .ty
                .clone()
                .map(TypeVar::Known)
                .unwrap_or_else(|| self.fresh_type_var());
            self.local_ty.insert(local.id, ty.clone());
            ty
        }
    }

    fn ty_of_expr(&mut self, eid: ExprId) -> TypeVar {
        if let Some(ty) = self.expr_ty.get(&eid) {
            ty.clone()
        } else {
            let fresh = self.fresh_type_var();
            self.expr_ty.insert(eid, fresh.clone());
            fresh
        }
    }

    fn add_expr(&mut self, expr: &Expr<OT>) -> TypeVar {
        if let Some(ty) = self.expr_ty.get(&expr.id) {
            ty.clone()
        } else {
            let ty = expr
                .ty
                .clone()
                .map(TypeVar::Known)
                .unwrap_or_else(|| self.fresh_type_var());
            self.expr_ty.insert(expr.id, ty.clone());
            ty
        }
    }
}

/// Unification-based type solver
struct Solver {
    parent: HashMap<TypeVarId, TypeVarId>,         // union-find
    binding: HashMap<TypeVarId, HIRType>,          // Var -> Known(T)
    classes: HashMap<TypeVarId, HashSet<TyClass>>, // Var ∈ {...}
    expr_ty: HashMap<ExprId, TypeVar>,
    local_ty: HashMap<LocalId, TypeVar>,
}

impl Solver {
    fn new(
        constraints: Vec<Constraint>,
        expr_ty: HashMap<ExprId, TypeVar>,
        local_ty: HashMap<LocalId, TypeVar>,
    ) -> Solver {
        let mut solver = Solver {
            parent: HashMap::new(),
            binding: HashMap::new(),
            classes: HashMap::new(),
            expr_ty,
            local_ty,
        };

        for constraint in constraints.into_iter() {
            match constraint {
                Constraint::Eq(a, b) => {
                    solver.unify(a, b).ok(); /* TODO: ошибки в errors */
                }
                Constraint::InClass(t, cl) => {
                    if let TypeVar::Var(v) = t {
                        solver.in_class(v, cl)
                    }
                }
            }
        }
        solver
    }

    /// Find with path compression
    fn find(&mut self, v: TypeVarId) -> TypeVarId {
        let p = *self.parent.get(&v).unwrap_or(&v);
        if p != v {
            let r = self.find(p);
            self.parent.insert(v, r);
            r
        } else {
            v
        }
    }

    fn union(&mut self, a: TypeVarId, b: TypeVarId) {
        let ra = self.find(a);
        let rb = self.find(b);
        if ra == rb {
            return;
        }
        self.parent.insert(ra, rb);

        // склеиваем binding и классы
        if let Some(ta) = self.binding.remove(&ra) {
            self.bind(rb, ta).ok(); // конфликт — отловишь выше, если захочешь
        }
        if let Some(tb) = self.binding.get(&rb).cloned() {
            self.bind(rb, tb).ok();
        }

        if let Some(ca) = self.classes.remove(&ra) {
            let entry = self.classes.entry(rb).or_default();
            entry.extend(ca);
        }
    }

    fn bind(&mut self, v: TypeVarId, t: HIRType) -> Result<(), ()> {
        let r = self.find(v);
        if let Some(prev) = self.binding.get(&r) {
            if prev != &t {
                return Err(());
            } // конфликт: Known(A) vs Known(B)
        } else {
            self.binding.insert(r, t);
        }
        Ok(())
    }

    fn in_class(&mut self, v: TypeVarId, c: TyClass) {
        let r = self.find(v);
        self.classes.entry(r).or_default().insert(c);
    }

    fn unify(&mut self, a: TypeVar, b: TypeVar) -> Result<(), ()> {
        match (a, b) {
            (TypeVar::Known(x), TypeVar::Known(y)) => {
                if x == y {
                    Ok(())
                } else {
                    Err(())
                }
            }
            (TypeVar::Known(t), TypeVar::Var(v)) | (TypeVar::Var(v), TypeVar::Known(t)) => {
                self.bind(v, t)
            }
            (TypeVar::Var(x), TypeVar::Var(y)) => {
                self.union(x, y);
                Ok(())
            }
        }
    }

    /// Финализация: подставляем Known или фолбэки
    fn resolve(&mut self, t: TypeVar) -> Result<HIRType, ()> {
        match t {
            TypeVar::Known(k) => Ok(k),
            TypeVar::Var(v) => {
                let r = self.find(v);
                if let Some(k) = self.binding.get(&r).cloned() {
                    return Ok(k);
                }
                // нет явного Known — смотрим классы
                if let Some(cls) = self.classes.get(&r) {
                    // Простой случай: один класс => берём фолбэк
                    if cls.len() == 1 {
                        let c = *cls.iter().next().unwrap();
                        let fallback_type = c.fallback_type();
                        self.binding.insert(r, fallback_type.clone());
                        return Ok(fallback_type);
                    }
                    // Несколько классов ⇒ ищи пересечение; в простом варианте — ошибка
                    return Err(());
                }
                // вообще ни подсказок — ошибка (нужна аннотация)
                Err(())
            }
        }
    }

    fn resolve_local(&mut self, local_id: LocalId) -> Result<HIRType, ()> {
        if let Some(tv) = self.local_ty.get(&local_id).cloned() {
            self.resolve(tv)
        } else {
            Err(())
        }
    }

    fn resolve_expr(&mut self, expr_id: ExprId) -> Result<HIRType, ()> {
        if let Some(tv) = self.expr_ty.get(&expr_id).cloned() {
            self.resolve(tv)
        } else {
            Err(())
        }
    }
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
                    let constraints = FunctionCtx::from_function(&func)
                        .with_signatures(signatures.clone())
                        .into_solver();
                    let annotator = FunctionAnnotator::new(constraints);
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

    fn annotate(mut self, func: Function<OT>) -> (Function<HIRType>, Vec<S<Error>>) {
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

    fn annotate_expr_arena(&mut self, arena: ExprArena<OT>) -> ExprArena<HIRType> {
        let mut out_exprs: Vec<Expr<HIRType>> = Vec::with_capacity(arena.0.len());
        for expr in arena.0.into_iter() {
            let ty = if let Some(ty) = expr.ty {
                ty
            } else {
                match self.solver.resolve_expr(expr.id) {
                    Ok(t) => t,
                    Err(e) => {
                        todo!()
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
            out_exprs.push(Expr {
                id: expr.id,
                kind,
                ty,
            });
        }
        ExprArena(out_exprs)
    }

    fn annotate_block(&mut self, block: Block<OT>) -> Block<HIRType> {
        let locals = block
            .locals
            .into_iter()
            .map(|local| {
                let ty = if let Some(ty) = local.ty {
                    ty
                } else {
                    match self.solver.resolve_local(local.id) {
                        Ok(t) => t,
                        Err(e) => {
                            todo!()
                        }
                    }
                };
                HIRLocal {
                    id: local.id,
                    name: local.name,
                    ty,
                }
            })
            .collect();
        Block {
            locals,
            statements: block.statements,
        }
    }
}
