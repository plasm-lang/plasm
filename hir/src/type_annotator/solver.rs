use std::collections::{HashMap, HashSet};

use diagnostic::{MaybeSpanned, Spanned};

use crate::error::Error;
use crate::hir::HIRType;
use crate::ids::{ExprId, LocalId, TypeVarId};

use super::type_var::{Constraint, TyClass, TypeVar};

// For brevity
type S<T> = Spanned<T>;

/// Unification-based type solver
pub struct Solver {
    parent: HashMap<TypeVarId, TypeVarId>, // union-find
    binding: HashMap<TypeVarId, MaybeSpanned<HIRType>>, // Var -> Known(T)
    classes: HashMap<TypeVarId, HashSet<TyClass>>, // Var ∈ {...}
    expr_ty: HashMap<ExprId, S<TypeVar>>,
    local_ty: HashMap<LocalId, S<TypeVar>>,
}

impl Solver {
    pub fn new(
        constraints: Vec<Constraint>,
        expr_ty: HashMap<ExprId, S<TypeVar>>,
        local_ty: HashMap<LocalId, S<TypeVar>>,
    ) -> (Solver, Vec<S<Error>>) {
        let mut solver = Solver {
            parent: HashMap::new(),
            binding: HashMap::new(),
            classes: HashMap::new(),
            expr_ty,
            local_ty,
        };
        let mut errors = Vec::new();

        for constraint in constraints.into_iter() {
            match constraint {
                Constraint::Eq(a, b) => {
                    if let Err(e) = solver.unify(a.node, b.node) {
                        errors.push(S::new(e, a.span.max(b.span)));
                    }
                }
                Constraint::InClass(type_var, cl) => {
                    if let TypeVar::Var(type_var_id) = type_var.node {
                        solver.in_class(type_var_id, cl)
                    }
                }
            }
        }
        (solver, errors)
    }

    /// Find with path compression
    fn find(&mut self, type_var_id: TypeVarId) -> TypeVarId {
        let p = *self.parent.get(&type_var_id).unwrap_or(&type_var_id);
        if p != type_var_id {
            let target_type_var_id = self.find(p);
            self.parent.insert(type_var_id, target_type_var_id);
            target_type_var_id
        } else {
            type_var_id
        }
    }

    fn union(&mut self, a: TypeVarId, b: TypeVarId) -> Result<(), Error> {
        let target_a = self.find(a);
        let target_b = self.find(b);
        if target_a == target_b {
            return Ok(());
        }
        self.parent.insert(target_a, target_b);

        if let Some(ty_a) = self.binding.remove(&target_a) {
            self.bind(target_b, ty_a)?;
        }
        if let Some(ty_b) = self.binding.get(&target_b).cloned() {
            self.bind(target_b, ty_b)?;
        }

        if let Some(classes) = self.classes.remove(&target_a) {
            let entry = self.classes.entry(target_b).or_default();
            entry.extend(classes);
        }

        Ok(())
    }

    fn bind(&mut self, type_var_id: TypeVarId, ty: MaybeSpanned<HIRType>) -> Result<(), Error> {
        let target_type_var_id = self.find(type_var_id);
        if let Some(prev) = self.binding.get(&target_type_var_id) {
            if prev != &ty {
                return Err(Error::TypesConflict {
                    first: prev.clone(),
                    second: ty,
                });
            }
        } else {
            self.binding.insert(target_type_var_id, ty);
        }
        Ok(())
    }

    fn in_class(&mut self, type_var_id: TypeVarId, c: TyClass) {
        let target_type_var_id = self.find(type_var_id);
        self.classes
            .entry(target_type_var_id)
            .or_default()
            .insert(c);
    }

    fn unify(&mut self, a: TypeVar, b: TypeVar) -> Result<(), Error> {
        match (a, b) {
            (TypeVar::Known(known_a), TypeVar::Known(known_b)) => {
                if known_a == known_b {
                    Ok(())
                } else {
                    Err(Error::TypesConflict {
                        first: known_a,
                        second: known_b,
                    })
                }
            }
            (TypeVar::Known(ty), TypeVar::Var(type_var_id))
            | (TypeVar::Var(type_var_id), TypeVar::Known(ty)) => self.bind(type_var_id, ty),
            (TypeVar::Var(x), TypeVar::Var(y)) => self.union(x, y),
        }
    }

    fn resolve(&mut self, type_var: S<TypeVar>) -> Result<MaybeSpanned<HIRType>, S<Error>> {
        match type_var.node {
            TypeVar::Known(known) => Ok(known),
            TypeVar::Var(type_var_id) => {
                let target_type_var_id = self.find(type_var_id);

                // If type variable is bound to a known type, return it
                if let Some(known) = self.binding.get(&target_type_var_id).cloned() {
                    // If known type conflicts with class constraints, error out
                    if let Some(classes) = self.classes.get(&target_type_var_id)
                        && classes.len() == 1
                    {
                        let class = *classes.iter().next().unwrap();
                        let possible_class = TyClass::from_type(&known.node);
                        if let Some(possible_class) = possible_class
                            && possible_class != class
                        {
                            return Err(S::new(
                                Error::TypesConflict {
                                    first: known.clone(),
                                    second: MaybeSpanned::new(class.fallback_type()),
                                },
                                type_var.span,
                            ));
                        }
                    }
                    return Ok(known);
                }

                // If type variable is constrained to a single class, use its fallback type
                if let Some(classes) = self.classes.get(&target_type_var_id) {
                    if classes.len() == 1 {
                        let c = *classes.iter().next().unwrap();
                        let fallback_type = MaybeSpanned::new(c.fallback_type());
                        self.binding
                            .insert(target_type_var_id, fallback_type.clone());
                        return Ok(fallback_type);
                    }

                    // If multiple classes, error out as ambiguous
                    return Err(S::new(
                        Error::AmbiguousClass {
                            possible_classes: classes.iter().copied().collect(),
                        },
                        type_var.span,
                    ));
                }

                // Otherwise, cannot resolve
                Err(S::new(Error::CantResolveType, type_var.span))
            }
        }
    }

    pub fn resolve_local(&mut self, local_id: LocalId) -> Result<MaybeSpanned<HIRType>, S<Error>> {
        if let Some(tv) = self.local_ty.get(&local_id).cloned() {
            self.resolve(tv)
        } else {
            Err(S::zero(Error::UnregisteredLocalId { id: local_id }))
        }
    }

    pub fn resolve_expr(&mut self, expr_id: ExprId) -> Result<MaybeSpanned<HIRType>, S<Error>> {
        if let Some(tv) = self.expr_ty.get(&expr_id).cloned() {
            self.resolve(tv)
        } else {
            Err(S::zero(Error::UnregisteredExprId { id: expr_id }))
        }
    }
}

/// I generated these tests using ChatGPT as I'm a lazy bitch^-^
/// I checked it and it looks correct so I keep it. Just FYI.
#[cfg(test)]
mod tests {
    use super::*;
    use ast::ast::PrimitiveType;

    // ---------- helpers ----------

    fn s<T>(node: T) -> Spanned<T> {
        Spanned {
            node,
            span: diagnostic::Span { start: 0, end: 0 },
        }
    }

    fn ms<T: Clone>(node: T) -> MaybeSpanned<T> {
        MaybeSpanned { node, span: None }
    }

    fn i32_ty() -> HIRType {
        HIRType::Primitive(PrimitiveType::I32)
    }

    fn f32_ty() -> HIRType {
        HIRType::Primitive(PrimitiveType::F32)
    }

    fn expr_id(n: u32) -> ExprId {
        ExprId::new(std::num::NonZeroU32::new(n).unwrap())
    }

    fn local_id(n: u32) -> LocalId {
        LocalId::new(std::num::NonZeroU32::new(n).unwrap())
    }

    fn tvar_id(n: u32) -> TypeVarId {
        TypeVarId::new(std::num::NonZeroU32::new(n).unwrap())
    }

    // ---------- tests ----------

    #[test]
    fn eq_var_known_binds_and_resolves() {
        // constraints: v1 == i32
        // resolve_expr(e1) => i32
        let e1 = expr_id(1);
        let v1 = tvar_id(1);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e1, s(TypeVar::Var(v1)));

        let local_ty = HashMap::new();

        let constraints = vec![Constraint::Eq(
            s(TypeVar::Var(v1)),
            s(TypeVar::Known(ms(i32_ty()))),
        )];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty);
        assert!(errs.is_empty(), "unexpected constraint errors: {:?}", errs);

        let ty = solver.resolve_expr(e1).expect("resolve_expr failed");
        assert_eq!(ty.node, i32_ty());
    }

    #[test]
    fn inclass_single_class_fallbacks() {
        // constraints: v1 ∈ Int
        // resolve_expr(e1) => fallback of Int (i32)
        let e1 = expr_id(1);
        let v1 = tvar_id(1);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e1, s(TypeVar::Var(v1)));
        let local_ty = HashMap::new();

        let constraints = vec![Constraint::InClass(s(TypeVar::Var(v1)), TyClass::Int)];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty);
        assert!(errs.is_empty());

        let ty = solver.resolve_expr(e1).expect("resolve_expr failed");
        assert_eq!(ty.node, i32_ty(), "Int fallback should be i32");
    }

    #[test]
    fn inclass_multiple_classes_is_ambiguous() {
        // constraints: v1 ∈ Int, v1 ∈ Float
        // resolve_expr(e1) => Err(AmbiguousClass)
        let e1 = expr_id(1);
        let v1 = tvar_id(1);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e1, s(TypeVar::Var(v1)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::InClass(s(TypeVar::Var(v1)), TyClass::Int),
            Constraint::InClass(s(TypeVar::Var(v1)), TyClass::Float),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty);
        assert!(errs.is_empty());

        let err = solver.resolve_expr(e1).unwrap_err();
        matches!(err.node, Error::AmbiguousClass { .. });
    }

    #[test]
    fn conflict_known_known_is_reported_during_new() {
        // constraints: i32 == f32 -> TypesConflict returned from Solver::new
        let expr_ty = HashMap::new();
        let local_ty = HashMap::new();

        let constraints = vec![Constraint::Eq(
            s(TypeVar::Known(ms(i32_ty()))),
            s(TypeVar::Known(ms(f32_ty()))),
        )];

        let (_solver, errs) = Solver::new(constraints, expr_ty, local_ty);
        assert!(!errs.is_empty(), "expected conflict error");
        let has_conflict = errs
            .iter()
            .any(|e| matches!(e.node, Error::TypesConflict { .. }));
        assert!(has_conflict, "expected TypesConflict, got: {:?}", errs);
    }

    #[test]
    fn union_propagates_binding_across_vars() {
        // constraints: v1 == v2, v1 == i32
        // resolve_expr(e2) => i32
        let e1 = expr_id(1);
        let e2 = expr_id(2);
        let v1 = tvar_id(1);
        let v2 = tvar_id(2);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e1, s(TypeVar::Var(v1)));
        expr_ty.insert(e2, s(TypeVar::Var(v2)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::Eq(s(TypeVar::Var(v1)), s(TypeVar::Var(v2))),
            Constraint::Eq(s(TypeVar::Var(v1)), s(TypeVar::Known(ms(i32_ty())))),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty);
        assert!(errs.is_empty());

        let ty2 = solver.resolve_expr(e2).expect("resolve_expr failed");
        assert_eq!(ty2.node, i32_ty());
    }

    #[test]
    fn resolve_local_unregistered_yields_error() {
        let expr_ty = HashMap::new();
        let local_ty = HashMap::new();
        let constraints = Vec::<Constraint>::new();

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty);
        assert!(errs.is_empty());

        let bogus_local = local_id(42);
        let err = solver.resolve_local(bogus_local).unwrap_err();
        matches!(err.node, Error::UnregisteredLocalId { .. });
    }

    #[test]
    fn bound_known_conflicts_with_inclass_mismatch() {
        // constraints: v1 == f32, v1 ∈ Int
        // resolve_expr(e1) => Err(TypesConflict) (known f32 violates class Int)
        //
        // This relies on TyClass::from_type(HIRType) to classify known types.
        let e1 = expr_id(1);
        let v1 = tvar_id(1);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e1, s(TypeVar::Var(v1)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::Eq(s(TypeVar::Var(v1)), s(TypeVar::Known(ms(f32_ty())))),
            Constraint::InClass(s(TypeVar::Var(v1)), TyClass::Int),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty);
        assert!(errs.is_empty(), "no constraint error expected during new");

        let err = solver.resolve_expr(e1).unwrap_err();
        matches!(err.node, Error::TypesConflict { .. });
    }

    #[test]
    fn inclass_on_both_vars_then_union_then_bind_resolves() {
        // constraints:
        //   v1 ∈ Int
        //   v2 ∈ Int
        //   v1 == v2
        //   v2 == i32
        // resolve_expr(e1) => i32, resolve_expr(e2) => i32
        let e1 = expr_id(1);
        let e2 = expr_id(2);
        let v1 = tvar_id(1);
        let v2 = tvar_id(2);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e1, s(TypeVar::Var(v1)));
        expr_ty.insert(e2, s(TypeVar::Var(v2)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::InClass(s(TypeVar::Var(v1)), TyClass::Int),
            Constraint::InClass(s(TypeVar::Var(v2)), TyClass::Int),
            Constraint::Eq(s(TypeVar::Var(v1)), s(TypeVar::Var(v2))),
            Constraint::Eq(s(TypeVar::Var(v2)), s(TypeVar::Known(ms(i32_ty())))),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty);
        assert!(errs.is_empty());

        let t1 = solver.resolve_expr(e1).expect("resolve e1");
        let t2 = solver.resolve_expr(e2).expect("resolve e2");
        assert_eq!(t1.node, i32_ty());
        assert_eq!(t2.node, i32_ty());
    }

    #[test]
    fn chain_union_path_compression_still_resolves() {
        // constraints: v1==v2, v2==v3, v1==i32
        // resolve_expr(e3) => i32
        let e3 = expr_id(3);
        let v1 = tvar_id(1);
        let v2 = tvar_id(2);
        let v3 = tvar_id(3);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e3, s(TypeVar::Var(v3)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::Eq(s(TypeVar::Var(v1)), s(TypeVar::Var(v2))),
            Constraint::Eq(s(TypeVar::Var(v2)), s(TypeVar::Var(v3))),
            Constraint::Eq(s(TypeVar::Var(v1)), s(TypeVar::Known(ms(i32_ty())))),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty);
        assert!(errs.is_empty());

        let t3 = solver.resolve_expr(e3).expect("resolve e3");
        assert_eq!(t3.node, i32_ty());
    }
}
