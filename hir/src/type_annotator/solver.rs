use std::collections::{HashMap, HashSet};

use diagnostic::{MaybeSpanned, Spanned};

use crate::error::Error;
use crate::hir::HIRType;
use crate::ids::{ExprId, LocalId, TypeVarId};

use super::type_var::{Constraint, TyClass, TypeVar};

/// For brevity
type OT = Option<S<HIRType>>;
type S<T> = Spanned<T>;
type MaybeS<T> = MaybeSpanned<T>;

/// Unification-based type solver
pub struct Solver {
    parent: HashMap<TypeVarId, TypeVarId>,         // union-find
    binding: HashMap<TypeVarId, MaybeS<HIRType>>,  // Var -> Known(T)
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
                Constraint::InClass(t, cl) => {
                    if let TypeVar::Var(v) = t.node {
                        solver.in_class(v, cl)
                    }
                }
            }
        }
        (solver, errors)
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

    fn union(&mut self, a: TypeVarId, b: TypeVarId) -> Result<(), Error> {
        let ra = self.find(a);
        let rb = self.find(b);
        if ra == rb {
            return Ok(());
        }
        self.parent.insert(ra, rb);

        // склеиваем binding и классы
        if let Some(ta) = self.binding.remove(&ra) {
            self.bind(rb, ta)?; // конфликт — отловишь выше, если захочешь
        }
        if let Some(tb) = self.binding.get(&rb).cloned() {
            self.bind(rb, tb)?;
        }

        if let Some(ca) = self.classes.remove(&ra) {
            let entry = self.classes.entry(rb).or_default();
            entry.extend(ca);
        }

        Ok(())
    }

    fn bind(&mut self, v: TypeVarId, t: MaybeS<HIRType>) -> Result<(), Error> {
        let r = self.find(v);
        if let Some(prev) = self.binding.get(&r) {
            if prev != &t {
                return Err(Error::TypesConflict {
                    first: prev.clone(),
                    second: t,
                });
            }
        } else {
            self.binding.insert(r, t);
        }
        Ok(())
    }

    fn in_class(&mut self, v: TypeVarId, c: TyClass) {
        let r = self.find(v);
        self.classes.entry(r).or_default().insert(c);
    }

    fn unify(&mut self, a: TypeVar, b: TypeVar) -> Result<(), Error> {
        match (a, b) {
            (TypeVar::Known(x), TypeVar::Known(y)) => {
                if x == y {
                    Ok(())
                } else {
                    Err(Error::TypesConflict {
                        first: x,
                        second: y,
                    })
                }
            }
            (TypeVar::Known(t), TypeVar::Var(v)) | (TypeVar::Var(v), TypeVar::Known(t)) => {
                self.bind(v, t)
            }
            (TypeVar::Var(x), TypeVar::Var(y)) => self.union(x, y),
        }
    }

    /// Финализация: подставляем Known или фолбэки
    fn resolve(&mut self, t: S<TypeVar>) -> Result<MaybeS<HIRType>, S<Error>> {
        match t.node {
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
                        let fallback_type = MaybeS::new(c.fallback_type());
                        self.binding.insert(r, fallback_type.clone());
                        return Ok(fallback_type);
                    }
                    // Несколько классов ⇒ ищи пересечение; в простом варианте — ошибка
                    return Err(S::new(
                        Error::AmbiguousClass {
                            possible_classes: cls.iter().copied().collect(),
                        },
                        t.span,
                    ));
                }
                // вообще ни подсказок — ошибка (нужна аннотация)
                Err(S::new(Error::CantResolveType, t.span))
            }
        }
    }

    pub fn resolve_local(&mut self, local_id: LocalId) -> Result<MaybeS<HIRType>, S<Error>> {
        if let Some(tv) = self.local_ty.get(&local_id).cloned() {
            self.resolve(tv)
        } else {
            Err(S::zero(Error::UnregisteredLocalId { id: local_id }))
        }
    }

    pub fn resolve_expr(&mut self, expr_id: ExprId) -> Result<MaybeS<HIRType>, S<Error>> {
        if let Some(tv) = self.expr_ty.get(&expr_id).cloned() {
            self.resolve(tv)
        } else {
            Err(S::zero(Error::UnregisteredExprId { id: expr_id }))
        }
    }
}
