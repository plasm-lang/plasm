use std::collections::{HashMap, HashSet};

use diagnostic::{MaybeSpanned, Span, Spanned};
use utils::ids::{ExprId, HIRTypeId, LocalId, TypeVarId};

use super::type_var::{Constraint, Shape, TyClass, TypeVar};
use crate::error::Error;
use crate::types::{HIRType, HIRTypeArena, StructField, StructType};

// For brevity
type S<T> = Spanned<T>;
type MS<T> = MaybeSpanned<T>;

/// Unification-based type solver.
///
/// Resolves the concrete type of every type variable by processing a set of
/// equality constraints (`Eq`) and class constraints (`InClass`).
///
/// Internally uses a union–find (disjoint-set) data structure to group type
/// variables that must have the same type. Each group has at most one concrete
/// type binding and zero or more class constraints.
///
/// See also: <https://en.wikipedia.org/wiki/Disjoint-set_data_structure>
pub struct Solver<'a> {
    type_arena: &'a mut HIRTypeArena,

    /// Union-find parent map. Maps a type variable to its parent in the
    /// union-find tree. If a variable is not present, it is its own root.
    /// Used to group type variables that are constrained to be equal.
    parent: HashMap<TypeVarId, TypeVarId>,

    /// Maps the root of each union-find group to the concrete type that has
    /// been assigned to that group. A group can have at most one binding, a
    /// second conflicting binding produces a `TypesConflict` error.
    binding: HashMap<TypeVarId, MS<HIRTypeId>>,

    /// Maps the root of each union-find group to the set of type classes its
    /// type must belong to (e.g. `Int`, `Float`). When resolving, if no
    /// concrete type is bound yet and exactly one class is present, the class
    /// fallback type is used. Multiple classes produce an `AmbiguousClass`
    /// error.
    classes: HashMap<TypeVarId, HashSet<TyClass>>,

    /// Maps the root of each union-find group to its structural shape constraint.
    ///
    /// A `Shape` describes what fields a type must have before the concrete type is
    /// known — it is generated for struct literals (`struct { x: 1, y: 2 }`). Once a
    /// concrete type is bound to the same group, the shape is reconciled with it:
    /// field names are matched, and each field's TypeVar is unified with the
    /// corresponding field type from the known struct. If the concrete type turns out
    /// not to be a struct, a `TypesConflict` error is reported.
    ///
    /// Each group has at most one shape. When two groups with shapes are merged via
    /// `union`, their shapes are merged by `merge_shapes` before reconciliation.
    shapes: HashMap<TypeVarId, S<Shape>>,

    expr_ty: HashMap<ExprId, S<TypeVar>>,
    local_ty: HashMap<LocalId, S<TypeVar>>,
}

impl<'a> Solver<'a> {
    /// Creates a new solver and immediately processes all constraints.
    ///
    /// `Eq(a, b)` constraints are unified: the two type variables (or known
    /// types) are required to be the same type.  
    /// `InClass(var, class)` constraints record that the variable's type must
    /// belong to the given class.
    ///
    /// Errors discovered while processing constraints (e.g. two known types
    /// that conflict) are collected and returned alongside the solver. Errors
    /// that can only be detected at resolution time are returned later by
    /// `resolve_expr` / `resolve_local`.
    pub fn new(
        constraints: Vec<Constraint>,
        expr_ty: HashMap<ExprId, S<TypeVar>>,
        local_ty: HashMap<LocalId, S<TypeVar>>,
        type_arena: &'a mut HIRTypeArena,
    ) -> (Solver<'a>, Vec<S<Error>>) {
        let mut solver = Solver {
            type_arena,
            parent: HashMap::new(),
            binding: HashMap::new(),
            classes: HashMap::new(),
            shapes: HashMap::new(),
            expr_ty,
            local_ty,
        };
        let mut errors = Vec::new();

        for constraint in constraints.into_iter() {
            match constraint {
                Constraint::Eq(a, b) => {
                    let errs = solver.unify(a.node, b.node);
                    errors.extend(errs.into_iter().map(|e| S::new(e, a.span.max(b.span))));
                }
                Constraint::InClass(type_var, class) => {
                    if let TypeVar::Var(type_var_id) = type_var.node {
                        solver.in_class(type_var_id, class)
                    }
                }
                Constraint::HasShape(type_var, shape) => {
                    if let TypeVar::Var(type_var_id) = type_var.node {
                        let errs = solver.has_shape(type_var_id, S::new(shape, type_var.span));
                        errors.extend(errs.into_iter().map(|e| S::new(e, type_var.span)));
                    }
                }
            }
        }

        (solver, errors)
    }

    /// Returns the root of the union-find group that `type_var_id` belongs to.
    ///
    /// Uses path compression: every node visited on the way to the root is
    /// re-pointed directly at the root so that future lookups are faster.
    fn find(&mut self, type_var_id: TypeVarId) -> TypeVarId {
        let parent = *self.parent.get(&type_var_id).unwrap_or(&type_var_id);
        if parent != type_var_id {
            let target_type_var_id = self.find(parent);
            self.parent.insert(type_var_id, target_type_var_id);
            target_type_var_id
        } else {
            type_var_id
        }
    }

    /// Merges the union-find groups of type variables `a` and `b`.
    ///
    /// After this call, `a` and `b` share the same root. Any concrete type
    /// binding and class constraints that were on `a`'s group are moved to
    /// `b`'s group. If both groups are already bound to different types, a
    /// `TypesConflict` error is returned.
    fn union(&mut self, a: TypeVarId, b: TypeVarId) -> Vec<Error> {
        let target_a = self.find(a);
        let target_b = self.find(b);
        if target_a == target_b {
            return vec![];
        }
        self.parent.insert(target_a, target_b);
        let mut errors = Vec::new();

        // Remove shapes BEFORE binding transfers so that reconcile inside bind
        // does not fire on a stale or partial shape. We reconcile once explicitly
        // after the shapes are merged/inserted below.
        let shape_a = self.shapes.remove(&target_a);
        let shape_b = self.shapes.remove(&target_b);

        if let Some(ty_a) = self.binding.remove(&target_a) {
            errors.extend(self.bind(target_b, ty_a));
        }
        // The second bind(target_b, ty_b) is always a no-op (same binding) — drop it.

        if let Some(classes) = self.classes.remove(&target_a) {
            let entry = self.classes.entry(target_b).or_default();
            entry.extend(classes);
        }

        // Merge or transfer shapes, then reconcile once with the current binding.
        match (shape_a, shape_b) {
            (Some(sa), Some(sb)) => {
                // Both groups have a shape — merge them, keep span of target_a's literal.
                let (merged, merge_errors) = self.merge_shapes(sa, sb);
                errors.extend(merge_errors);
                self.shapes.insert(target_b, merged);
                if let Some(known) = self.binding.get(&target_b).copied() {
                    errors.extend(self.reconcile_shape_with_known(target_b, known));
                }
            }
            (Some(sa), None) | (None, Some(sa)) => {
                self.shapes.insert(target_b, sa);
                if let Some(known) = self.binding.get(&target_b).copied() {
                    errors.extend(self.reconcile_shape_with_known(target_b, known));
                }
            }
            (None, None) => {}
        }

        errors
    }

    /// Merges two Shape field lists.
    ///
    /// Field names must form the same set in both shapes; if a name is present
    /// in one but not the other, an error is emitted. TypeVars of fields with
    /// matching names are unified. The order of `existing` is preserved.
    fn merge_shapes(&mut self, existing: S<Shape>, incoming: S<Shape>) -> (S<Shape>, Vec<Error>) {
        let span = existing.span;
        let Shape::Struct(fields_existing) = existing.node;
        let Shape::Struct(fields_incoming) = incoming.node;

        let mut fields_incoming_map: HashMap<_, _> = fields_incoming.into_iter().collect();

        let mut errors = Vec::new();

        for (name, tv_existing) in &fields_existing {
            if let Some(tv_incoming) = fields_incoming_map.remove(name) {
                errors.extend(self.unify(tv_existing.node.clone(), tv_incoming.node));
            } else {
                errors.push(Error::MissingStructField {
                    name: name.node.clone(),
                });
            }
        }

        // Remaining keys in incoming_map are fields not present in existing
        for (name, _) in fields_incoming_map {
            errors.push(Error::UnknownStructField { name: name.node });
        }

        (S::new(Shape::Struct(fields_existing), span), errors)
    }

    /// Assigns the concrete type `ty` to the union-find group of `type_var_id`.
    ///
    /// If the group already has a binding that is identical to `ty`, this is a
    /// no-op. If the group is bound to a different type, returns
    /// `TypesConflict`.
    fn bind(&mut self, type_var_id: TypeVarId, ty: MS<HIRTypeId>) -> Vec<Error> {
        let root = self.find(type_var_id);
        match self.binding.get(&root).copied() {
            // Already bound to the same type — no-op.
            Some(prev) if prev == ty => vec![],
            // Conflict: group is bound to a different type.
            Some(prev) => {
                let first_hir = self.type_arena.get_by_id(prev.node).unwrap().clone();
                let second_hir = self.type_arena.get_by_id(ty.node).unwrap().clone();
                vec![Error::TypesConflict {
                    first: MS {
                        node: first_hir,
                        span: prev.span,
                    },
                    second: MS {
                        node: second_hir,
                        span: ty.span,
                    },
                }]
            }
            // Not yet bound — record the type and reconcile any pending shape constraint.
            None => {
                self.binding.insert(root, ty);
                self.reconcile_shape_with_known(root, ty)
            }
        }
    }

    /// Records that the type of `type_var_id` must belong to class `c`.
    ///
    /// The constraint is stored on the root of the group so it is shared by
    /// all variables in the same group.
    fn in_class(&mut self, type_var_id: TypeVarId, c: TyClass) {
        let target_type_var_id = self.find(type_var_id);
        self.classes
            .entry(target_type_var_id)
            .or_default()
            .insert(c);
    }

    /// Registers a structural shape constraint for `type_var_id`.
    ///
    /// If the group already has a shape (e.g. two struct literals share the same TypeVar after
    /// unification), the two shapes are merged by `merge_shapes` so no information is lost.
    /// After inserting or merging, if the group is already bound to a concrete type the shape
    /// is immediately reconciled with it.
    fn has_shape(&mut self, type_var_id: TypeVarId, shape: S<Shape>) -> Vec<Error> {
        let root = self.find(type_var_id);
        let mut errors = Vec::new();

        if let Some(existing) = self.shapes.remove(&root) {
            let (merged, merge_errors) = self.merge_shapes(existing, shape);
            errors.extend(merge_errors);
            self.shapes.insert(root, merged);
        } else {
            self.shapes.insert(root, shape);
        }

        if let Some(known) = self.binding.get(&root).copied() {
            errors.extend(self.reconcile_shape_with_known(root, known));
        }

        errors
    }

    /// Resolves all shape fields to concrete types and builds a `HIRType::Struct`.
    ///
    /// Returns an error if any field's type cannot be resolved. Used in the shape-fallback
    /// path of `resolve` where all field types must be known.
    /// Resolves all shape fields to concrete types and builds a `HIRType::Struct`.
    ///
    /// Returns the first resolution error encountered, stopping immediately.
    fn resolve_shape_fields_to_struct(
        &mut self,
        fields: Vec<(S<String>, S<TypeVar>)>,
    ) -> Result<HIRType, S<Error>> {
        let mut resolved_fields = Vec::with_capacity(fields.len());
        for (name_s, tv) in fields {
            let name_span = name_s.span;
            let ty_span = tv.span;
            let field_type_id = self.resolve(tv)?;
            let field_hir = self
                .type_arena
                .get_by_id(field_type_id.node)
                .unwrap()
                .clone();
            resolved_fields.push(S::new(
                StructField {
                    name: name_s,
                    ty: S::new(field_hir, ty_span),
                },
                Span {
                    start: name_span.start,
                    end: ty_span.end,
                },
            ));
        }
        Ok(HIRType::Struct(StructType {
            fields: resolved_fields,
        }))
    }

    /// Reconciles a Shape constraint on `root` with a newly known `HIRTypeId`.
    ///
    /// Checks that the set of field names in the Shape matches the set of
    /// fields in the Known struct type, and unifies each matching field's
    /// TypeVar with the concrete field type from the Known struct.
    fn reconcile_shape_with_known(
        &mut self,
        root: TypeVarId,
        known_id: MS<HIRTypeId>,
    ) -> Vec<Error> {
        // Clone shape fields to release the borrow on self.shapes
        let (shape_fields, _shape_span) = match self.shapes.get(&root) {
            Some(s_shape) => {
                let Shape::Struct(fields) = &s_shape.node;
                (fields.clone(), s_shape.span)
            }
            None => return vec![],
        };

        let known_hir = self.type_arena.get_by_id(known_id.node).unwrap().clone();

        let struct_type = match known_hir.peel_named() {
            HIRType::Struct(st) => st.clone(),
            _non_struct => {
                self.shapes.remove(&root);
                return vec![Error::ShapeOnNonStructType {
                    known: MS {
                        node: known_hir,
                        span: known_id.span,
                    },
                }];
            }
        };

        // Consume the known struct fields into a map — moves field names and types, no extra cloning
        let mut known_field_map: HashMap<String, HIRType> = struct_type
            .fields
            .into_iter()
            .map(|f| (f.node.name.node, f.node.ty.node))
            .collect();

        let mut errors = Vec::new();

        // For each literal field: match against known struct via remove() to move the type
        for (field_name_s, field_tv) in shape_fields {
            if let Some(field_type) = known_field_map.remove(&field_name_s.node) {
                let field_type_id = self.type_arena.get_or_insert(field_type);
                errors.extend(self.unify(field_tv.node, TypeVar::Known(MS::new(field_type_id))));
            } else {
                errors.push(Error::UnknownStructField {
                    name: field_name_s.node,
                });
            }
        }

        // Remaining entries are fields required by the known struct but absent in the literal
        for (name, _) in known_field_map {
            errors.push(Error::MissingStructField { name });
        }

        errors
    }

    /// Unifies two `TypeVar` values, requiring them to represent the same type.
    ///
    /// Dispatches to `bind` or `union` depending on whether each side is a
    /// known concrete type or an unresolved variable:
    /// - `Known` vs `Known`: the two types must be identical, otherwise
    ///   `TypesConflict` is returned.
    /// - `Known` vs `Var`: the variable is bound to the known type via `bind`.
    /// - `Var` vs `Var`: the two groups are merged via `union`.
    fn unify(&mut self, a: TypeVar, b: TypeVar) -> Vec<Error> {
        match (a, b) {
            (TypeVar::Known(known_a), TypeVar::Known(known_b)) => {
                if known_a == known_b {
                    vec![]
                } else {
                    let first_hir = self.type_arena.get_by_id(known_a.node).unwrap().clone();
                    let second_hir = self.type_arena.get_by_id(known_b.node).unwrap().clone();
                    vec![Error::TypesConflict {
                        first: MS {
                            node: first_hir,
                            span: known_a.span,
                        },
                        second: MS {
                            node: second_hir,
                            span: known_b.span,
                        },
                    }]
                }
            }
            (TypeVar::Known(ty), TypeVar::Var(type_var_id))
            | (TypeVar::Var(type_var_id), TypeVar::Known(ty)) => self.bind(type_var_id, ty),
            (TypeVar::Var(x), TypeVar::Var(y)) => self.union(x, y),
        }
    }

    /// Resolves a `TypeVar` to a concrete `HIRTypeId`.
    ///
    /// Resolution rules, in order:
    /// 1. If the value is already `Known`, return it directly.
    /// 2. Find the root of the variable's union-find group.
    /// 3. If the root has a concrete binding, verify it is compatible with any
    ///    class constraint and return it. Returns `TypesConflict` if the bound
    ///    type does not satisfy the class.
    /// 4. If the root has exactly one class constraint and no binding, use the
    ///    class's fallback type (e.g. `i32` for `Int`) and record it as the
    ///    binding.
    /// 5. If the root has more than one class constraint, return
    ///    `AmbiguousClass`.
    /// 6. If nothing is known about the variable, return `CantResolveType`.
    fn resolve(&mut self, type_var: S<TypeVar>) -> Result<MS<HIRTypeId>, S<Error>> {
        match type_var.node {
            TypeVar::Known(known) => Ok(known),
            TypeVar::Var(type_var_id) => {
                let target_type_var_id = self.find(type_var_id);

                // If type variable is bound to a known type, return it
                if let Some(known) = self.binding.get(&target_type_var_id).copied() {
                    // If known type conflicts with class constraints, error out
                    if let Some(classes) = self.classes.get(&target_type_var_id)
                        && classes.len() == 1
                    {
                        let class = *classes.iter().next().unwrap();
                        let hir_type = self.type_arena.get_by_id(known.node).unwrap();
                        let possible_class = TyClass::from_type(hir_type);
                        if let Some(possible_class) = possible_class
                            && possible_class != class
                        {
                            let known_hir = hir_type.clone();
                            return Err(S::new(
                                Error::TypesConflict {
                                    first: MS {
                                        node: known_hir,
                                        span: known.span,
                                    },
                                    second: MS::new(class.fallback_type()),
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
                        let fallback_id = self.type_arena.get_or_insert(c.fallback_type());
                        let fallback = MS::new(fallback_id);
                        self.binding.insert(target_type_var_id, fallback);
                        return Ok(fallback);
                    }

                    // If multiple classes, error out as ambiguous
                    return Err(S::new(
                        Error::AmbiguousClass {
                            possible_classes: classes.iter().copied().collect(),
                        },
                        type_var.span,
                    ));
                }

                // If type variable has a Shape, build the struct type from it (fallback).
                // Each field is resolved with full error propagation — if a field's type
                // is unknown, the error is returned rather than silently substituting void.
                if let Some(s_shape) = self.shapes.get(&target_type_var_id).cloned() {
                    let Shape::Struct(fields) = s_shape.node;
                    let struct_hir = self.resolve_shape_fields_to_struct(fields)?;
                    let type_id = self.type_arena.get_or_insert(struct_hir);
                    let result = MS::new(type_id);
                    self.binding.insert(target_type_var_id, result);
                    return Ok(result);
                }

                // Otherwise, cannot resolve
                Err(S::new(Error::CantResolveType, type_var.span))
            }
        }
    }

    /// Resolves the type of a local variable by its `LocalId`.
    ///
    /// Looks up the `TypeVar` registered for `local_id` and delegates to
    /// `resolve`. Returns `UnregisteredLocalId` if no mapping exists.
    pub fn resolve_local(&mut self, local_id: LocalId) -> Result<MS<HIRTypeId>, S<Error>> {
        if let Some(tv) = self.local_ty.get(&local_id).cloned() {
            self.resolve(tv)
        } else {
            Err(S::zero(Error::UnregisteredLocalId { id: local_id }))
        }
    }

    /// Resolves the type of an expression by its `ExprId`.
    ///
    /// Looks up the `TypeVar` registered for `expr_id` and delegates to
    /// `resolve`. Returns `UnregisteredExprId` if no mapping exists.
    pub fn resolve_expr(&mut self, expr_id: ExprId) -> Result<MS<HIRTypeId>, S<Error>> {
        if let Some(tv) = self.expr_ty.get(&expr_id).cloned() {
            self.resolve(tv)
        } else {
            Err(S::zero(Error::UnregisteredExprId { id: expr_id }))
        }
    }
}

/// I generated these tests using LLMs as I'm a lazy bitch^-^
/// I checked it and it looks correct so I keep it. Just FYI.
#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{HIRType, StructField, StructType};
    use utils::primitive_types::PrimitiveType;

    // ---------- helpers ----------

    fn s<T>(node: T) -> S<T> {
        S {
            node,
            span: diagnostic::Span { start: 0, end: 0 },
        }
    }

    fn ms<T: Clone>(node: T) -> MS<T> {
        MS { node, span: None }
    }

    fn i32_hir() -> HIRType {
        HIRType::Primitive(PrimitiveType::I32)
    }

    fn f32_hir() -> HIRType {
        HIRType::Primitive(PrimitiveType::F32)
    }

    fn expr_id(n: usize) -> ExprId {
        ExprId::new(std::num::NonZeroUsize::new(n).unwrap())
    }

    fn local_id(n: usize) -> LocalId {
        LocalId::new(std::num::NonZeroUsize::new(n).unwrap())
    }

    fn tvar_id(n: usize) -> TypeVarId {
        TypeVarId::new(std::num::NonZeroUsize::new(n).unwrap())
    }

    // ---------- tests ----------

    #[test]
    fn eq_var_known_binds_and_resolves() {
        // constraints: v1 == i32
        // resolve_expr(e1) => i32
        let mut arena = HIRTypeArena::new();
        let i32_id = arena.get_or_insert(i32_hir());

        let e1 = expr_id(1);
        let v1 = tvar_id(1);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e1, s(TypeVar::Var(v1)));

        let local_ty = HashMap::new();

        let constraints = vec![Constraint::Eq(
            s(TypeVar::Var(v1)),
            s(TypeVar::Known(ms(i32_id))),
        )];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty(), "unexpected constraint errors: {errs:?}");

        let ty = solver.resolve_expr(e1).expect("resolve_expr failed");
        assert_eq!(ty.node, i32_id);
    }

    #[test]
    fn inclass_single_class_fallbacks() {
        // constraints: v1 ∈ Int
        // resolve_expr(e1) => fallback of Int (i32)
        let mut arena = HIRTypeArena::new();
        let i32_id = arena.get_or_insert(i32_hir());

        let e1 = expr_id(1);
        let v1 = tvar_id(1);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e1, s(TypeVar::Var(v1)));
        let local_ty = HashMap::new();

        let constraints = vec![Constraint::InClass(s(TypeVar::Var(v1)), TyClass::Int)];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty());

        let ty = solver.resolve_expr(e1).expect("resolve_expr failed");
        assert_eq!(ty.node, i32_id, "Int fallback should be i32");
    }

    #[test]
    fn inclass_multiple_classes_is_ambiguous() {
        // constraints: v1 ∈ Int, v1 ∈ Float
        // resolve_expr(e1) => Err(AmbiguousClass)
        let mut arena = HIRTypeArena::new();

        let e1 = expr_id(1);
        let v1 = tvar_id(1);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e1, s(TypeVar::Var(v1)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::InClass(s(TypeVar::Var(v1)), TyClass::Int),
            Constraint::InClass(s(TypeVar::Var(v1)), TyClass::Float),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty());

        let err = solver.resolve_expr(e1).unwrap_err();
        assert!(
            matches!(err.node, Error::AmbiguousClass { .. }),
            "expected AmbiguousClass, got: {:?}",
            err.node
        );
    }

    #[test]
    fn conflict_known_known_is_reported_during_new() {
        // constraints: i32 == f32 -> TypesConflict returned from Solver::new
        let mut arena = HIRTypeArena::new();
        let i32_id = arena.get_or_insert(i32_hir());
        let f32_id = arena.get_or_insert(f32_hir());

        let expr_ty = HashMap::new();
        let local_ty = HashMap::new();

        let constraints = vec![Constraint::Eq(
            s(TypeVar::Known(ms(i32_id))),
            s(TypeVar::Known(ms(f32_id))),
        )];

        let (_solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(!errs.is_empty(), "expected conflict error");
        let has_conflict = errs
            .iter()
            .any(|e| matches!(e.node, Error::TypesConflict { .. }));
        assert!(has_conflict, "expected TypesConflict, got: {errs:?}");
    }

    #[test]
    fn union_propagates_binding_across_vars() {
        // constraints: v1 == v2, v1 == i32
        // resolve_expr(e2) => i32
        let mut arena = HIRTypeArena::new();
        let i32_id = arena.get_or_insert(i32_hir());

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
            Constraint::Eq(s(TypeVar::Var(v1)), s(TypeVar::Known(ms(i32_id)))),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty());

        let ty2 = solver.resolve_expr(e2).expect("resolve_expr failed");
        assert_eq!(ty2.node, i32_id);
    }

    #[test]
    fn resolve_local_unregistered_yields_error() {
        let mut arena = HIRTypeArena::new();

        let expr_ty = HashMap::new();
        let local_ty = HashMap::new();
        let constraints = Vec::<Constraint>::new();

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty());

        let bogus_local = local_id(42);
        let err = solver.resolve_local(bogus_local).unwrap_err();
        assert!(
            matches!(err.node, Error::UnregisteredLocalId { .. }),
            "expected UnregisteredLocalId, got: {:?}",
            err.node
        );
    }

    #[test]
    fn bound_known_conflicts_with_inclass_mismatch() {
        // constraints: v1 == f32, v1 ∈ Int
        // resolve_expr(e1) => Err(TypesConflict) (known f32 violates class Int)
        //
        // This relies on TyClass::from_type(HIRType) to classify known types.
        let mut arena = HIRTypeArena::new();
        let f32_id = arena.get_or_insert(f32_hir());

        let e1 = expr_id(1);
        let v1 = tvar_id(1);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e1, s(TypeVar::Var(v1)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::Eq(s(TypeVar::Var(v1)), s(TypeVar::Known(ms(f32_id)))),
            Constraint::InClass(s(TypeVar::Var(v1)), TyClass::Int),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty(), "no constraint error expected during new");

        let err = solver.resolve_expr(e1).unwrap_err();
        assert!(
            matches!(err.node, Error::TypesConflict { .. }),
            "expected TypesConflict, got: {:?}",
            err.node
        );
    }

    #[test]
    fn inclass_on_both_vars_then_union_then_bind_resolves() {
        // constraints:
        //   v1 ∈ Int
        //   v2 ∈ Int
        //   v1 == v2
        //   v2 == i32
        // resolve_expr(e1) => i32, resolve_expr(e2) => i32
        let mut arena = HIRTypeArena::new();
        let i32_id = arena.get_or_insert(i32_hir());

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
            Constraint::Eq(s(TypeVar::Var(v2)), s(TypeVar::Known(ms(i32_id)))),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty());

        let t1 = solver.resolve_expr(e1).expect("resolve e1");
        let t2 = solver.resolve_expr(e2).expect("resolve e2");
        assert_eq!(t1.node, i32_id);
        assert_eq!(t2.node, i32_id);
    }

    #[test]
    fn chain_union_path_compression_still_resolves() {
        // constraints: v1==v2, v2==v3, v1==i32
        // resolve_expr(e3) => i32
        let mut arena = HIRTypeArena::new();
        let i32_id = arena.get_or_insert(i32_hir());

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
            Constraint::Eq(s(TypeVar::Var(v1)), s(TypeVar::Known(ms(i32_id)))),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty());

        let t3 = solver.resolve_expr(e3).expect("resolve e3");
        assert_eq!(t3.node, i32_id);
    }

    // ---------- struct helpers ----------

    /// Builds a HIRType::Struct with zero spans on all names and types.
    fn struct_hir(fields: &[(&str, HIRType)]) -> HIRType {
        HIRType::Struct(StructType {
            fields: fields
                .iter()
                .map(|(name, ty)| {
                    s(StructField {
                        name: s(name.to_string()),
                        ty: s(ty.clone()),
                    })
                })
                .collect(),
        })
    }

    /// Builds a Shape::Struct from a slice of (field_name, TypeVar) pairs.
    fn struct_shape(fields: &[(&str, TypeVar)]) -> Shape {
        Shape::Struct(
            fields
                .iter()
                .map(|(name, tv)| (s(name.to_string()), s(tv.clone())))
                .collect(),
        )
    }

    // ==========================
    // Group 1: HasShape basics
    // ==========================

    #[test]
    fn has_shape_struct_var_bound_by_known_resolves() {
        // HasShape(tv_v, {x: tv_x, y: tv_y}) + Eq(tv_v, Known(struct{x:i32, y:i32}))
        // => tv_v resolves to struct{x:i32, y:i32}
        let mut arena = HIRTypeArena::new();
        let struct_id = arena.get_or_insert(struct_hir(&[("x", i32_hir()), ("y", i32_hir())]));

        let e_v = expr_id(10);
        let v_v = tvar_id(10);
        let v_x = tvar_id(11);
        let v_y = tvar_id(12);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e_v, s(TypeVar::Var(v_v)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::HasShape(
                s(TypeVar::Var(v_v)),
                struct_shape(&[("x", TypeVar::Var(v_x)), ("y", TypeVar::Var(v_y))]),
            ),
            Constraint::Eq(s(TypeVar::Var(v_v)), s(TypeVar::Known(ms(struct_id)))),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");

        let ty = solver.resolve_expr(e_v).expect("resolve failed");
        assert_eq!(ty.node, struct_id);
    }

    #[test]
    fn has_shape_fields_get_types_from_known_struct() {
        // HasShape(tv_v, {x: tv_x, y: tv_y}) + Eq(tv_v, Known(struct{x:i32, y:i32}))
        // => tv_x and tv_y both resolve to i32 via structural unification
        let mut arena = HIRTypeArena::new();
        let i32_id = arena.get_or_insert(i32_hir());
        let struct_id = arena.get_or_insert(struct_hir(&[("x", i32_hir()), ("y", i32_hir())]));

        let e_v = expr_id(10);
        let e_x = expr_id(11);
        let e_y = expr_id(12);
        let v_v = tvar_id(10);
        let v_x = tvar_id(11);
        let v_y = tvar_id(12);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e_v, s(TypeVar::Var(v_v)));
        expr_ty.insert(e_x, s(TypeVar::Var(v_x)));
        expr_ty.insert(e_y, s(TypeVar::Var(v_y)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::HasShape(
                s(TypeVar::Var(v_v)),
                struct_shape(&[("x", TypeVar::Var(v_x)), ("y", TypeVar::Var(v_y))]),
            ),
            Constraint::Eq(s(TypeVar::Var(v_v)), s(TypeVar::Known(ms(struct_id)))),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");

        assert_eq!(
            solver.resolve_expr(e_x).expect("resolve e_x failed").node,
            i32_id,
            "field x should be i32"
        );
        assert_eq!(
            solver.resolve_expr(e_y).expect("resolve e_y failed").node,
            i32_id,
            "field y should be i32"
        );
    }

    #[test]
    fn has_shape_no_binding_fallback_builds_struct_from_field_vars() {
        // HasShape(tv_v, {x: tv_x, y: tv_y}) + InClass(Int) on both fields, no Eq with Known
        // => tv_v resolves to a struct type; both fields are i32 (Int fallback)
        let mut arena = HIRTypeArena::new();

        let e_v = expr_id(10);
        let v_v = tvar_id(10);
        let v_x = tvar_id(11);
        let v_y = tvar_id(12);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e_v, s(TypeVar::Var(v_v)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::HasShape(
                s(TypeVar::Var(v_v)),
                struct_shape(&[("x", TypeVar::Var(v_x)), ("y", TypeVar::Var(v_y))]),
            ),
            Constraint::InClass(s(TypeVar::Var(v_x)), TyClass::Int),
            Constraint::InClass(s(TypeVar::Var(v_y)), TyClass::Int),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");

        let ty_id = solver.resolve_expr(e_v).expect("resolve failed").node;
        drop(solver);

        let resolved = arena.get_by_id(ty_id).unwrap();
        let HIRType::Struct(st) = resolved else {
            panic!("expected Struct, got {resolved:?}");
        };
        assert_eq!(st.fields.len(), 2, "expected 2 fields");

        let mut field_names: Vec<&str> = st
            .fields
            .iter()
            .map(|f| f.node.name.node.as_str())
            .collect();
        field_names.sort();
        assert_eq!(field_names, vec!["x", "y"]);

        for field in &st.fields {
            assert!(
                matches!(field.node.ty.node, HIRType::Primitive(PrimitiveType::I32)),
                "field {} should be i32, got {:?}",
                field.node.name.node,
                field.node.ty.node
            );
        }
    }

    // ==========================
    // Group 2: Field order
    // ==========================

    #[test]
    fn field_order_matters_different_types() {
        // struct{x:i32, y:f32} and struct{y:f32, x:i32} must get distinct HIRTypeIds
        // because field order defines the memory layout.
        // Eq between them must produce TypesConflict.
        let mut arena = HIRTypeArena::new();
        let id_xy = arena.get_or_insert(struct_hir(&[("x", i32_hir()), ("y", f32_hir())]));
        let id_yx = arena.get_or_insert(struct_hir(&[("y", f32_hir()), ("x", i32_hir())]));

        assert_ne!(
            id_xy, id_yx,
            "different field orders must produce different type ids"
        );

        let expr_ty = HashMap::new();
        let local_ty = HashMap::new();
        let constraints = vec![Constraint::Eq(
            s(TypeVar::Known(ms(id_xy))),
            s(TypeVar::Known(ms(id_yx))),
        )];

        let (_solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(
            errs.iter()
                .any(|e| matches!(e.node, Error::TypesConflict { .. })),
            "expected TypesConflict for mismatched field orders, got: {errs:?}"
        );
    }

    #[test]
    fn literal_with_reversed_field_order_unifies_correctly() {
        // Simulates: let v: struct{x:i32, y:i32} = struct{y: 1, x: -1}
        // Shape from literal has y listed first, known type has x first.
        // Unification succeeds; tv_v resolves to struct{x:i32, y:i32}.
        let mut arena = HIRTypeArena::new();
        let i32_id = arena.get_or_insert(i32_hir());
        // Known type: x first, y second (canonical layout)
        let struct_id = arena.get_or_insert(struct_hir(&[("x", i32_hir()), ("y", i32_hir())]));

        let e_v = expr_id(10);
        let e_x = expr_id(11);
        let e_y = expr_id(12);
        let v_v = tvar_id(10);
        let v_x = tvar_id(11);
        let v_y = tvar_id(12);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e_v, s(TypeVar::Var(v_v)));
        expr_ty.insert(e_x, s(TypeVar::Var(v_x)));
        expr_ty.insert(e_y, s(TypeVar::Var(v_y)));
        let local_ty = HashMap::new();

        let constraints = vec![
            // Literal shape: y listed first inside the literal
            Constraint::HasShape(
                s(TypeVar::Var(v_v)),
                struct_shape(&[("y", TypeVar::Var(v_y)), ("x", TypeVar::Var(v_x))]),
            ),
            Constraint::InClass(s(TypeVar::Var(v_x)), TyClass::Int),
            Constraint::InClass(s(TypeVar::Var(v_y)), TyClass::Int),
            // Explicit annotation provides the canonical (x-first) layout
            Constraint::Eq(s(TypeVar::Var(v_v)), s(TypeVar::Known(ms(struct_id)))),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");

        assert_eq!(solver.resolve_expr(e_v).unwrap().node, struct_id);
        assert_eq!(
            solver.resolve_expr(e_x).unwrap().node,
            i32_id,
            "x should be i32"
        );
        assert_eq!(
            solver.resolve_expr(e_y).unwrap().node,
            i32_id,
            "y should be i32"
        );
    }

    #[test]
    fn field_type_inferred_transitively_through_struct() {
        // struct{x: v_field} where v_field has no annotation.
        // Eq(tv_v, Known(struct{x:i32})) => v_field resolves to i32 transitively.
        let mut arena = HIRTypeArena::new();
        let i32_id = arena.get_or_insert(i32_hir());
        let struct_id = arena.get_or_insert(struct_hir(&[("x", i32_hir())]));

        let e_v = expr_id(10);
        let e_field = expr_id(11);
        let v_v = tvar_id(10);
        let v_field = tvar_id(11); // unannotated variable used as field value

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e_v, s(TypeVar::Var(v_v)));
        expr_ty.insert(e_field, s(TypeVar::Var(v_field)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::HasShape(
                s(TypeVar::Var(v_v)),
                struct_shape(&[("x", TypeVar::Var(v_field))]),
            ),
            Constraint::Eq(s(TypeVar::Var(v_v)), s(TypeVar::Known(ms(struct_id)))),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");

        assert_eq!(
            solver.resolve_expr(e_field).unwrap().node,
            i32_id,
            "field var should be inferred as i32 from the known struct type"
        );
    }

    // =====================================
    // Group 3: Shape merging during union
    // =====================================

    #[test]
    fn union_of_two_vars_with_same_shape_merges_fields() {
        // tv_a: HasShape({x: tv_xa}), tv_b: HasShape({x: tv_xb})
        // Eq(tv_a, tv_b) -> union, shapes merged, tv_xa and tv_xb unified
        // Eq(tv_a, Known(struct{x:i32})) -> both e_a and e_b resolve to struct{x:i32}
        let mut arena = HIRTypeArena::new();
        let struct_id = arena.get_or_insert(struct_hir(&[("x", i32_hir())]));

        let e_a = expr_id(10);
        let e_b = expr_id(11);
        let v_a = tvar_id(10);
        let v_b = tvar_id(11);
        let v_xa = tvar_id(12);
        let v_xb = tvar_id(13);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e_a, s(TypeVar::Var(v_a)));
        expr_ty.insert(e_b, s(TypeVar::Var(v_b)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::HasShape(
                s(TypeVar::Var(v_a)),
                struct_shape(&[("x", TypeVar::Var(v_xa))]),
            ),
            Constraint::HasShape(
                s(TypeVar::Var(v_b)),
                struct_shape(&[("x", TypeVar::Var(v_xb))]),
            ),
            Constraint::Eq(s(TypeVar::Var(v_a)), s(TypeVar::Var(v_b))),
            Constraint::Eq(s(TypeVar::Var(v_a)), s(TypeVar::Known(ms(struct_id)))),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");

        assert_eq!(solver.resolve_expr(e_a).unwrap().node, struct_id);
        assert_eq!(solver.resolve_expr(e_b).unwrap().node, struct_id);
    }

    #[test]
    fn union_of_vars_with_overlapping_shapes_unifies_field_vars() {
        // tv_a: HasShape({x: tv_x1}), tv_b: HasShape({x: tv_x2})
        // Eq(tv_a, tv_b) -> tv_x1 and tv_x2 become the same union-find group
        // Eq(tv_x1, Known(i32)) -> tv_x2 also resolves to i32
        let mut arena = HIRTypeArena::new();
        let i32_id = arena.get_or_insert(i32_hir());

        let e_x2 = expr_id(13);
        let v_a = tvar_id(10);
        let v_b = tvar_id(11);
        let v_x1 = tvar_id(12);
        let v_x2 = tvar_id(13);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e_x2, s(TypeVar::Var(v_x2)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::HasShape(
                s(TypeVar::Var(v_a)),
                struct_shape(&[("x", TypeVar::Var(v_x1))]),
            ),
            Constraint::HasShape(
                s(TypeVar::Var(v_b)),
                struct_shape(&[("x", TypeVar::Var(v_x2))]),
            ),
            Constraint::Eq(s(TypeVar::Var(v_a)), s(TypeVar::Var(v_b))),
            Constraint::Eq(s(TypeVar::Var(v_x1)), s(TypeVar::Known(ms(i32_id)))),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");

        assert_eq!(
            solver.resolve_expr(e_x2).unwrap().node,
            i32_id,
            "tv_x2 should resolve to i32 via union with tv_x1"
        );
    }

    // ======================================
    // Group 4: Conflicts and error cases
    // ======================================

    #[test]
    fn shape_field_unknown_is_error() {
        // HasShape(tv_v, {x: tv_x}) + Eq(tv_v, Known(struct{y:i32}))
        // Literal has field "x", known type has "y" -> UnknownStructField{name:"x"}
        let mut arena = HIRTypeArena::new();
        let struct_id = arena.get_or_insert(struct_hir(&[("y", i32_hir())]));

        let e_v = expr_id(10);
        let v_v = tvar_id(10);
        let v_x = tvar_id(11);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e_v, s(TypeVar::Var(v_v)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::HasShape(
                s(TypeVar::Var(v_v)),
                struct_shape(&[("x", TypeVar::Var(v_x))]),
            ),
            Constraint::Eq(s(TypeVar::Var(v_v)), s(TypeVar::Known(ms(struct_id)))),
        ];

        let (mut solver, mut all_errors) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        if let Err(e) = solver.resolve_expr(e_v) {
            all_errors.push(e);
        }
        assert!(
            all_errors
                .iter()
                .any(|e| matches!(&e.node, Error::UnknownStructField { name } if name == "x")),
            "expected UnknownStructField{{name:\"x\"}}, got: {all_errors:?}"
        );
    }

    #[test]
    fn shape_field_missing_is_error() {
        // HasShape(tv_v, {x: tv_x}) + Eq(tv_v, Known(struct{x:i32, y:i32}))
        // Literal is missing field "y" -> MissingStructField{name:"y"}
        let mut arena = HIRTypeArena::new();
        let struct_id = arena.get_or_insert(struct_hir(&[("x", i32_hir()), ("y", i32_hir())]));

        let e_v = expr_id(10);
        let v_v = tvar_id(10);
        let v_x = tvar_id(11);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e_v, s(TypeVar::Var(v_v)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::HasShape(
                s(TypeVar::Var(v_v)),
                struct_shape(&[("x", TypeVar::Var(v_x))]),
            ),
            Constraint::Eq(s(TypeVar::Var(v_v)), s(TypeVar::Known(ms(struct_id)))),
        ];

        let (mut solver, mut all_errors) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        if let Err(e) = solver.resolve_expr(e_v) {
            all_errors.push(e);
        }
        assert!(
            all_errors
                .iter()
                .any(|e| matches!(&e.node, Error::MissingStructField { name } if name == "y")),
            "expected MissingStructField{{name:\"y\"}}, got: {all_errors:?}"
        );
    }

    #[test]
    fn shape_field_type_conflict_is_error() {
        // HasShape(tv_v, {x: tv_x}) + InClass(tv_x, Float) + Eq(tv_v, Known(struct{x:i32}))
        // Field x constrained to Float, but known struct says i32 -> TypesConflict
        let mut arena = HIRTypeArena::new();
        let struct_id = arena.get_or_insert(struct_hir(&[("x", i32_hir())]));

        let e_v = expr_id(10);
        let e_x = expr_id(11);
        let v_v = tvar_id(10);
        let v_x = tvar_id(11);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e_v, s(TypeVar::Var(v_v)));
        expr_ty.insert(e_x, s(TypeVar::Var(v_x)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::HasShape(
                s(TypeVar::Var(v_v)),
                struct_shape(&[("x", TypeVar::Var(v_x))]),
            ),
            Constraint::InClass(s(TypeVar::Var(v_x)), TyClass::Float),
            Constraint::Eq(s(TypeVar::Var(v_v)), s(TypeVar::Known(ms(struct_id)))),
        ];

        let (mut solver, mut all_errors) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        // TypesConflict on the field var may be deferred to resolve time
        if let Err(e) = solver.resolve_expr(e_x) {
            all_errors.push(e);
        }
        assert!(
            all_errors
                .iter()
                .any(|e| matches!(e.node, Error::TypesConflict { .. })),
            "expected TypesConflict on field x, got: {all_errors:?}"
        );
    }

    #[test]
    fn shape_mismatch_struct_vs_primitive_is_error() {
        // HasShape(tv_v, {x: tv_x}) + Eq(tv_v, Known(i32))
        // A variable with a struct shape cannot unify with a primitive -> ShapeOnNonStructType.
        // No InClass needed: the error is emitted immediately upon seeing the non-struct binding.
        let mut arena = HIRTypeArena::new();
        let i32_id = arena.get_or_insert(i32_hir());

        let e_v = expr_id(10);
        let v_v = tvar_id(10);
        let v_x = tvar_id(11);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e_v, s(TypeVar::Var(v_v)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::HasShape(
                s(TypeVar::Var(v_v)),
                struct_shape(&[("x", TypeVar::Var(v_x))]),
            ),
            Constraint::Eq(s(TypeVar::Var(v_v)), s(TypeVar::Known(ms(i32_id)))),
        ];

        let (_solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(
            errs.iter()
                .any(|e| matches!(e.node, Error::ShapeOnNonStructType { .. })),
            "expected ShapeOnNonStructType for struct shape vs primitive, got: {errs:?}"
        );
    }

    // ================================
    // Group 5: Nested struct types
    // ================================

    #[test]
    fn nested_struct_fields_resolve_correctly() {
        // let v: struct{ pos: struct{x:i32, y:i32} } = struct{ pos: struct{y:1, x:0} }
        //
        // tv_v:   HasShape({pos: tv_pos}) + Eq(Known(outer_struct_id))
        // tv_pos: HasShape({x: tv_x, y: tv_y})
        // tv_x, tv_y: InClass(Int)
        //
        // => tv_pos resolves to struct{x:i32, y:i32}
        // => tv_x and tv_y resolve to i32
        let mut arena = HIRTypeArena::new();
        let i32_id = arena.get_or_insert(i32_hir());
        let inner_struct_id =
            arena.get_or_insert(struct_hir(&[("x", i32_hir()), ("y", i32_hir())]));
        let outer_struct_id = arena.get_or_insert(struct_hir(&[(
            "pos",
            struct_hir(&[("x", i32_hir()), ("y", i32_hir())]),
        )]));

        let e_v = expr_id(10);
        let e_pos = expr_id(11);
        let e_x = expr_id(12);
        let e_y = expr_id(13);
        let v_v = tvar_id(10);
        let v_pos = tvar_id(11);
        let v_x = tvar_id(12);
        let v_y = tvar_id(13);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e_v, s(TypeVar::Var(v_v)));
        expr_ty.insert(e_pos, s(TypeVar::Var(v_pos)));
        expr_ty.insert(e_x, s(TypeVar::Var(v_x)));
        expr_ty.insert(e_y, s(TypeVar::Var(v_y)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::HasShape(
                s(TypeVar::Var(v_v)),
                struct_shape(&[("pos", TypeVar::Var(v_pos))]),
            ),
            Constraint::HasShape(
                s(TypeVar::Var(v_pos)),
                struct_shape(&[("x", TypeVar::Var(v_x)), ("y", TypeVar::Var(v_y))]),
            ),
            Constraint::InClass(s(TypeVar::Var(v_x)), TyClass::Int),
            Constraint::InClass(s(TypeVar::Var(v_y)), TyClass::Int),
            Constraint::Eq(s(TypeVar::Var(v_v)), s(TypeVar::Known(ms(outer_struct_id)))),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");

        assert_eq!(solver.resolve_expr(e_v).unwrap().node, outer_struct_id);
        assert_eq!(
            solver.resolve_expr(e_pos).unwrap().node,
            inner_struct_id,
            "pos field should resolve to struct{{x:i32, y:i32}}"
        );
        assert_eq!(
            solver.resolve_expr(e_x).unwrap().node,
            i32_id,
            "x should be i32"
        );
        assert_eq!(
            solver.resolve_expr(e_y).unwrap().node,
            i32_id,
            "y should be i32"
        );
    }

    #[test]
    fn has_shape_after_eq_known_reconciles() {
        // Eq(tv_v, Known(struct{x:i32, y:i32})) arrives BEFORE HasShape(tv_v, {x: tv_x, y: tv_y}).
        // reconcile must still happen — field vars must be bound to i32.
        let mut arena = HIRTypeArena::new();
        let i32_id = arena.get_or_insert(i32_hir());
        let struct_id = arena.get_or_insert(struct_hir(&[("x", i32_hir()), ("y", i32_hir())]));

        let e_v = expr_id(10);
        let e_x = expr_id(11);
        let e_y = expr_id(12);
        let v_v = tvar_id(10);
        let v_x = tvar_id(11);
        let v_y = tvar_id(12);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e_v, s(TypeVar::Var(v_v)));
        expr_ty.insert(e_x, s(TypeVar::Var(v_x)));
        expr_ty.insert(e_y, s(TypeVar::Var(v_y)));
        let local_ty = HashMap::new();

        // Note: Eq comes BEFORE HasShape — this is the order being tested
        let constraints = vec![
            Constraint::Eq(s(TypeVar::Var(v_v)), s(TypeVar::Known(ms(struct_id)))),
            Constraint::HasShape(
                s(TypeVar::Var(v_v)),
                struct_shape(&[("x", TypeVar::Var(v_x)), ("y", TypeVar::Var(v_y))]),
            ),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");

        assert_eq!(solver.resolve_expr(e_v).unwrap().node, struct_id);
        assert_eq!(
            solver.resolve_expr(e_x).unwrap().node,
            i32_id,
            "x should be i32"
        );
        assert_eq!(
            solver.resolve_expr(e_y).unwrap().node,
            i32_id,
            "y should be i32"
        );
    }

    #[test]
    fn union_shape_on_a_binding_on_b_reconciles_after_union() {
        // HasShape(tv_a, {x: tv_x})  →  shape on tv_a, no binding
        // Eq(tv_b, Known(struct{x:i32}))  →  binding on tv_b, no shape → no reconcile yet
        // Eq(tv_a, tv_b)  →  union: shape moves from tv_a to tv_b root (Some, None branch)
        //
        // Bug: (Some, None) branch in union never calls reconcile_shape_with_known,
        // so tv_x is never unified with i32.
        let mut arena = HIRTypeArena::new();
        let i32_id = arena.get_or_insert(i32_hir());
        let struct_id = arena.get_or_insert(struct_hir(&[("x", i32_hir())]));

        let e_x = expr_id(11);
        let v_a = tvar_id(10);
        let v_b = tvar_id(11);
        let v_x = tvar_id(12);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e_x, s(TypeVar::Var(v_x)));
        let local_ty = HashMap::new();

        let constraints = vec![
            // shape on v_a
            Constraint::HasShape(
                s(TypeVar::Var(v_a)),
                struct_shape(&[("x", TypeVar::Var(v_x))]),
            ),
            // binding on v_b (no shape)
            Constraint::Eq(s(TypeVar::Var(v_b)), s(TypeVar::Known(ms(struct_id)))),
            // union: shape from v_a must reconcile with binding already on v_b
            Constraint::Eq(s(TypeVar::Var(v_a)), s(TypeVar::Var(v_b))),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");

        assert_eq!(
            solver.resolve_expr(e_x).unwrap().node,
            i32_id,
            "tv_x should be i32 via reconcile triggered during union"
        );
    }

    #[test]
    fn union_both_shapes_one_with_binding_no_duplicate_errors() {
        // tv_a: HasShape({x}) + Known(struct{x:i32})  →  shape + binding
        // tv_b: HasShape({x, z})                      →  shape only, z is NOT in struct{x}
        // Eq(tv_a, tv_b)  →  union merges both shapes and reconciles with the known type.
        //
        // Field "z" appears only in tv_b's shape but not in struct{x:i32}, so exactly one
        // UnknownStructField{z} error must be reported — the union logic must not report it
        // twice by naively reconciling and merging independently.
        let mut arena = HIRTypeArena::new();
        let struct_id = arena.get_or_insert(struct_hir(&[("x", i32_hir())]));

        let v_a = tvar_id(10);
        let v_b = tvar_id(11);
        let v_xa = tvar_id(12);
        let v_xb = tvar_id(13);
        let v_zb = tvar_id(14);

        let expr_ty = HashMap::new();
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::HasShape(
                s(TypeVar::Var(v_a)),
                struct_shape(&[("x", TypeVar::Var(v_xa))]),
            ),
            Constraint::Eq(s(TypeVar::Var(v_a)), s(TypeVar::Known(ms(struct_id)))),
            Constraint::HasShape(
                s(TypeVar::Var(v_b)),
                struct_shape(&[("x", TypeVar::Var(v_xb)), ("z", TypeVar::Var(v_zb))]),
            ),
            Constraint::Eq(s(TypeVar::Var(v_a)), s(TypeVar::Var(v_b))),
        ];

        let (_solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);

        let unknown_z_count = errs
            .iter()
            .filter(|e| matches!(&e.node, Error::UnknownStructField { name } if name == "z"))
            .count();
        assert_eq!(
            unknown_z_count, 1,
            "UnknownStructField{{z}} should appear exactly once, got {unknown_z_count} times: {errs:?}"
        );
    }

    #[test]
    fn resolve_shape_with_unknown_field_type_is_error_not_void_struct() {
        // HasShape(tv_v, {x: tv_x}) — tv_x has no type info at all (no Eq, no InClass).
        // When resolving tv_v via the shape fallback path, each field must be resolved with
        // full error propagation. If tv_x cannot be resolved, the error must bubble up
        // rather than being silently replaced with void, producing a CantResolveType error.
        let mut arena = HIRTypeArena::new();

        let e_v = expr_id(10);
        let v_v = tvar_id(10);
        let v_x = tvar_id(11);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e_v, s(TypeVar::Var(v_v)));
        let local_ty = HashMap::new();

        let constraints = vec![Constraint::HasShape(
            s(TypeVar::Var(v_v)),
            struct_shape(&[("x", TypeVar::Var(v_x))]),
        )];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty(), "unexpected constraint errors: {errs:?}");

        let result = solver.resolve_expr(e_v);
        assert!(
            result.is_err(),
            "expected error resolving struct with unknown field type, got: {result:?}"
        );
    }

    #[test]
    fn named_type_with_struct_resolves_shape() {
        // type Pos = struct { x: i32, y: i32 }
        // HasShape(tv_expr, {x: tv_x, y: tv_y}) + Eq(tv_expr, Known(Named("Pos", Struct{x:i32,y:i32})))
        // After reconciliation, tv_x and tv_y must resolve to i32.
        let mut arena = HIRTypeArena::new();
        let i32_id = arena.get_or_insert(i32_hir());
        let pos_id = arena.get_or_insert(HIRType::Named(
            "Pos".to_string(),
            Box::new(struct_hir(&[("x", i32_hir()), ("y", i32_hir())])),
        ));

        let e_expr = expr_id(1);
        let v_expr = tvar_id(1);
        let v_x = tvar_id(2);
        let v_y = tvar_id(3);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e_expr, s(TypeVar::Var(v_expr)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::Eq(s(TypeVar::Var(v_expr)), s(TypeVar::Known(ms(pos_id)))),
            Constraint::HasShape(
                s(TypeVar::Var(v_expr)),
                struct_shape(&[("x", TypeVar::Var(v_x)), ("y", TypeVar::Var(v_y))]),
            ),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty(), "unexpected constraint errors: {errs:?}");

        // Use a fresh local mapped to v_x/v_y to test resolution
        solver.local_ty.insert(local_id(10), s(TypeVar::Var(v_x)));
        solver.local_ty.insert(local_id(11), s(TypeVar::Var(v_y)));

        let x_result = solver
            .resolve_local(local_id(10))
            .expect("tv_x should resolve");
        let y_result = solver
            .resolve_local(local_id(11))
            .expect("tv_y should resolve");
        assert_eq!(x_result.node, i32_id, "tv_x should be i32");
        assert_eq!(y_result.node, i32_id, "tv_y should be i32");
    }

    #[test]
    fn chained_named_type_resolves_shape() {
        // type Pos = struct { x: i32 }
        // type MyPos = Pos  (i.e. Named("MyPos", Named("Pos", Struct{x:i32})))
        // HasShape(tv_expr, {x: tv_x}) + Eq(tv_expr, Known(MyPos))
        // tv_x must resolve to i32 after peeling two Named layers.
        let mut arena = HIRTypeArena::new();
        let i32_id = arena.get_or_insert(i32_hir());
        let my_pos_id = arena.get_or_insert(HIRType::Named(
            "MyPos".to_string(),
            Box::new(HIRType::Named(
                "Pos".to_string(),
                Box::new(struct_hir(&[("x", i32_hir())])),
            )),
        ));

        let e_expr = expr_id(1);
        let v_expr = tvar_id(1);
        let v_x = tvar_id(2);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e_expr, s(TypeVar::Var(v_expr)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::Eq(s(TypeVar::Var(v_expr)), s(TypeVar::Known(ms(my_pos_id)))),
            Constraint::HasShape(
                s(TypeVar::Var(v_expr)),
                struct_shape(&[("x", TypeVar::Var(v_x))]),
            ),
        ];

        let (mut solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(errs.is_empty(), "unexpected constraint errors: {errs:?}");

        solver.local_ty.insert(local_id(10), s(TypeVar::Var(v_x)));
        let x_result = solver
            .resolve_local(local_id(10))
            .expect("tv_x should resolve");
        assert_eq!(
            x_result.node, i32_id,
            "tv_x should be i32 through Named chain"
        );
    }

    #[test]
    fn named_non_struct_with_shape_is_conflict() {
        // type MyId = u32
        // HasShape(tv_expr, {x: tv_x}) + Eq(tv_expr, Known(Named("MyId", Primitive(u32))))
        // Must report ShapeOnNonStructType — MyId is not a struct.
        // No InClass or ordering workaround needed: the error is emitted immediately.
        let mut arena = HIRTypeArena::new();
        let my_id_id = arena.get_or_insert(HIRType::Named(
            "MyId".to_string(),
            Box::new(HIRType::Primitive(
                utils::primitive_types::PrimitiveType::U32,
            )),
        ));

        let e_expr = expr_id(1);
        let v_expr = tvar_id(1);
        let v_x = tvar_id(2);

        let mut expr_ty = HashMap::new();
        expr_ty.insert(e_expr, s(TypeVar::Var(v_expr)));
        let local_ty = HashMap::new();

        let constraints = vec![
            Constraint::Eq(s(TypeVar::Var(v_expr)), s(TypeVar::Known(ms(my_id_id)))),
            Constraint::HasShape(
                s(TypeVar::Var(v_expr)),
                struct_shape(&[("x", TypeVar::Var(v_x))]),
            ),
        ];

        let (_solver, errs) = Solver::new(constraints, expr_ty, local_ty, &mut arena);
        assert!(
            errs.iter()
                .any(|e| matches!(&e.node, Error::ShapeOnNonStructType { .. })),
            "expected ShapeOnNonStructType for Named(primitive) + HasShape, got: {errs:?}"
        );
    }
}
