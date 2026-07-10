use std::collections::{HashMap, HashSet};

use diagnostic::{MaybeSpanned, Span, Spanned};
use utils::ids::{ExprId, HIRTypeId, LocalId, TypeVarId};

use super::type_var::{Constraint, Shape, TyClass, TypeVar, Attribute};
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

    attributes: HashMap<TypeVarId, Vec<Attribute>>,

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
            attributes: HashMap::new(),
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
                Constraint::HasAttribute(type_var, attribute) => {
                    todo!()
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
