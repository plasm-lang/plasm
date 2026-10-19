use std::collections::{BTreeSet, HashMap};

use diagnostic::{MaybeSpanned, Span, Spanned};
use utils::ids::{ExprId, FuncId, HIRTypeId, LocalId};

use super::constraint_gen::{
    Access, Constraint, ConstraintSet, InferTypeMaps, signature_eq_constraints,
};
use super::engine::ModuleCtx;
use super::error::TypeInferenceError;
use super::type_class::TypeClass;
use super::type_var::{Binding, InferType, TypeVarId};
use super::union_find::UnionFind;
use super::work_list::WorkList;
use crate::types::{HIRType, HIRTypeArena, StructField, StructType, TupleType};

// For brevity
type S<T> = Spanned<T>;
type MS<T> = MaybeSpanned<T>;

pub fn solve_function_types(
    constraints: ConstraintSet,
    arena: &mut HIRTypeArena,
    module_ctx: &ModuleCtx,
) -> (Solution, Vec<S<TypeInferenceError>>) {
    FunctionTypeSolver::new(arena, constraints, module_ctx).solve()
}

struct FunctionTypeSolver<'a> {
    arena: &'a mut HIRTypeArena,
    module_ctx: &'a ModuleCtx,
    union_find: UnionFind,
    work_list: WorkList,
    maps: InferTypeMaps,
    callees: HashMap<ExprId, FuncId>,
    errors: Vec<S<TypeInferenceError>>,
}

impl<'a> FunctionTypeSolver<'a> {
    fn new(
        arena: &'a mut HIRTypeArena,
        constraints: ConstraintSet,
        module_ctx: &'a ModuleCtx,
    ) -> Self {
        Self {
            arena,
            module_ctx,
            union_find: UnionFind::new(),
            work_list: WorkList::new(constraints.constraints),
            maps: constraints.maps,
            callees: HashMap::new(),
            errors: Vec::new(),
        }
    }

    fn solve(mut self) -> (Solution, Vec<S<TypeInferenceError>>) {
        self.process_work_list();
        // Fallback types are guesses, so they are applied only when nothing
        // else can be inferred.
        while self.apply_fallback_types() {
            self.process_work_list();
        }
        self.report_freezed_accesses();
        let solution = self.generate_solution();
        (solution, self.errors)
    }

    fn process_work_list(&mut self) {
        while let Some(constraint) = self.work_list.pop() {
            self.process_constraint(constraint);
        }
    }

    fn process_constraint(&mut self, constraint: Constraint) {
        match constraint {
            Constraint::Equality(first, second) => self.unify(first, second),
            Constraint::Binding(infer_type, binding) => {
                self.bind(infer_type, binding);
            }
            Constraint::Access(access) => self.process_access(access),
        }
    }

    fn unify(&mut self, first: S<InferType>, second: S<InferType>) {
        let span = first.span.max(second.span);
        match (first.node, second.node) {
            (InferType::Var(first_var), InferType::Var(second_var)) => {
                self.merge(first_var, second_var, span);
            }
            (infer_type, InferType::Known(type_id))
            | (InferType::Known(type_id), infer_type) => {
                self.bind(S::new(infer_type, span), Binding::Known(type_id));
            }
        }
    }

    fn merge(&mut self, first: TypeVarId, second: TypeVarId, span: Span) {
        let Some(merged) = self.union_find.union(first, second) else {
            return;
        };
        self.work_list.merge(merged.absorbed, merged.into);
        if let Some(binding) = merged.absorbed_binding {
            self.bind_root(merged.into, binding, span);
        }
        // Constraints frozen on the absorbed root haven't seen the binding of
        // the `into` root yet.
        if self.union_find.binding_of(merged.into).is_some() {
            self.work_list.unfreeze(merged.into);
        }
    }

    /// Adds knowledge about the type.
    fn bind(&mut self, infer_type: S<InferType>, binding: Binding) {
        let (infer_type, span) = infer_type.unwrap();
        match infer_type {
            InferType::Known(type_id) => {
                self.validate_binding(type_id, binding, span);
            }
            InferType::Var(type_var_id) => {
                let root = self.union_find.find(type_var_id);
                self.bind_root(root, binding, span);
            }
        }
    }

    fn bind_root(&mut self, root: TypeVarId, binding: Binding, span: Span) {
        let refined = match self.union_find.binding_of(root).cloned() {
            Some(current) => self.join(current, binding, span),
            None => Some(binding),
        };
        if let Some(refined) = refined {
            self.union_find.bind(root, refined);
            self.work_list.unfreeze(root);
        }
    }

    /// Checks that the new binding is compatible with the current one.
    /// Returns the new binding if it's more specific than the current one.
    fn join(
        &mut self,
        current: Binding,
        new: Binding,
        span: Span,
    ) -> Option<Binding> {
        match (current, new) {
            (Binding::Known(type_id), new) => {
                self.validate_binding(type_id, new, span);
                None
            }
            (current, Binding::Known(type_id)) => {
                self.validate_binding(type_id, current, span);
                Some(Binding::Known(type_id))
            }
            (Binding::Class(current_class), Binding::Class(new_class)) => {
                if current_class != new_class {
                    let error = TypeInferenceError::MultipleTypeClasses {
                        classes: vec![current_class, new_class],
                    };
                    self.errors.push(S::new(error, span));
                }
                None
            }
            (
                Binding::StructShape(current_fields),
                Binding::StructShape(new_fields),
            ) => {
                let struct_type = self.format_struct_shape(&current_fields);
                self.validate_struct_shapes_compatibility(
                    new_fields.into_iter().collect(),
                    current_fields.into_iter().collect(),
                    span,
                    struct_type,
                );
                None
            }
            (
                Binding::TupleShape(current_elements),
                Binding::TupleShape(new_elements),
            ) => {
                let tuple_type = self.format_tuple_shape(&current_elements);
                self.validate_tuple_shapes_compatibility(
                    new_elements,
                    current_elements,
                    span,
                    tuple_type,
                );
                None
            }
            (Binding::Class(class), shape) | (shape, Binding::Class(class)) => {
                let error = TypeInferenceError::IncompatibleTypeClass {
                    ty: self.format_binding(&shape),
                    class,
                };
                self.errors.push(S::new(error, span));
                None
            }
            (current, Binding::StructShape(_)) => {
                let error = TypeInferenceError::ShapeOnNonStructType {
                    ty: self.format_binding(&current),
                };
                self.errors.push(S::new(error, span));
                None
            }
            (current, Binding::TupleShape(_)) => {
                let error = TypeInferenceError::ShapeOnNonTupleType {
                    ty: self.format_binding(&current),
                };
                self.errors.push(S::new(error, span));
                None
            }
        }
    }

    /// Checks that the binding is compatible with the known type.
    fn validate_binding(
        &mut self,
        type_id: HIRTypeId,
        binding: Binding,
        span: Span,
    ) {
        match binding {
            Binding::Known(other_type_id) => {
                if type_id != other_type_id {
                    self.push_type_conflict(type_id, other_type_id, span);
                }
            }
            Binding::Class(class) => self.validate_type_class(type_id, class, span),
            Binding::StructShape(fields) => {
                self.validate_struct_shape(type_id, fields, span);
            }
            Binding::TupleShape(elements) => {
                self.validate_tuple_shape(type_id, elements, span);
            }
        }
    }

    fn process_access(&mut self, access: Access) {
        let type_var_id = match access.base().node {
            InferType::Known(type_id) => {
                self.access_known_type(access, type_id);
                return;
            }
            InferType::Var(type_var_id) => type_var_id,
        };
        let root = self.union_find.find(type_var_id);
        match (self.union_find.binding_of(root).cloned(), access) {
            (Some(Binding::Known(type_id)), access) => {
                self.access_known_type(access, type_id);
            }
            (
                Some(Binding::StructShape(fields)),
                Access::Field {
                    base,
                    field_name,
                    field_type,
                },
            ) => self.access_field(base, fields, field_name, field_type),
            (
                Some(Binding::TupleShape(elements)),
                Access::Index {
                    base,
                    index,
                    element_type,
                },
            ) => self.access_element(base, elements, index, element_type),
            // The base type isn't specific enough, so wait for its binding
            (_, access) => self.work_list.freeze(access, root),
        }
    }

    fn access_known_type(&mut self, access: Access, type_id: HIRTypeId) {
        match access {
            Access::Field {
                base,
                field_name,
                field_type,
            } => {
                let Some(fields) = self.struct_fields(type_id) else {
                    let error = TypeInferenceError::FieldOnNonStructType {
                        ty: self.format_type(type_id),
                        field_name: field_name.node,
                    };
                    self.errors.push(S::new(error, field_name.span));
                    return;
                };
                self.access_field(base, fields, field_name, field_type);
            }
            Access::Index {
                base,
                index,
                element_type,
            } => {
                let Some(elements) = self.tuple_elements(type_id) else {
                    let error = TypeInferenceError::IndexOnNonTupleType {
                        ty: self.format_type(type_id),
                        index: index.node,
                    };
                    self.errors.push(S::new(error, index.span));
                    return;
                };
                self.access_element(base, elements, index, element_type);
            }
            Access::Method {
                base,
                expr_id,
                method_name,
                args,
                return_type,
            } => self.access_method(
                base,
                type_id,
                expr_id,
                method_name,
                args,
                return_type,
            ),
        }
    }

    fn access_field(
        &mut self,
        base: S<InferType>,
        fields: Vec<(S<String>, S<InferType>)>,
        field_name: S<String>,
        field_type: S<InferType>,
    ) {
        let Some((_, field)) =
            fields.into_iter().find(|(name, _)| *name == field_name)
        else {
            let error = TypeInferenceError::UnknownStructField {
                struct_type: self.format_infer_type(base.node),
                field_name: field_name.node,
            };
            self.errors.push(S::new(error, field_name.span));
            return;
        };
        self.work_list.push(Constraint::Equality(field_type, field));
    }

    fn access_element(
        &mut self,
        base: S<InferType>,
        elements: Vec<S<InferType>>,
        index: S<usize>,
        element_type: S<InferType>,
    ) {
        let Some(element) = elements.get(index.node) else {
            let error = TypeInferenceError::ImpossibleTupleIndex {
                tuple_type: self.format_infer_type(base.node),
                index: index.node,
            };
            self.errors.push(S::new(error, index.span));
            return;
        };
        self.work_list
            .push(Constraint::Equality(element_type, *element));
    }

    /// The base is passed to the method as the first argument.
    fn access_method(
        &mut self,
        base: S<InferType>,
        base_type_id: HIRTypeId,
        expr_id: ExprId,
        method_name: S<String>,
        args: Vec<S<InferType>>,
        return_type: S<InferType>,
    ) {
        let Some(func_id) = self
            .module_ctx
            .func_map
            .get_func_id(&method_name.node, Some(base_type_id))
        else {
            let error = TypeInferenceError::UnknownMethod {
                ty: self.format_type(base_type_id),
                method_name: method_name.node,
            };
            self.errors.push(S::new(error, method_name.span));
            return;
        };
        let signature = self.module_ctx.func_signatures.get(&func_id).unwrap();

        let args: Vec<S<InferType>> = std::iter::once(base).chain(args).collect();
        if args.len() != signature.args.len() {
            let error = TypeInferenceError::ArgumentsCountMismatch {
                function: format!(
                    "{}.{}",
                    self.format_type(base_type_id),
                    method_name.node
                ),
                expected: signature.args.len(),
                actual: args.len(),
            };
            self.errors.push(S::new(error, method_name.span));
        }

        // Matching arguments and the return type are bound anyway to avoid
        // cascading errors.
        for constraint in
            signature_eq_constraints(signature, args.into_iter(), return_type)
        {
            self.work_list.push(constraint);
        }
        self.callees.insert(expr_id, func_id);
    }

    /// Applies fallback types to types that frozen constraints aren't going to
    /// bind. Returns `false` if there is nothing to apply.
    fn apply_fallback_types(&mut self) -> bool {
        let awaited = self.awaited_roots();
        let candidates: Vec<(TypeVarId, Binding)> = self
            .union_find
            .bindings()
            .filter(|(root, binding)| {
                !matches!(binding, Binding::Known(_)) && !awaited.contains(root)
            })
            .map(|(root, binding)| (root, binding.clone()))
            .collect();

        // All fallback types are computed before any of them is applied, so
        // they don't depend on the order of type variables.
        let fallbacks: Vec<(TypeVarId, HIRTypeId)> = candidates
            .into_iter()
            .filter_map(|(root, binding)| {
                Some((root, self.fallback_type(&binding)?))
            })
            .collect();

        for (root, type_id) in fallbacks.iter() {
            // Fallback types are compatible with the bindings by construction
            self.union_find.bind(*root, Binding::Known(*type_id));
            self.work_list.unfreeze(*root);
        }
        !fallbacks.is_empty()
    }

    /// Roots that frozen accesses are going to bind, together with elements of
    /// their shapes.
    fn awaited_roots(&mut self) -> BTreeSet<TypeVarId> {
        let mut to_visit: Vec<InferType> = self
            .work_list
            .freezed()
            .flat_map(|(_, access)| access.accessed_types())
            .map(|infer_type| infer_type.node)
            .collect();
        let mut awaited = BTreeSet::new();
        while let Some(infer_type) = to_visit.pop() {
            let InferType::Var(type_var_id) = infer_type else {
                continue;
            };
            let root = self.union_find.find(type_var_id);
            if !awaited.insert(root) {
                continue;
            }
            match self.union_find.binding_of(root) {
                Some(Binding::StructShape(fields)) => {
                    to_visit.extend(fields.iter().map(|(_, field)| field.node));
                }
                Some(Binding::TupleShape(elements)) => {
                    to_visit.extend(elements.iter().map(|element| element.node));
                }
                _ => {}
            }
        }
        awaited
    }

    /// Returns `None` for a shape until all its elements are known.
    fn fallback_type(&mut self, binding: &Binding) -> Option<HIRTypeId> {
        match binding {
            Binding::Known(_) => None,
            Binding::Class(class) => {
                Some(self.arena.get_or_insert(class.fallback_type()))
            }
            Binding::StructShape(fields) => self.materialize_struct_shape(fields),
            Binding::TupleShape(elements) => self.materialize_tuple_shape(elements),
        }
    }

    /// Accesses that are still frozen have bases that can't be inferred.
    fn report_freezed_accesses(&mut self) {
        for access in self.work_list.take_freezed() {
            let error = match access {
                Access::Field { field_name, .. } => S::new(
                    TypeInferenceError::FieldOnUnknownType {
                        field_name: field_name.node,
                    },
                    field_name.span,
                ),
                Access::Index { index, .. } => S::new(
                    TypeInferenceError::IndexOnUnknownType { index: index.node },
                    index.span,
                ),
                Access::Method { method_name, .. } => S::new(
                    TypeInferenceError::MethodOnUnknownType {
                        method_name: method_name.node,
                    },
                    method_name.span,
                ),
            };
            self.errors.push(error);
        }
    }

    fn push_type_conflict(
        &mut self,
        first: HIRTypeId,
        second: HIRTypeId,
        span: Span,
    ) {
        let error = TypeInferenceError::TypesConflict {
            first: self.format_type(first),
            second: self.format_type(second),
        };
        self.errors.push(S::new(error, span));
    }

    fn validate_type_class(
        &mut self,
        type_id: HIRTypeId,
        class: TypeClass,
        span: Span,
    ) {
        let ty = self.arena.get_by_id(type_id).unwrap();
        if !class.is_compatible_with(ty) {
            let error = TypeInferenceError::IncompatibleTypeClass {
                ty: ty.format(self.arena),
                class,
            };
            self.errors.push(S::new(error, span));
        }
    }

    fn validate_struct_shape(
        &mut self,
        type_id: HIRTypeId,
        shape_fields: Vec<(S<String>, S<InferType>)>,
        type_span: Span,
    ) {
        let ty = self.format_type(type_id);
        let Some(struct_fields) = self.struct_fields(type_id) else {
            let error = TypeInferenceError::ShapeOnNonStructType { ty };
            self.errors.push(S::new(error, type_span));
            return;
        };

        self.validate_struct_shapes_compatibility(
            shape_fields.into_iter().collect(),
            struct_fields.into_iter().collect(),
            type_span,
            ty,
        );
    }

    fn validate_tuple_shape(
        &mut self,
        type_id: HIRTypeId,
        shape_elements: Vec<S<InferType>>,
        type_span: Span,
    ) {
        let ty = self.format_type(type_id);
        let Some(type_elements) = self.tuple_elements(type_id) else {
            let error = TypeInferenceError::ShapeOnNonTupleType { ty };
            self.errors.push(S::new(error, type_span));
            return;
        };

        self.validate_tuple_shapes_compatibility(
            shape_elements,
            type_elements,
            type_span,
            ty,
        );
    }

    fn validate_struct_shapes_compatibility(
        &mut self,
        first_shape_map: HashMap<S<String>, S<InferType>>,
        second_shape_map: HashMap<S<String>, S<InferType>>,
        literal_span: Span,
        struct_type: String,
    ) {
        for (name, infer_type) in first_shape_map.iter() {
            if !second_shape_map.contains_key(name) {
                let error = TypeInferenceError::UnknownStructField {
                    struct_type: struct_type.clone(),
                    field_name: name.node.clone(),
                };
                self.errors
                    .push(S::new(error, name.span.join(infer_type.span)));
            }
        }

        for name in second_shape_map.keys() {
            if !first_shape_map.contains_key(name) {
                let error = TypeInferenceError::MissingStructField {
                    struct_type: struct_type.clone(),
                    field_name: name.node.clone(),
                };
                self.errors.push(S::new(error, literal_span));
            }
        }

        for (name, first_field_type) in first_shape_map.iter() {
            if let Some(second_field_type) = second_shape_map.get(name) {
                let new_constraint =
                    Constraint::Equality(*first_field_type, *second_field_type);
                self.work_list.push(new_constraint);
            }
        }
    }

    fn validate_tuple_shapes_compatibility(
        &mut self,
        first_shape: Vec<S<InferType>>,
        second_shape: Vec<S<InferType>>,
        literal_span: Span,
        tuple_type: String,
    ) {
        if first_shape.len() != second_shape.len() {
            let span = first_shape
                .iter()
                .map(|e| e.span)
                .reduce(|s1, s2| s1.join(s2))
                .unwrap_or(literal_span);
            let error = TypeInferenceError::TupleShapeLengthMismatch {
                ty: tuple_type,
                expected: second_shape.len(),
                actual: first_shape.len(),
            };
            self.errors.push(S::new(error, span));
            return;
        }

        for (first_element, second_element) in
            first_shape.into_iter().zip(second_shape)
        {
            let new_constraint = Constraint::Equality(first_element, second_element);
            self.work_list.push(new_constraint);
        }
    }

    /// Fields of a known struct type in the form of a struct shape.
    fn struct_fields(
        &self,
        type_id: HIRTypeId,
    ) -> Option<Vec<(S<String>, S<InferType>)>> {
        let ty = self.arena.get_by_id(type_id).unwrap();
        let HIRType::Struct(struct_type) = ty.peel_named() else {
            return None;
        };
        let fields = struct_type
            .fields
            .iter()
            .map(|field| (field.name.clone(), field.ty_id.map(InferType::Known)))
            .collect();
        Some(fields)
    }

    /// Elements of a known tuple type in the form of a tuple shape.
    fn tuple_elements(&self, type_id: HIRTypeId) -> Option<Vec<S<InferType>>> {
        let ty = self.arena.get_by_id(type_id).unwrap();
        let HIRType::Tuple(TupleType(elements)) = ty.peel_named() else {
            return None;
        };
        let elements = elements
            .iter()
            .map(|element| element.map(InferType::Known))
            .collect();
        Some(elements)
    }

    fn materialize_struct_shape(
        &mut self,
        fields: &[(S<String>, S<InferType>)],
    ) -> Option<HIRTypeId> {
        let mut struct_fields = Vec::with_capacity(fields.len());
        for (name, field_infer_type) in fields {
            let field_type_id =
                Self::get_type_id(&mut self.union_find, field_infer_type.node)?;
            let struct_field = StructField {
                name: name.clone(),
                ty_id: S::new(field_type_id, field_infer_type.span),
            };
            struct_fields.push(S::new(struct_field, name.span));
        }
        let hir_type = HIRType::Struct(StructType {
            fields: struct_fields,
        });
        let type_id = self.arena.get_or_insert(hir_type);
        Some(type_id)
    }

    fn materialize_tuple_shape(
        &mut self,
        elements: &[S<InferType>],
    ) -> Option<HIRTypeId> {
        let mut tuple_elements = Vec::with_capacity(elements.len());
        for element_infer_type in elements {
            let element_type_id =
                Self::get_type_id(&mut self.union_find, element_infer_type.node)?;
            tuple_elements.push(S::new(element_type_id, element_infer_type.span));
        }
        let hir_type = HIRType::Tuple(TupleType(tuple_elements));
        let type_id = self.arena.get_or_insert(hir_type);
        Some(type_id)
    }

    fn binding_of(&mut self, infer_type: InferType) -> Option<Binding> {
        match infer_type {
            InferType::Known(type_id) => Some(Binding::Known(type_id)),
            InferType::Var(type_var_id) => {
                self.union_find.binding_of(type_var_id).cloned()
            }
        }
    }

    fn get_type_id(
        union_find: &mut UnionFind,
        infer_type: InferType,
    ) -> Option<HIRTypeId> {
        match infer_type {
            InferType::Known(type_id) => Some(type_id),
            InferType::Var(type_var_id) => {
                match union_find.binding_of(type_var_id) {
                    Some(Binding::Known(type_id)) => Some(*type_id),
                    _ => None,
                }
            }
        }
    }

    fn format_type(&self, type_id: HIRTypeId) -> String {
        self.arena.get_by_id(type_id).unwrap().format(self.arena)
    }

    /// Formats a type for error messages. Unknown types are shown as `_`.
    fn format_infer_type(&mut self, infer_type: InferType) -> String {
        match self.binding_of(infer_type) {
            Some(binding) => self.format_binding(&binding),
            None => "_".to_string(),
        }
    }

    fn format_binding(&mut self, binding: &Binding) -> String {
        match binding {
            Binding::Known(type_id) => self.format_type(*type_id),
            Binding::Class(_) => "_".to_string(),
            Binding::StructShape(fields) => self.format_struct_shape(fields),
            Binding::TupleShape(elements) => self.format_tuple_shape(elements),
        }
    }

    fn format_struct_shape(
        &mut self,
        fields: &[(S<String>, S<InferType>)],
    ) -> String {
        let fields_str = fields
            .iter()
            .map(|(name, field)| {
                format!("{}: {}", name.node, self.format_shape_element(field.node))
            })
            .collect::<Vec<_>>()
            .join(", ");
        format!("struct {{ {fields_str} }}")
    }

    fn format_tuple_shape(&mut self, elements: &[S<InferType>]) -> String {
        let elements_str = elements
            .iter()
            .map(|element| self.format_shape_element(element.node))
            .collect::<Vec<_>>()
            .join(", ");
        format!("({elements_str})")
    }

    /// Nested shapes aren't formatted, because a shape can contain itself until
    /// infinite types are detected.
    fn format_shape_element(&mut self, infer_type: InferType) -> String {
        match Self::get_type_id(&mut self.union_find, infer_type) {
            Some(type_id) => self.format_type(type_id),
            None => "_".to_string(),
        }
    }

    fn generate_solution(&mut self) -> Solution {
        let mut solution = Solution {
            expr_ty: HashMap::new(),
            local_ty: HashMap::new(),
            callees: std::mem::take(&mut self.callees),
        };

        for (expr_id, infer_type) in self.maps.expr_ty.iter() {
            if let Some(type_id) =
                Self::get_type_id(&mut self.union_find, infer_type.node)
            {
                solution
                    .expr_ty
                    .insert(*expr_id, MS::new(type_id).with_span(infer_type.span));
            }
        }

        for (local_id, infer_type) in self.maps.local_ty.iter() {
            if let Some(type_id) =
                Self::get_type_id(&mut self.union_find, infer_type.node)
            {
                solution
                    .local_ty
                    .insert(*local_id, MS::new(type_id).with_span(infer_type.span));
            }
        }

        solution
    }
}

#[derive(Debug)]
pub struct Solution {
    pub expr_ty: HashMap<ExprId, MS<HIRTypeId>>,
    pub local_ty: HashMap<LocalId, MS<HIRTypeId>>,
    /// Functions called by method calls, found by the types of the receivers.
    pub callees: HashMap<ExprId, FuncId>,
}

// --- Debug Helpers --- //

#[allow(dead_code)]
#[cfg(debug_assertions)]
impl FunctionTypeSolver<'_> {
    const FREEZED_EDGE: &'static str = "style=dashed, color=\"#c0392b\"";
    const KNOWN_COLOR: &'static str = "#d4edda";
    const PARTIAL_COLOR: &'static str = "#fff3cd";
    const SHAPE_EDGE: &'static str = "style=solid";
    const UNKNOWN_COLOR: &'static str = "#f0f0f0";

    pub fn print_dot(&mut self) {
        println!("{}", self.to_dot());
    }

    /// Nodes are groups of type variables with their bindings. Solid edges lead
    /// to shape elements, dashed edges are frozen field and index accesses.
    pub fn to_dot(&mut self) -> String {
        let type_vars: BTreeSet<TypeVarId> = self
            .maps
            .expr_ty
            .values()
            .chain(self.maps.local_ty.values())
            .filter_map(|infer_type| match infer_type.node {
                InferType::Var(type_var_id) => Some(type_var_id),
                InferType::Known(_) => None,
            })
            .chain(self.union_find.type_vars())
            .collect();
        let mut groups: std::collections::BTreeMap<TypeVarId, Vec<TypeVarId>> =
            std::collections::BTreeMap::new();
        for type_var_id in type_vars {
            let root = self.union_find.find(type_var_id);
            groups.entry(root).or_default().push(type_var_id);
        }

        let mut lines = Vec::new();
        for (root, type_vars) in groups {
            let type_vars_str = type_vars
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ");
            let binding = self.union_find.binding_of(root).cloned();
            let (binding_str, color) = match binding {
                Some(Binding::Known(type_id)) => {
                    (self.format_type(type_id), Self::KNOWN_COLOR)
                }
                Some(Binding::Class(class)) => {
                    (format!("∈ {class:?}"), Self::PARTIAL_COLOR)
                }
                Some(Binding::StructShape(fields)) => {
                    for (name, field) in fields.iter() {
                        let label = format!(".{}", name.node);
                        let edge = self.dot_edge(
                            root,
                            field.node,
                            &label,
                            Self::SHAPE_EDGE,
                        );
                        lines.extend(edge);
                    }
                    ("struct shape".to_string(), Self::PARTIAL_COLOR)
                }
                Some(Binding::TupleShape(elements)) => {
                    for (index, element) in elements.iter().enumerate() {
                        let label = format!(".{index}");
                        let edge = self.dot_edge(
                            root,
                            element.node,
                            &label,
                            Self::SHAPE_EDGE,
                        );
                        lines.extend(edge);
                    }
                    ("tuple shape".to_string(), Self::PARTIAL_COLOR)
                }
                None => ("?".to_string(), Self::UNKNOWN_COLOR),
            };
            lines.push(format!(
                "    {root} [label=\"{type_vars_str}\\n{binding_str}\", fillcolor=\"{color}\"];"
            ));
        }

        let freezed: Vec<(TypeVarId, String, InferType)> = self
            .work_list
            .freezed()
            .flat_map(|(root, access)| {
                let label = match access {
                    Access::Field { field_name, .. } => {
                        format!(".{}", field_name.node)
                    }
                    Access::Index { index, .. } => format!(".{}", index.node),
                    Access::Method { method_name, .. } => {
                        format!(".{}(...)", method_name.node)
                    }
                };

                access
                    .accessed_types()
                    .into_iter()
                    .map(move |accessed_type| {
                        (root, label.clone(), accessed_type.node)
                    })
            })
            .collect();
        for (root, label, output) in freezed {
            lines.extend(self.dot_edge(root, output, &label, Self::FREEZED_EDGE));
        }

        let node_style = "    node [shape=box, style=\"rounded,filled\", fontname=\"Helvetica\"];";
        let edge_style = "    edge [color=\"#555555\", fontname=\"Helvetica\"];";
        format!(
            "digraph Types {{\n{}\n{}\n\n{}\n}}\n",
            node_style,
            edge_style,
            lines.join("\n")
        )
    }

    /// Known types inside shapes and accesses are drawn as separate nodes.
    fn dot_edge(
        &mut self,
        from: TypeVarId,
        to: InferType,
        label: &str,
        attributes: &str,
    ) -> Vec<String> {
        match to {
            InferType::Var(type_var_id) => {
                let root = self.union_find.find(type_var_id);
                vec![format!(
                    "    {from} -> {root} [label=\"{label}\", {attributes}];"
                )]
            }
            InferType::Known(type_id) => {
                let node = format!("\"{from}{label}\"");
                vec![
                    format!(
                        "    {node} [label=\"{}\", fillcolor=\"{}\"];",
                        self.format_type(type_id),
                        Self::KNOWN_COLOR
                    ),
                    format!(
                        "    {from} -> {node} [label=\"{label}\", {attributes}];"
                    ),
                ]
            }
        }
    }
}
