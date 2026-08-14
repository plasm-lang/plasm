use std::collections::HashMap;

use diagnostic::{MaybeSpanned, Span, Spanned};
use utils::ids::{ExprId, HIRTypeId, LocalId};

use super::constraint_gen::{Constraint, ConstraintSet, InferTypeMaps};
use super::error::TypeInferenceError;
use super::type_class::TypeClass;
use super::type_var::InferType;
use super::union_find::{TypesConflictError, UnificationOutcome, UnionFind};
use super::work_list::WorkList;
use crate::types::{HIRType, HIRTypeArena, StructField, StructType, TupleType};

// For brevity
type S<T> = Spanned<T>;
type MS<T> = MaybeSpanned<T>;

pub fn solve_function_types(
    constraints: ConstraintSet,
    arena: &mut HIRTypeArena,
) -> (Solution, Vec<S<TypeInferenceError>>) {
    FunctionTypeSolver::new(arena, constraints).solve()
}

struct FunctionTypeSolver<'a> {
    arena: &'a mut HIRTypeArena,
    union_find: UnionFind,
    work_list: WorkList,
    maps: InferTypeMaps,
    errors: Vec<S<TypeInferenceError>>,
}

impl<'a> FunctionTypeSolver<'a> {
    fn new(arena: &'a mut HIRTypeArena, constraints: ConstraintSet) -> Self {
        Self {
            arena,
            union_find: UnionFind::new(),
            work_list: WorkList::new(constraints.constraints),
            maps: constraints.maps,
            errors: Vec::new(),
        }
    }

    fn solve(mut self) -> (Solution, Vec<S<TypeInferenceError>>) {
        loop {
            if let Some(constraint) = self.work_list.pop() {
                self.process_constraint(constraint);
            } else if let Some(constraint) =
                self.work_list.pop_freezed_leaf(&mut self.union_find)
            {
                self.process_freezed_constraint(constraint);
            } else {
                break;
            }
        }
        let solution = self.generate_solution();
        (solution, self.errors)
    }

    fn process_constraint(&mut self, constraint: Constraint) {
        match constraint {
            Constraint::Equality(first, second) => {
                let (first_node, first_span) = first.unwrap();
                let (second_node, second_span) = second.unwrap();
                let res = self.union_find.unify(first_node, second_node);
                match res {
                    Ok(UnificationOutcome::Merged { absorbed, into }) => {
                        self.work_list.merge(absorbed, into);
                    }
                    Ok(UnificationOutcome::Bound { root }) => {
                        self.work_list.unfreeze(root);
                    }
                    Ok(UnificationOutcome::NoOp) => {}
                    Err(error) => {
                        let span = first_span.max(second_span);
                        self.push_unification_error(error, span);
                    }
                }
            }
            Constraint::InClass(spanned, type_class) => {
                let (infer_type, span) = spanned.unwrap();
                if let Some(type_id) = self.infer_type_to_type_id(infer_type) {
                    self.validate_type_class(type_id, type_class, span);
                } else {
                    self.work_list.freeze(
                        constraint,
                        self.union_find.find(infer_type.type_var_id_unchecked()),
                    );
                }
            }
            Constraint::StructShape(spanned, items) => {
                let (infer_type, span) = spanned.unwrap();
                if let Some(type_id) = self.infer_type_to_type_id(infer_type) {
                    self.validate_struct_shape(type_id, items, span);
                } else {
                    let constraint =
                        Constraint::StructShape(S::new(infer_type, span), items);
                    self.work_list.freeze(
                        constraint,
                        self.union_find.find(infer_type.type_var_id_unchecked()),
                    );
                }
            }
            Constraint::TupleShape(spanned, elements) => {
                let (infer_type, span) = spanned.unwrap();
                if let Some(type_id) = self.infer_type_to_type_id(infer_type) {
                    self.validate_tuple_shape(type_id, elements, span);
                } else {
                    let constraint =
                        Constraint::TupleShape(S::new(infer_type, span), elements);
                    self.work_list.freeze(
                        constraint,
                        self.union_find.find(infer_type.type_var_id_unchecked()),
                    );
                }
            }
            Constraint::HasField {
                base,
                field_name,
                field_type,
            } => {
                let (base_infer_type, base_span) = base.unwrap();
                if let Some(type_id) = self.infer_type_to_type_id(base_infer_type) {
                    self.validate_has_field(type_id, field_name, field_type);
                } else {
                    let constraint = Constraint::HasField {
                        base: S::new(base_infer_type, base_span),
                        field_name,
                        field_type,
                    };
                    self.work_list.freeze(
                        constraint,
                        self.union_find
                            .find(base_infer_type.type_var_id_unchecked()),
                    );
                }
            }
            Constraint::HasIndex {
                base,
                index,
                element_type,
            } => {
                let (base_infer_type, base_span) = base.unwrap();
                if let Some(type_id) = self.infer_type_to_type_id(base_infer_type) {
                    self.validate_has_index(type_id, index, element_type);
                } else {
                    let constraint = Constraint::HasIndex {
                        base: S::new(base_infer_type, base_span),
                        index,
                        element_type,
                    };
                    self.work_list.freeze(
                        constraint,
                        self.union_find
                            .find(base_infer_type.type_var_id_unchecked()),
                    );
                }
            }
        }
    }

    fn process_freezed_constraint(&mut self, constraint: Constraint) {
        match constraint {
            Constraint::Equality(..) => self.process_constraint(constraint),
            Constraint::InClass(spanned, type_class) => {
                let (infer_type, span) = spanned.unwrap();
                if let Some(type_id) = self.infer_type_to_type_id(infer_type) {
                    self.validate_type_class(type_id, type_class, span);
                } else {
                    let fallback_type = type_class.fallback_type();
                    let fallback_type_id = self.arena.get_or_insert(fallback_type);
                    let new_constraint = Constraint::Equality(
                        S::new(infer_type, span),
                        S::new(InferType::Known(fallback_type_id), span),
                    );
                    self.work_list.push(new_constraint);
                }
            }
            Constraint::StructShape(spanned, items) => {
                let (infer_type, span) = spanned.unwrap();
                if let Some(type_id) = self.infer_type_to_type_id(infer_type) {
                    self.validate_struct_shape(type_id, items, span);
                } else {
                    let Some(fallback_type_id) =
                        self.materialize_struct_shape(&items)
                    else {
                        return;
                    };
                    let new_constraint = Constraint::Equality(
                        S::new(infer_type, span),
                        S::new(InferType::Known(fallback_type_id), span),
                    );
                    self.work_list.push(new_constraint);
                }
            }
            Constraint::TupleShape(spanned, elements) => {
                let (infer_type, span) = spanned.unwrap();
                if let Some(type_id) = self.infer_type_to_type_id(infer_type) {
                    self.validate_tuple_shape(type_id, elements, span);
                } else {
                    let Some(fallback_type_id) =
                        self.materialize_tuple_shape(&elements)
                    else {
                        return;
                    };
                    let new_constraint = Constraint::Equality(
                        S::new(infer_type, span),
                        S::new(InferType::Known(fallback_type_id), span),
                    );
                    self.work_list.push(new_constraint);
                }
            }
            Constraint::HasField {
                base,
                field_name,
                field_type,
            } => {
                if let Some(type_id) = self.infer_type_to_type_id(base.node) {
                    self.validate_has_field(type_id, field_name, field_type);
                } else {
                    let error = TypeInferenceError::FieldOnUnknownType {
                        field_name: field_name.node,
                    };
                    self.errors.push(S::new(error, field_name.span));
                }
            }
            Constraint::HasIndex {
                base,
                index,
                element_type,
            } => {
                if let Some(type_id) = self.infer_type_to_type_id(base.node) {
                    self.validate_has_index(type_id, index, element_type);
                } else {
                    let error =
                        TypeInferenceError::IndexOnUnknownType { index: index.node };
                    self.errors.push(S::new(error, index.span));
                }
            }
        }
    }

    fn infer_type_to_type_id(&mut self, infer_type: InferType) -> Option<HIRTypeId> {
        match infer_type {
            InferType::Known(type_id) => Some(type_id),
            InferType::Var(type_var_id) => self.union_find.binding_of(type_var_id),
        }
    }

    fn push_unification_error(&mut self, error: TypesConflictError, span: Span) {
        let TypesConflictError(first, second) = error;
        self.push_type_conflict(first, second, span);
    }

    fn push_type_conflict(
        &mut self,
        first: HIRTypeId,
        second: HIRTypeId,
        span: Span,
    ) {
        let first_ty = self.arena.get_by_id(first).unwrap().format(self.arena);
        let second_ty = self.arena.get_by_id(second).unwrap().format(self.arena);
        let error = TypeInferenceError::TypesConflict {
            first: first_ty,
            second: second_ty,
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
        let ty = self.arena.get_by_id(type_id).unwrap();

        let HIRType::Struct(struct_type) = ty.peel_named() else {
            let error = TypeInferenceError::ShapeOnNonStructType {
                ty: ty.format(self.arena),
            };
            self.errors.push(S::new(error, type_span));
            return;
        };

        let struct_fields = struct_type
            .fields
            .iter()
            .map(|field| (field.name.clone(), field.ty_id.map(InferType::Known)))
            .collect();

        self.validate_struct_shapes_compatibility(
            shape_fields.into_iter().collect(),
            struct_fields,
            type_span,
            ty.clone(),
        );
    }

    fn validate_tuple_shape(
        &mut self,
        type_id: HIRTypeId,
        shape_elements: Vec<S<InferType>>,
        type_span: Span,
    ) {
        let ty = self.arena.get_by_id(type_id).unwrap();

        let HIRType::Tuple(TupleType(type_elements)) = ty.peel_named() else {
            let error = TypeInferenceError::ShapeOnNonTupleType {
                ty: ty.format(self.arena),
            };
            self.errors.push(S::new(error, type_span));
            return;
        };

        if shape_elements.len() != type_elements.len() {
            let span = shape_elements
                .iter()
                .map(|e| e.span)
                .reduce(|s1, s2| s1.join(s2))
                .unwrap_or(type_span);
            let error = TypeInferenceError::TupleShapeLengthMismatch {
                ty: ty.format(self.arena),
                expected: type_elements.len(),
                actual: shape_elements.len(),
            };
            self.errors.push(S::new(error, span));
            return;
        }

        for (shape_element, type_element) in
            shape_elements.iter().zip(type_elements.iter())
        {
            let new_constraint = Constraint::Equality(
                *shape_element,
                type_element.map(InferType::Known),
            );
            self.work_list.push(new_constraint);
        }
    }

    fn validate_has_field(
        &mut self,
        base_type_id: HIRTypeId,
        field_name: S<String>,
        field_type: S<InferType>,
    ) {
        let base_type = self.arena.get_by_id(base_type_id).unwrap();
        let HIRType::Struct(struct_type) = base_type.peel_named() else {
            let error = TypeInferenceError::FieldOnNonStructType {
                ty: base_type.format(self.arena),
                field_name: field_name.node.clone(),
            };
            self.errors.push(S::new(error, field_name.span));
            return;
        };

        for field in struct_type.fields.iter() {
            if field.name == field_name {
                let new_constraint = Constraint::Equality(
                    field_type,
                    field.ty_id.map(InferType::Known),
                );
                self.work_list.push(new_constraint);
                return;
            }
        }

        let error = TypeInferenceError::UnknownStructField {
            struct_type: base_type.format(self.arena),
            field_name: field_name.node.clone(),
        };
        self.errors.push(S::new(error, field_name.span));
    }

    fn validate_has_index(
        &mut self,
        base_type_id: HIRTypeId,
        index: S<usize>,
        element_type: S<InferType>,
    ) {
        let base_type = self.arena.get_by_id(base_type_id).unwrap();
        let HIRType::Tuple(tuple_type) = base_type.peel_named() else {
            let error = TypeInferenceError::IndexOnNonTupleType {
                ty: base_type.format(self.arena),
                index: index.node,
            };
            self.errors.push(S::new(error, index.span));
            return;
        };

        if index.node >= tuple_type.0.len() {
            let error = TypeInferenceError::ImpossibleTupleIndex {
                tuple_type: base_type.format(self.arena),
                index: index.node,
            };
            self.errors.push(S::new(error, index.span));
            return;
        }

        let element = tuple_type.0.get(index.node).unwrap();
        let new_constraint =
            Constraint::Equality(element_type, element.map(InferType::Known));
        self.work_list.push(new_constraint);
    }

    fn validate_struct_shapes_compatibility(
        &mut self,
        first_shape_map: HashMap<S<String>, S<InferType>>,
        second_shape_map: HashMap<S<String>, S<InferType>>,
        literal_span: Span,
        ty: HIRType,
    ) {
        for (name, infer_type) in first_shape_map.iter() {
            if !second_shape_map.contains_key(name) {
                let error = TypeInferenceError::UnknownStructField {
                    struct_type: ty.format(self.arena),
                    field_name: name.node.clone(),
                };
                self.errors
                    .push(S::new(error, name.span.join(infer_type.span)));
            }
        }

        for name in second_shape_map.keys() {
            if !first_shape_map.contains_key(name) {
                let error = TypeInferenceError::MissingStructField {
                    struct_type: ty.format(self.arena),
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

    fn materialize_struct_shape(
        &mut self,
        fields: &[(S<String>, S<InferType>)],
    ) -> Option<HIRTypeId> {
        let mut struct_fields = Vec::with_capacity(fields.len());
        for (name, field_infer_type) in fields {
            let Some(field_type_id) =
                Self::get_type_id(&mut self.union_find, field_infer_type.node)
            else {
                let err = S::new(
                    TypeInferenceError::CantResolveType,
                    field_infer_type.span,
                );
                self.errors.push(err);
                return None;
            };
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
            let Some(element_type_id) =
                Self::get_type_id(&mut self.union_find, element_infer_type.node)
            else {
                let err = S::new(
                    TypeInferenceError::CantResolveType,
                    element_infer_type.span,
                );
                self.errors.push(err);
                return None;
            };
            tuple_elements.push(S::new(element_type_id, element_infer_type.span));
        }
        let hir_type = HIRType::Tuple(TupleType(tuple_elements));
        let type_id = self.arena.get_or_insert(hir_type);
        Some(type_id)
    }

    fn get_type_id(
        union_find: &mut UnionFind,
        infer_type: InferType,
    ) -> Option<HIRTypeId> {
        match infer_type {
            InferType::Known(type_id) => Some(type_id),
            InferType::Var(type_var_id) => union_find.binding_of(type_var_id),
        }
    }

    fn generate_solution(&mut self) -> Solution {
        let mut solution = Solution {
            expr_ty: HashMap::new(),
            local_ty: HashMap::new(),
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
}
