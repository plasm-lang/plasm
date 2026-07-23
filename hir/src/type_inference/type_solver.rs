//! ```text
//! +----------------------------------------------------+
//! |                 FunctionTypeSolver                 |
//! +----------------------------------------------------+
//! |   |                                                |
//! |   | step 1.              +---------+               |
//! |   +--- Vec<Equality> --> | Unifier |               |
//! |   |                      +---------+               |
//! |   | step 2.                                        |
//! |   +--- Vec<Obligation> --> validate_obligations()  |
//! |   |                                                |
//! |   | step 3.                                        |
//! |   +--- InferTypeMaps --> generate_solution()       |
//! |                                                    |
//! |                                                    |
//! |   generate_solution() -> Solution                  |
//! |   | step 1.                                        |
//! |   +---> Try to get type using Unifier              |
//! |   | step2.                                         |
//! |   +---> try to get fallback type using obligations |
//! |   | error.                                         |
//! |   +---> TypeInferenceError::CantResolveType        |
//! |                                                    |
//! +----------------------------------------------------+
//! ```
//!
//! TODO: Move out obligation-related logic (validation + fallback generation)
//! to a separate module, e.g. `ObligationSolver` or `Obligator`.

use std::collections::HashMap;

use diagnostic::{MaybeSpanned, Span, Spanned};
use utils::ids::{ExprId, HIRTypeId, LocalId, TypeVarId};

use super::constraint_gen::{Constraints, Equality, InferTypeMaps};
use super::error::TypeInferenceError;
use super::type_class::TypeClass;
use super::type_var::InferType;
use super::unifier::{Unifier, UnifyError};
use crate::type_inference::constraint_gen::Obligation;
use crate::types::{HIRType, HIRTypeArena, StructField, StructType};

// For brevity
type S<T> = Spanned<T>;
type MS<T> = MaybeSpanned<T>;

pub fn solve_function_types(
    constraints: Constraints,
    arena: &mut HIRTypeArena,
) -> (Solution, Vec<S<TypeInferenceError>>) {
    FunctionTypeSolver::new().solve(constraints, arena)
}

struct FunctionTypeSolver {
    unifier: Unifier,
    classes: HashMap<TypeVarId, TypeClass>,
    struct_shapes: HashMap<TypeVarId, Vec<(S<String>, S<InferType>)>>,
}

impl FunctionTypeSolver {
    pub fn new() -> Self {
        Self {
            unifier: Unifier::new(),
            classes: HashMap::new(),
            struct_shapes: HashMap::new(),
        }
    }

    /// Algorithm:
    /// 1. Unify all equalities using `Unifier`.
    /// 2. Validate result of step 1 with obligations.
    /// 3. Generate fallback types for type variables that are not bound to a
    ///    concrete type if it's possible.
    pub fn solve(
        mut self,
        constraints: Constraints,
        arena: &mut HIRTypeArena,
    ) -> (Solution, Vec<S<TypeInferenceError>>) {
        print_constraints(&constraints, arena);
        let Constraints {
            equalities,
            obligations,
            maps,
        } = constraints;

        // Step 1: Unify equalities
        let unification_errors = self.unify_equalities(equalities, arena);

        // Step 2: Validate obligations
        let obligation_errors = self.validate_obligations(obligations, arena);

        // Step 3: Generate fallback types and `Solution`.
        // This step may be combined with step 2, se there will be not need in
        // `self.classes` state, but I split it just for readability.
        let (solution, solution_errors) = self.generate_solution(&maps, arena);

        let errors = unification_errors
            .into_iter()
            .chain(obligation_errors)
            .chain(solution_errors)
            .collect();
        (solution, errors)
    }

    fn unify_equalities(
        &mut self,
        equalities: Vec<Equality>,
        arena: &mut HIRTypeArena,
    ) -> Vec<S<TypeInferenceError>> {
        let errors = self
            .unifier
            .unify_equalities(equalities)
            .into_iter()
            .map(|unify_error| self.unify_error_into_type_error(unify_error, arena))
            .collect::<Vec<_>>();
        print_unifier_state(&self.unifier, arena);
        errors
    }

    fn unify_error_into_type_error(
        &mut self,
        unify_error: S<UnifyError>,
        arena: &mut HIRTypeArena,
    ) -> S<TypeInferenceError> {
        match unify_error.node {
            UnifyError::Conflict(a, b) => {
                let type_id_a = match self.get_type_id(&a, arena) {
                    Ok(type_id) => type_id,
                    Err(e) => return e,
                };
                let hir_type_a =
                    type_id_a.map(|id| arena.get_by_id(id).unwrap().clone());
                let type_id_b = match self.get_type_id(&b, arena) {
                    Ok(type_id) => type_id,
                    Err(e) => return e,
                };
                let hir_type_b = type_id_b.map(|id| arena.get_by_id(id).unwrap());
                let error = TypeInferenceError::TypesConflict {
                    first: hir_type_a.map(|ty| ty.format(arena)),
                    second: hir_type_b.map(|ty| ty.format(arena)),
                };
                S::new(error, unify_error.span)
            }
            UnifyError::Recursion(a, b) => todo!(),
        }
    }

    fn validate_obligations(
        &mut self,
        obligations: Vec<Obligation>,
        arena: &HIRTypeArena,
    ) -> Vec<S<TypeInferenceError>> {
        let mut obligation_errors = Vec::new();
        for obligation in obligations.into_iter() {
            match obligation {
                Obligation::InClass(infer_type, class) => {
                    if let Some(error) =
                        self.validate_in_class_obligation(infer_type, class, arena)
                    {
                        obligation_errors.push(error);
                    }
                }
                Obligation::StructShape(infer_type, fields) => {
                    let errors = self
                        .validate_struct_shape_obligation(infer_type, fields, arena);
                    obligation_errors.extend(errors);
                }
            }
        }
        obligation_errors
    }

    /// This function is almost the same as `validate_in_class_obligation`.
    /// This should be encapsulated in ObligationSolver or something like this.
    fn validate_struct_shape_obligation(
        &mut self,
        infer_type: S<InferType>,
        shape_fields: Vec<(S<String>, S<InferType>)>,
        arena: &HIRTypeArena,
    ) -> Vec<S<TypeInferenceError>> {
        match infer_type.node {
            InferType::Scalar(type_id) => {
                let ty_str = arena.get_by_id(type_id).unwrap().format(arena);
                let err = TypeInferenceError::ShapeOnNonStructType { ty: ty_str };
                vec![S::new(err, infer_type.span)]
            }
            InferType::Var(type_var_id) => {
                self.add_struct_shape(type_var_id, shape_fields.clone());

                let Some(binding_infer_type) = self.unifier.binding_of(type_var_id)
                else {
                    // If not bound, we will generate fallback type later
                    return Vec::new();
                };

                if binding_infer_type == &infer_type {
                    return Vec::new();
                }

                match binding_infer_type.node.clone() {
                    InferType::Var(binding_type_var_id) => {
                        self.add_struct_shape(binding_type_var_id, shape_fields);
                        Vec::new()
                    }
                    InferType::Scalar(type_id) => {
                        let ty_str = arena.get_by_id(type_id).unwrap().format(arena);
                        let err =
                            TypeInferenceError::ShapeOnNonStructType { ty: ty_str };
                        vec![S::new(err, binding_infer_type.span)]
                    }
                    InferType::Struct(fields) => {
                        self.validate_struct_shape(shape_fields, fields)
                    }
                }
            }
            InferType::Struct(fields) => {
                self.validate_struct_shape(shape_fields, fields)
            }
        }
    }

    /// Order of fields doesn't matter here.
    fn validate_struct_shape(
        &mut self,
        shape_fields: Vec<(S<String>, S<InferType>)>,
        struct_fields: Vec<(S<String>, S<InferType>)>,
    ) -> Vec<S<TypeInferenceError>> {
        let shape_fields_map = shape_fields.into_iter().collect::<HashMap<_, _>>();
        let struct_fields_map = struct_fields.into_iter().collect::<HashMap<_, _>>();

        let unknown_struct_field_errors = shape_fields_map
            .keys()
            .filter(|name| !struct_fields_map.contains_key(*name))
            .map(|name| {
                let err = TypeInferenceError::UnknownStructField {
                    struct_name: None,
                    field_name: name.node.clone(),
                };
                S::new(err, name.span)
            })
            .collect::<Vec<_>>();
        let missing_struct_field_errors = struct_fields_map
            .keys()
            .filter(|name| !shape_fields_map.contains_key(*name))
            .map(|name| {
                let err = TypeInferenceError::MissingStructField {
                    struct_name: None,
                    field_name: name.node.clone(),
                };
                S::new(err, name.span)
            })
            .collect::<Vec<_>>();

        unknown_struct_field_errors
            .into_iter()
            .chain(missing_struct_field_errors)
            .collect()
    }

    fn generate_solution(
        &mut self,
        maps: &InferTypeMaps,
        arena: &mut HIRTypeArena,
    ) -> (Solution, Vec<S<TypeInferenceError>>) {
        let mut solution = Solution {
            expr_ty: HashMap::new(),
            local_ty: HashMap::new(),
        };
        let mut solution_errors = Vec::new();
        for (expr_id, infer_type) in maps.expr_ty.iter() {
            match self.get_type_id(infer_type, arena) {
                Ok(type_id) => {
                    solution.expr_ty.insert(*expr_id, type_id);
                }
                Err(error) => solution_errors.push(error),
            };
        }
        for (local_id, infer_type) in maps.local_ty.iter() {
            match self.get_type_id(infer_type, arena) {
                Ok(type_id) => {
                    solution.local_ty.insert(*local_id, type_id);
                }
                Err(error) => solution_errors.push(error),
            };
        }
        print_solution(&solution, arena);
        (solution, solution_errors)
    }

    fn get_type_id(
        &mut self,
        infer_type: &S<InferType>,
        arena: &mut HIRTypeArena,
    ) -> Result<MS<HIRTypeId>, S<TypeInferenceError>> {
        let span = infer_type.span;
        match &infer_type.node {
            InferType::Var(type_var_id) => {
                let root_type_var_id = self.unifier.find(*type_var_id);
                if let Some(infer_type) = self.unifier.binding_of(root_type_var_id) {
                    let binding_node = infer_type.node.clone();
                    match binding_node {
                        InferType::Var(_) => unreachable!(
                            "Invariant: binding of type variable is never InferType::Var"
                        ),
                        InferType::Scalar(type_id) => {
                            return Ok(MS::new(type_id).with_span(infer_type.span));
                        }
                        InferType::Struct(fields) => {
                            return self.get_type_id_from_struct_fields(
                                &fields, arena, span,
                            );
                        }
                    };
                }
                // Fallbacks
                if let Some(class) = self.get_class(*type_var_id) {
                    let fallback_type = class.fallback_type();
                    let type_id = arena.get_or_insert(fallback_type);
                    return Ok(MS::new(type_id).with_span(span));
                }
                if let Some(fields) = self.get_struct_shape(*type_var_id) {
                    return self
                        .get_type_id_from_struct_fields(&fields, arena, span);
                }
                Err(S::new(TypeInferenceError::CantResolveType, span))
            }
            InferType::Scalar(type_id) => Ok(MS::new(*type_id).with_span(span)),
            InferType::Struct(fields) => {
                self.get_type_id_from_struct_fields(fields, arena, span)
            }
        }
    }

    fn get_type_id_from_struct_fields(
        &mut self,
        fields: &[(S<String>, S<InferType>)],
        arena: &mut HIRTypeArena,
        span: Span,
    ) -> Result<MS<HIRTypeId>, S<TypeInferenceError>> {
        let mut struct_fields = Vec::new();
        for (name, field_infer_type) in fields {
            let field_type_id = self
                .get_type_id(field_infer_type, arena)?
                .into_spanned_or(name.span);
            let struct_field = StructField {
                name: name.clone(),
                ty_id: field_type_id,
            };
            struct_fields.push(S::new(struct_field, name.span));
        }
        let hir_type = HIRType::Struct(StructType {
            fields: struct_fields,
        });
        let type_id = arena.get_or_insert(hir_type);
        Ok(MS::new(type_id).with_span(span))
    }

    fn validate_in_class_obligation(
        &mut self,
        infer_type: S<InferType>,
        class: TypeClass,
        arena: &HIRTypeArena,
    ) -> Option<S<TypeInferenceError>> {
        match infer_type.node {
            InferType::Var(type_var_id) => {
                // Add class to the set of classes for this type variable
                self.add_class(type_var_id, class);

                // Get binding of type variable id from unifier
                let Some(binding_infer_type) = self.unifier.binding_of(type_var_id)
                else {
                    // If not bound, we will generate fallback type later
                    return None;
                };

                if binding_infer_type == &infer_type {
                    return None;
                }

                match binding_infer_type.node {
                    InferType::Var(binding_type_var_id) => {
                        self.add_class(binding_type_var_id, class);
                        None
                    }
                    InferType::Scalar(type_id) => {
                        // Validate binding against class
                        Self::validate_type_class(
                            type_id,
                            class,
                            arena,
                            binding_infer_type.span,
                        )
                    }
                    InferType::Struct(_) => todo!(),
                }
            }
            InferType::Scalar(type_id) => {
                // Validate Obligation::InClass itself
                Self::validate_type_class(type_id, class, arena, infer_type.span)
            }
            InferType::Struct(_) => todo!(),
        }
    }

    fn add_class(&mut self, type_var_id: TypeVarId, class: TypeClass) {
        let root_type_var_id = self.unifier.find(type_var_id);
        self.classes.insert(root_type_var_id, class);
    }

    fn get_class(&mut self, type_var_id: TypeVarId) -> Option<TypeClass> {
        let root_type_var_id = self.unifier.find(type_var_id);
        self.classes.get(&root_type_var_id).cloned()
    }

    fn add_struct_shape(
        &mut self,
        type_var_id: TypeVarId,
        fields: Vec<(S<String>, S<InferType>)>,
    ) {
        let root_type_var_id = self.unifier.find(type_var_id);
        self.struct_shapes.insert(root_type_var_id, fields);
    }

    fn get_struct_shape(
        &mut self,
        type_var_id: TypeVarId,
    ) -> Option<Vec<(S<String>, S<InferType>)>> {
        let root_type_var_id = self.unifier.find(type_var_id);
        self.struct_shapes.get(&root_type_var_id).cloned()
    }

    fn validate_type_class(
        type_id: HIRTypeId,
        class: TypeClass,
        arena: &HIRTypeArena,
        span: Span,
    ) -> Option<S<TypeInferenceError>> {
        let ty = arena.get_by_id(type_id).unwrap();
        if !class.is_compatible_with(ty) {
            let ty_str = ty.format(arena);
            let error =
                TypeInferenceError::IncompatibleTypeClass { ty: ty_str, class };
            return Some(S::new(error, span));
        }
        None
    }
}

#[derive(Debug)]
pub struct Solution {
    pub expr_ty: HashMap<ExprId, MS<HIRTypeId>>,
    pub local_ty: HashMap<LocalId, MS<HIRTypeId>>,
}

// ------------------- Helpers to test manually ------------------- //

// TODO: Remove these functions after adding support for structs.

fn print_constraints(constraints: &Constraints, type_arena: &HIRTypeArena) {
    println!("Equalities:");
    for eq in &constraints.equalities {
        let infer_type_str_a = format_infer_type(&eq.0.node, type_arena);
        let infer_type_str_b = format_infer_type(&eq.1.node, type_arena);
        println!("\t{infer_type_str_a} == {infer_type_str_b}");
    }

    println!("Obligations:");
    for obligation in &constraints.obligations {
        print!("\t");
        match obligation {
            Obligation::InClass(infer_ty, ty_class) => {
                let infer_type_str = format_infer_type(&infer_ty.node, type_arena);
                print!("{infer_type_str} ∈ {ty_class:?}");
            }
            Obligation::StructShape(infer_ty, fields) => {
                let infer_type_str = format_infer_type(&infer_ty.node, type_arena);
                print!("{infer_type_str} === {{ ");
                for (name, field_infer_ty) in fields {
                    let field_infer_type_str =
                        format_infer_type(&field_infer_ty.node, type_arena);
                    print!("{}: {}, ", name.node, field_infer_type_str);
                }
                print!("}}");
            }
        }
        println!();
    }

    println!("Expr type map:");
    if constraints.maps.expr_ty.is_empty() {
        println!("\tNo.");
    }
    for (expr_id, infer_ty) in &constraints.maps.expr_ty {
        let infer_type_str = format_infer_type(&infer_ty.node, type_arena);
        println!("\t{expr_id} == {infer_type_str}");
    }

    println!("Local type map:");
    if constraints.maps.local_ty.is_empty() {
        println!("\tNo.");
    }
    for (local_id, infer_ty) in &constraints.maps.local_ty {
        let infer_type_str = format_infer_type(&infer_ty.node, type_arena);
        println!("\t{local_id} == {infer_type_str}");
    }
}

fn format_infer_type(infer_type: &InferType, type_arena: &HIRTypeArena) -> String {
    match infer_type {
        InferType::Var(id) => format!("{id}"),
        InferType::Scalar(type_id) => {
            let ty = type_arena.get_by_id(*type_id).unwrap();
            format!("{ty:?}")
        }
        InferType::Struct(fields) => {
            let mut s = String::from("Struct { ");
            for (name, field_infer_type) in fields {
                s.push_str(&format!(
                    "{}: {}, ",
                    name.node,
                    format_infer_type(&field_infer_type.node, type_arena)
                ));
            }
            s.push('}');
            s
        }
    }
}

fn print_unifier_state(unifier: &Unifier, arena: &HIRTypeArena) {
    println!("Parent map:");
    for (type_var_id, parent_id) in &unifier.parent {
        println!("\t{type_var_id:?} -> {parent_id:?}");
    }

    println!("Binding map:");
    for (type_var_id, binding) in &unifier.binding {
        let binding_str = match &binding.node {
            InferType::Var(type_var_id) => format!("{type_var_id:?}"),
            InferType::Scalar(type_id) => {
                arena.get_by_id(*type_id).unwrap().format(arena)
            }
            InferType::Struct(fields) => {
                let mut s = String::from("Struct { ");
                for (name, field_infer_type) in fields {
                    let field_infer_type_str =
                        format_infer_type(&field_infer_type.node, arena);
                    s.push_str(&format!(
                        "{}: {}, ",
                        name.node, field_infer_type_str
                    ));
                }
                s.push('}');
                s
            }
        };
        println!("\t{type_var_id:?} -> {binding_str}");
    }
}

fn print_solution(solution: &Solution, arena: &HIRTypeArena) {
    println!("Solution:");
    for (expr_id, type_id) in &solution.expr_ty {
        let ty = arena.get_by_id(type_id.node).unwrap().format(arena);
        println!("\t{expr_id:?}: {ty}");
    }
    for (local_id, type_id) in &solution.local_ty {
        let ty = arena.get_by_id(type_id.node).unwrap().format(arena);
        println!("\t{local_id:?}: {ty}");
    }
    println!();
}
