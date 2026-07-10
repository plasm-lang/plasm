use std::collections::HashMap;

use diagnostic::{MaybeSpanned, Span, Spanned};
use utils::ids::{ExprId, HIRTypeId, LocalId, TypeVarId};

use super::constraint_gen::{Constraints, Equality, InferTypeMaps};
use super::error::TypeInferenceError;
use super::type_class::TypeClass;
use super::type_var::InferType;
use super::unifier::{Unifier, UnifyError};
use crate::type_inference::constraint_gen::Obligation;
use crate::types::HIRTypeArena;

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
}

impl FunctionTypeSolver {
    pub fn new() -> Self {
        Self {
            unifier: Unifier::new(),
            classes: HashMap::new(),
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
                let hir_type_b =
                    type_id_b.map(|id| arena.get_by_id(id).unwrap().clone());
                let error = TypeInferenceError::TypesConflict {
                    first: hir_type_a,
                    second: hir_type_b,
                };
                S::new(error, unify_error.span)
            }
            UnifyError::Recursion(a, b) => todo!(),
            UnifyError::MissingStructField {
                struct_ty,
                field_name,
            } => todo!(),
            UnifyError::UnknownStructField {
                struct_ty,
                field_name,
            } => todo!(),
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
            }
        }
        obligation_errors
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
        match infer_type.node {
            InferType::Var(type_var_id) => {
                let root_type_var_id = self.unifier.find(type_var_id);
                if let Some(infer_type) = self.unifier.binding_of(root_type_var_id) {
                    match &infer_type.node {
                        InferType::Var(_) => unreachable!(
                            "Invariant: binding of type variable is never InferType::Var"
                        ),
                        InferType::Scalar(type_id) => {
                            return Ok(MS::new(*type_id).with_span(infer_type.span));
                        }
                        InferType::Struct(_) => todo!(),
                    };
                }
                if let Some(class) = self.get_class(type_var_id) {
                    let fallback_type = class.fallback_type();
                    let type_id = arena.get_or_insert(fallback_type);
                    return Ok(MS::new(type_id).with_span(span));
                }
                Err(S::new(TypeInferenceError::CantResolveType, span))
            }
            InferType::Scalar(type_id) => Ok(MS::new(type_id).with_span(span)),
            InferType::Struct(_) => todo!(),
        }
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

    fn validate_type_class(
        type_id: HIRTypeId,
        class: TypeClass,
        arena: &HIRTypeArena,
        span: Span,
    ) -> Option<S<TypeInferenceError>> {
        let ty = arena.get_by_id(type_id).unwrap();
        if !class.is_compatible_with(ty) {
            let error = TypeInferenceError::IncompatibleTypeClass {
                ty: ty.clone(),
                class,
            };
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

fn print_constraints(constraints: &Constraints, type_arena: &HIRTypeArena) {
    println!("Equalities:");
    for eq in &constraints.equalities {
        print!("\t");
        print_infer_type(&eq.0.node, type_arena);
        print!(" == ");
        print_infer_type(&eq.1.node, type_arena);
        println!();
    }

    println!("Obligations:");
    for obligation in &constraints.obligations {
        print!("\t");
        match obligation {
            Obligation::InClass(s_infer_ty, ty_class) => {
                print_infer_type(&s_infer_ty.node, type_arena);
                print!(" ∈ {ty_class:?}");
            }
        }
        println!();
    }

    println!("Expr type map:");
    for (expr_id, infer_ty) in &constraints.maps.expr_ty {
        print!("\t{expr_id} == ");
        print_infer_type(&infer_ty.node, type_arena);
        println!();
    }

    println!("Local type map:");
    for (local_id, infer_ty) in &constraints.maps.local_ty {
        print!("\t{local_id} == ");
        print_infer_type(&infer_ty.node, type_arena);
        println!();
    }
}

fn print_infer_type(infer_type: &InferType, type_arena: &HIRTypeArena) {
    match infer_type {
        InferType::Var(id) => print!("{id}"),
        InferType::Scalar(type_id) => {
            let ty = type_arena.get_by_id(*type_id).unwrap();
            print!("{ty:?}");
        }
        InferType::Struct(fields) => {
            print!("Struct {{ ");
            for (name, field_infer_type) in fields {
                print!("{}: ", name.node);
                print_infer_type(&field_infer_type.node, type_arena);
                print!(", ");
            }
            print!("}}");
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
                let hir_type = arena.get_by_id(*type_id).unwrap();
                format!("{hir_type}")
            }
            InferType::Struct(_) => todo!(),
        };
        println!("\t{type_var_id:?} -> {binding_str}");
    }
}

fn print_solution(solution: &Solution, arena: &HIRTypeArena) {
    println!("Solution:");
    for (expr_id, type_id) in &solution.expr_ty {
        let ty = arena.get_by_id(type_id.node).unwrap();
        println!("\t{expr_id:?}: {ty}");
    }
    for (local_id, type_id) in &solution.local_ty {
        let ty = arena.get_by_id(type_id.node).unwrap();
        println!("\t{local_id:?}: {ty}");
    }
}
