use std::collections::{HashMap, HashSet};

use diagnostic::{MaybeSpanned, Spanned};
use utils::ids::{ExprId, HIRTypeId, LocalId, TypeVarId};

use crate::error::Error;
use crate::hir::{InternalFunction, OptTyped, Typed};
use crate::types::HIRTypeArena;

use super::constraint_gen::{Constraints, Equality, FunctionConstraintGen};
use super::type_class::TypeClass;
use super::type_var::InferType;
use super::unifier::{Unifier, UnifyError};

// For brevity
type S<T> = Spanned<T>;
type MS<T> = MaybeSpanned<T>;

pub struct FunctionTypeSolver {
    unifier: Unifier,
    classes: HashMap<TypeVarId, HashSet<TypeClass>>,
    in_func: InternalFunction<OptTyped>,
    constraints: Constraints,
}

impl FunctionTypeSolver {
    pub fn new(func: InternalFunction<OptTyped>, constraints: Constraints) -> Self {
        Self {
            unifier: Unifier::new(),
            classes: HashMap::new(),
            in_func: func,
            constraints,
        }
    }

    fn unify_error_into_type_error(
        unify_error: S<UnifyError>,
        arena: &HIRTypeArena,
    ) -> S<Error> {
        todo!()
    }

    pub fn solve(
        mut self,
        arena: &mut HIRTypeArena,
    ) -> (InternalFunction<Typed>, Vec<S<Error>>) {
        let (equalities, obligations) =
            (self.constraints.equalities, self.constraints.obligations);
        let mut errors = self
            .unifier
            .unify_equalities(equalities)
            .into_iter()
            .map(|unify_error| Self::unify_error_into_type_error(unify_error, arena))
            .collect::<Vec<_>>();

        // for obligation in obligations {
        //     match obligation {
        //         super::constraint_gen::Obligation::InClass(s_infer_ty, ty_class) => {
        //             let infer_ty = s_infer_ty.node;
        //             let ty_var_id = match infer_ty {
        //                 InferType::Var(id) => id, // TODO: Perform class fallback if type is not solved.
        //                 _ => continue, // TODO: Validate that the type is allowed by class.
        //             };
        //             let root_ty_var_id = self.unifier.find(ty_var_id);
        //             self.classes.entry(root_ty_var_id).or_default().insert(ty_class);
        //         }
        //     }
        // }

        todo!()
    }
}

// pub struct Solution {
//     pub expr_ty: HashMap<ExprId, MS<HIRTypeId>>,
//     pub local_ty: HashMap<LocalId, MS<HIRTypeId>>,
// }
