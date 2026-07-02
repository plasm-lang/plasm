use std::collections::HashMap;

use diagnostic::{MaybeSpanned, Span, Spanned};
use utils::ids::{ExprId, HIRTypeId, LocalId, TypeVarId};

use super::constraint_gen::{Equality, Obligation};
use super::type_var::InferType;
use crate::types::{HIRType, HIRTypeArena, StructField, StructType};

// For brevity
type S<T> = Spanned<T>;
type MS<T> = MaybeSpanned<T>;

pub enum UnifyError {
    Conflict(S<InferType>, S<InferType>),
    Recursion(S<InferType>, S<InferType>),
    MissingStructField {
        struct_ty: S<InferType>,
        field_name: S<String>,
    },
    UnknownStructField {
        struct_ty: S<InferType>,
        field_name: S<String>,
    },
}

/// Union–find (disjoint-set) data structure to group type variables that must
/// have the same type. Each group has at most one concrete type binding.
///
/// See also: <https://en.wikipedia.org/wiki/Disjoint-set_data_structure>
pub struct Unifier {
    parent: HashMap<TypeVarId, TypeVarId>,
    binding: HashMap<TypeVarId, S<InferType>>,
}

impl Unifier {
    pub fn new() -> Self {
        Self {
            parent: HashMap::new(),
            binding: HashMap::new(),
        }
    }

    pub fn unify_equalities(
        &mut self,
        equalities: Vec<Equality>,
    ) -> Vec<S<UnifyError>> {
        todo!()
    }

    /// Finds the root of the union–find tree for a given type variable,
    /// performing path compression along the way.
    pub fn find(&mut self, id: TypeVarId) -> TypeVarId {
        todo!()
    }

    /// Returns the binding of a type variable, if it has one.
    pub fn binding_of(&mut self, id: TypeVarId) -> Option<&S<InferType>> {
        todo!()
    }

    fn bind(&mut self, id: TypeVarId, term: S<InferType>) -> Vec<S<UnifyError>> {
        todo!()
    }

    /// Recursively unifies two type variables, returning a list of errors if any.
    fn unify(&mut self, a: S<InferType>, b: S<InferType>) -> Vec<S<UnifyError>> {
        todo!()
    }

    /// Merges 2 sets to indicate that two type variables are equivalent.
    ///
    /// Does not check for conflicts or errors.
    fn union(&mut self, a: TypeVarId, b: TypeVarId) {
        todo!()
    }

    /// Checks if a type variable occurs in a term, which would create a infinite cycle.
    fn recursion_check(&mut self, id: TypeVarId, term: &InferType) -> bool {
        todo!()
    }
}
