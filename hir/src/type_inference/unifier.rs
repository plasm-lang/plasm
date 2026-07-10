use std::collections::HashMap;

use diagnostic::{MaybeSpanned, Spanned};
use utils::ids::TypeVarId;

use super::constraint_gen::Equality;
use super::type_var::InferType;

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
#[derive(Debug)]
pub struct Unifier {
    pub parent: HashMap<TypeVarId, TypeVarId>,
    /// Invariant: value of `binding` is never `InferType::Var`.
    pub binding: HashMap<TypeVarId, S<InferType>>,
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
        equalities
            .into_iter()
            .flat_map(|eq| self.unify(eq.0, eq.1))
            .collect()
    }

    /// Finds the root of the union–find tree for a given type variable,
    /// performing path compression along the way.
    pub fn find(&mut self, id: TypeVarId) -> TypeVarId {
        let parent = *self.parent.get(&id).unwrap_or(&id);
        if parent != id {
            let root = self.find(parent);
            self.parent.insert(id, root);
            root
        } else {
            id
        }
    }

    /// Returns the binding of a type variable, if it has one.
    pub fn binding_of(&mut self, id: TypeVarId) -> Option<&S<InferType>> {
        let root = self.find(id);
        self.binding.get(&root)
    }

    fn bind(&mut self, id: TypeVarId, term: S<InferType>) -> Vec<S<UnifyError>> {
        let root = self.find(id);
        match self.binding.get(&root) {
            Some(existing) => self.unify(existing.clone(), term),
            None => {
                self.binding.insert(root, term);
                Vec::new()
            }
        }
    }

    /// Recursively unifies two type variables, returning a list of errors if
    /// any.
    fn unify(&mut self, a: S<InferType>, b: S<InferType>) -> Vec<S<UnifyError>> {
        match (&a.node, &b.node) {
            (InferType::Var(type_var_id_a), InferType::Var(type_var_id_b)) => {
                return self.unify_var_var(*type_var_id_a, *type_var_id_b);
            }
            (InferType::Var(type_var_id), _) => {
                return self.bind(*type_var_id, b);
            }
            (_, InferType::Var(type_var_id)) => {
                return self.bind(*type_var_id, a);
            }
            (InferType::Scalar(scalar_a), InferType::Scalar(scalar_b)) => {
                if scalar_a != scalar_b {
                    let span = a.span.max(b.span);
                    return vec![S::new(UnifyError::Conflict(a, b), span)];
                }
            }
            _ => todo!(),
        }

        Vec::new()
    }

    fn unify_var_var(
        &mut self,
        type_var_id_a: TypeVarId,
        type_var_id_b: TypeVarId,
    ) -> Vec<S<UnifyError>> {
        let root_a = self.find(type_var_id_a);
        let root_b = self.find(type_var_id_b);

        if root_a == root_b {
            return Vec::new();
        }

        let binding_a = self.binding.remove(&root_a);
        let binding_b = self.binding.remove(&root_b);

        let root = self.union(root_a, root_b);

        match (binding_a, binding_b) {
            (Some(term_a), Some(term_b)) => {
                self.binding.insert(root, term_a.clone());

                // Check that term_a and term_b are compatible.
                self.unify(term_a, term_b)
            }
            (Some(term), None) | (None, Some(term)) => {
                self.binding.insert(root, term);
                Vec::new()
            }
            (None, None) => Vec::new(),
        }
    }

    /// Merges 2 sets to indicate that two type variables are equivalent.
    ///
    /// Payload-agnostic: touches only the union–find forest, never `binding`.
    /// Returns the surviving root of the merged set.
    fn union(&mut self, a: TypeVarId, b: TypeVarId) -> TypeVarId {
        let root_a = self.find(a);
        let root_b = self.find(b);

        if root_a != root_b {
            self.parent.insert(root_a, root_b);
            return root_b;
        }

        root_a
    }

    /// Checks if a type variable occurs in a term, which would create a
    /// infinite cycle.
    fn recursion_check(&mut self, id: TypeVarId, term: &InferType) -> bool {
        todo!()
    }
}
