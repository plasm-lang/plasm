use std::collections::HashMap;

use utils::ids::{HIRTypeId, TypeVarId};

use super::type_var::InferType;

/// Union–find (disjoint-set) data structure to group type variables that must
/// have the same type. Each group has at most one concrete type binding.
///
/// See also: <https://en.wikipedia.org/wiki/Disjoint-set_data_structure>
pub struct UnionFind {
    parents: HashMap<TypeVarId, TypeVarId>,
    bindings: HashMap<TypeVarId, HIRTypeId>,
}

pub struct TypesConflictError(pub HIRTypeId, pub HIRTypeId);

pub enum UnificationOutcome {
    Merged {
        absorbed: TypeVarId,
        into: TypeVarId,
    },
    Bound {
        root: TypeVarId,
    },
    NoOp,
}

impl UnionFind {
    pub fn new() -> Self {
        Self {
            parents: HashMap::new(),
            bindings: HashMap::new(),
        }
    }

    pub fn binding_of(&mut self, type_var_id: TypeVarId) -> Option<HIRTypeId> {
        let root = self.find(type_var_id);
        self.bindings.get(&root).copied()
    }

    pub fn find(&mut self, type_var_id: TypeVarId) -> TypeVarId {
        let root = *self.parents.get(&type_var_id).unwrap_or(&type_var_id);
        if root != type_var_id {
            let root_of_root = self.find(root);
            self.parents.insert(type_var_id, root_of_root);
            root_of_root
        } else {
            type_var_id
        }
    }

    pub fn unify(
        &mut self,
        a: InferType,
        b: InferType,
    ) -> Result<UnificationOutcome, TypesConflictError> {
        match (a, b) {
            (InferType::Var(a_id), InferType::Var(b_id)) => self.union(a_id, b_id),
            (InferType::Var(id), InferType::Known(type_id))
            | (InferType::Known(type_id), InferType::Var(id)) => {
                self.bind(id, type_id)
            }
            (InferType::Known(a_id), InferType::Known(b_id)) => {
                if a_id != b_id {
                    return Err(TypesConflictError(a_id, b_id));
                }
                Ok(UnificationOutcome::NoOp)
            }
        }
    }

    fn bind(
        &mut self,
        type_var_id: TypeVarId,
        type_id: HIRTypeId,
    ) -> Result<UnificationOutcome, TypesConflictError> {
        let root = self.find(type_var_id);
        let existing = self.bindings.get(&root);
        match existing {
            Some(existing) if *existing != type_id => {
                Err(TypesConflictError(*existing, type_id))
            }
            Some(_) => Ok(UnificationOutcome::NoOp),
            None => {
                self.bindings.insert(root, type_id);
                Ok(UnificationOutcome::Bound { root })
            }
        }
    }

    fn union(
        &mut self,
        a: TypeVarId,
        b: TypeVarId,
    ) -> Result<UnificationOutcome, TypesConflictError> {
        let root_a = self.find(a);
        let root_b = self.find(b);

        if root_a == root_b {
            return Ok(UnificationOutcome::NoOp);
        }

        self.parents.insert(root_a, root_b);

        let binding_a = self.bindings.remove(&root_a);
        let binding_b = self.bindings.remove(&root_b);

        match (binding_a, binding_b) {
            (Some(type_id_a), Some(type_id_b)) => {
                if type_id_a != type_id_b {
                    return Err(TypesConflictError(type_id_a, type_id_b));
                }
                self.bindings.insert(root_b, type_id_a);
            }
            (Some(type_id), None) | (None, Some(type_id)) => {
                self.bindings.insert(root_b, type_id);
            }
            (None, None) => {}
        }

        Ok(UnificationOutcome::Merged {
            absorbed: root_a,
            into: root_b,
        })
    }
}
