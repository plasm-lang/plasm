use std::collections::{BTreeMap, HashMap};

use super::type_var::{Binding, TypeVarId};

/// Union–find (disjoint-set) data structure to group type variables that must
/// have the same type. Each group has at most one binding.
///
/// See also: <https://en.wikipedia.org/wiki/Disjoint-set_data_structure>
pub struct UnionFind {
    parents: HashMap<TypeVarId, TypeVarId>,
    /// Bindings of the roots. Ordered to keep fallback deterministic.
    bindings: BTreeMap<TypeVarId, Binding>,
}

pub struct Merged {
    pub absorbed: TypeVarId,
    pub into: TypeVarId,
    /// Binding of the absorbed root, it must be joined into the `into` root.
    pub absorbed_binding: Option<Binding>,
}

impl UnionFind {
    pub fn new() -> Self {
        Self {
            parents: HashMap::new(),
            bindings: BTreeMap::new(),
        }
    }

    pub fn binding_of(&mut self, type_var_id: TypeVarId) -> Option<&Binding> {
        let root = self.find(type_var_id);
        self.bindings.get(&root)
    }

    /// Bound roots in ascending order.
    pub fn bindings(&self) -> impl Iterator<Item = (TypeVarId, &Binding)> {
        self.bindings.iter().map(|(root, binding)| (*root, binding))
    }

    pub fn bind(&mut self, root: TypeVarId, binding: Binding) {
        self.bindings.insert(root, binding);
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

    /// The older type variable stays the root, so the binding that appeared
    /// earlier in the code is treated as the expected one.
    /// Returns `None` if the type variables are already in the same group.
    pub fn union(&mut self, a: TypeVarId, b: TypeVarId) -> Option<Merged> {
        let root_a = self.find(a);
        let root_b = self.find(b);

        if root_a == root_b {
            return None;
        }

        let (absorbed, into) = if root_a < root_b {
            (root_b, root_a)
        } else {
            (root_a, root_b)
        };
        self.parents.insert(absorbed, into);

        Some(Merged {
            absorbed,
            into,
            absorbed_binding: self.bindings.remove(&absorbed),
        })
    }
}

// --- Debug Helpers --- //

#[allow(dead_code)]
#[cfg(debug_assertions)]
impl UnionFind {
    /// Type variables that were merged or bound.
    pub fn type_vars(&self) -> impl Iterator<Item = TypeVarId> {
        self.parents.keys().chain(self.bindings.keys()).copied()
    }
}
