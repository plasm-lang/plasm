use std::collections::{BTreeMap, VecDeque};

use super::constraint_gen::{Access, Constraint};
use super::type_var::TypeVarId;

pub struct WorkList {
    queue: VecDeque<Constraint>,
    /// Accesses that wait for a more specific binding of their base type.
    /// Keyed by the root of the base type variable.
    freezed_map: BTreeMap<TypeVarId, Vec<Access>>,
}

impl WorkList {
    pub fn new(constraints: Vec<Constraint>) -> Self {
        Self {
            queue: VecDeque::from(constraints),
            freezed_map: BTreeMap::new(),
        }
    }

    pub fn pop(&mut self) -> Option<Constraint> {
        self.queue.pop_front()
    }

    pub fn push(&mut self, constraint: Constraint) {
        self.queue.push_back(constraint);
    }

    pub fn freeze(&mut self, access: Access, root: TypeVarId) {
        self.freezed_map.entry(root).or_default().push(access);
    }

    /// Returns accesses frozen on the root back to the queue.
    pub fn unfreeze(&mut self, root: TypeVarId) {
        if let Some(accesses) = self.freezed_map.remove(&root) {
            self.queue
                .extend(accesses.into_iter().map(Constraint::Access));
        }
    }

    /// Moves accesses frozen on the absorbed root to the `into` root.
    pub fn merge(&mut self, absorbed: TypeVarId, into: TypeVarId) {
        if let Some(accesses) = self.freezed_map.remove(&absorbed) {
            self.freezed_map.entry(into).or_default().extend(accesses);
        }
    }

    /// Frozen accesses together with the roots they are frozen on.
    pub fn freezed(&self) -> impl Iterator<Item = (TypeVarId, &Access)> {
        self.freezed_map.iter().flat_map(|(root, accesses)| {
            accesses.iter().map(move |access| (*root, access))
        })
    }

    pub fn take_freezed(&mut self) -> impl Iterator<Item = Access> {
        std::mem::take(&mut self.freezed_map)
            .into_values()
            .flatten()
    }
}
