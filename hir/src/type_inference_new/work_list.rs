use std::collections::{HashMap, HashSet, VecDeque};

use utils::ids::TypeVarId;

use super::constraint_gen::Constraint;
use super::type_var::InferType;
use super::union_find::UnionFind;

pub struct WorkList {
    queue: VecDeque<Constraint>,
    freezed: HashMap<TypeVarId, Vec<Constraint>>,
}

impl WorkList {
    pub fn new(constraints: Vec<Constraint>) -> Self {
        Self {
            queue: VecDeque::from(constraints),
            freezed: HashMap::new(),
        }
    }

    pub fn pop(&mut self) -> Option<Constraint> {
        self.queue.pop_front()
    }

    pub fn push(&mut self, constraint: Constraint) {
        self.queue.push_back(constraint);
    }

    pub fn freeze(&mut self, constraint: Constraint, root: TypeVarId) {
        self.freezed.entry(root).or_default().push(constraint);
    }

    pub fn unfreeze(&mut self, type_var_id: TypeVarId) {
        if let Some(constraints) = self.freezed.remove(&type_var_id) {
            self.queue.extend(constraints);
        }
    }

    pub fn merge(&mut self, absorbed: TypeVarId, into: TypeVarId) {
        if absorbed == into {
            return;
        }

        if let Some(mut constraints) = self.freezed.remove(&absorbed) {
            self.freezed
                .entry(into)
                .or_default()
                .append(&mut constraints);
        }
    }

    /// Pops a frozen constraint that is a leaf.
    /// `self.frozen` is processed as a forest of dependency trees with
    /// Depth-First Search.
    pub fn pop_freezed_leaf(
        &mut self,
        union_find: &mut UnionFind,
    ) -> Option<Constraint> {
        // The frozen forest is scheduled in two tiers.
        //
        // 1. Generators (`StructShape`, `InClass`) first: only they can bind a type
        //    (fallback).
        // 2. Inspectors (`HasField`) only once no generator can make progress.
        for generators_only in [true, false] {
            let roots: Vec<TypeVarId> = self.freezed.keys().copied().collect();
            for root in roots {
                let mut visited = HashSet::new();
                if let Some(constraint) = self.pop_leaf_from(
                    root,
                    &mut visited,
                    union_find,
                    generators_only,
                ) {
                    return Some(constraint);
                }
            }
        }
        None
    }

    fn pop_leaf_from(
        &mut self,
        type_var_id: TypeVarId,
        visited: &mut HashSet<TypeVarId>,
        union_find: &mut UnionFind,
        generators_only: bool,
    ) -> Option<Constraint> {
        // Already visited on this path = a cycle, nothing to extract here.
        if !visited.insert(type_var_id) {
            return None;
        }

        let constraints = self.freezed.get(&type_var_id)?;

        // Check if any constraint of the current node depends on another frozen root
        let external_dep = constraints.iter().find_map(|c| {
            constraint_deps(c).into_iter().find(|d| {
                let d_root = union_find.find(*d);
                d_root != type_var_id && self.freezed.contains_key(&d_root)
            })
        });

        // If there is a dependency on another frozen root, we descend into it
        if let Some(dep_var) = external_dep {
            let dep_root = union_find.find(dep_var);
            return self.pop_leaf_from(
                dep_root,
                visited,
                union_find,
                generators_only,
            );
        }

        // If there are no external frozen dependencies, the node type_var_id is a
        // leaf. We give priority to type generators (StructShape, InClass)
        // over inspectors (HasField).
        let generator_pos = constraints.iter().position(|c| {
            matches!(c, Constraint::StructShape(..) | Constraint::InClass(..))
        });
        let pos = match generator_pos {
            Some(pos) => pos,
            // In the generators-only tier a pure-inspector leaf is skipped so
            // the search moves on to a root that can still make progress.
            None if generators_only => return None,
            None => 0,
        };

        let constraints = self.freezed.get_mut(&type_var_id).unwrap();
        let constraint = constraints.swap_remove(pos);
        if constraints.is_empty() {
            self.freezed.remove(&type_var_id);
        }
        Some(constraint)
    }
}

fn constraint_deps(constraint: &Constraint) -> Vec<TypeVarId> {
    fn var(infer: &InferType) -> Option<TypeVarId> {
        match infer {
            InferType::Var(id) => Some(*id),
            InferType::Known(_) => None,
        }
    }

    match constraint {
        Constraint::Equality(a, b) => {
            var(&a.node).into_iter().chain(var(&b.node)).collect()
        }
        Constraint::InClass(infer_type, _) => {
            var(&infer_type.node).into_iter().collect()
        }
        Constraint::StructShape(base, fields) => var(&base.node)
            .into_iter()
            .chain(fields.iter().filter_map(|(_, ty)| var(&ty.node)))
            .collect(),
        Constraint::HasField { base, .. } => var(&base.node).into_iter().collect(),
    }
}
