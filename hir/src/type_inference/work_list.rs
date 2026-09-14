use std::collections::{HashMap, HashSet, VecDeque};

use utils::ids::create_id_type;

use super::constraint_gen::Constraint;
use super::type_var::{InferType, TypeVarId};
use super::union_find::UnionFind;

create_id_type!(
    /// Node id, used by hir (WorkList data structure)
    ConstraintId, ConstraintMarker, "CondId"
);

pub struct WorkList {
    queue: VecDeque<Constraint>,

    // --- Fields related to freezing functionality --- //
    next_cond_id: ConstraintId,
    freezed_map: HashMap<ConstraintId, Constraint>,

    /// Dependency graph between frozen constraints.
    /// Left = dependent, Right = dependency.
    freezed_graph: M2MRelation<ConstraintId, ConstraintId>,
    type_var_to_constraints: M2MRelation<TypeVarId, ConstraintId>,
    /// Leaf nodes, ready to provide.
    /// VecDeque gives FIFO, deterministic order.
    leafs: VecDeque<ConstraintId>,
}

impl WorkList {
    pub fn new(constraints: Vec<Constraint>) -> Self {
        Self {
            queue: VecDeque::from(constraints),
            next_cond_id: ConstraintId::one(),
            freezed_map: HashMap::new(),
            freezed_graph: M2MRelation::new(),
            type_var_to_constraints: M2MRelation::new(),
            leafs: VecDeque::new(),
        }
    }

    pub fn pop(&mut self) -> Option<Constraint> {
        self.queue.pop_front()
    }

    pub fn push(&mut self, constraint: Constraint) {
        self.queue.push_back(constraint);
    }

    /// Freezes a constraint, resolving its variables to union-find roots.
    pub fn freeze(&mut self, constraint: Constraint, union_find: &mut UnionFind) {
        let id = self.next_cond_id();
        let type_var_deps: Vec<TypeVarId> = constraint_deps(&constraint)
            .into_iter()
            .map(|var| union_find.find(var))
            .collect();
        self.freezed_map.insert(id, constraint);

        let mut has_dependencies = false;
        for type_var_dep in type_var_deps {
            self.type_var_to_constraints.link(type_var_dep, id);

            let Some(constraints) =
                self.type_var_to_constraints.get_right(type_var_dep)
            else {
                continue;
            };

            for &dependency_id in constraints {
                if dependency_id == id {
                    continue;
                }
                self.freezed_graph.link(id, dependency_id);
                has_dependencies = true;
            }
        }

        if !has_dependencies {
            self.leafs.push_back(id);
        }
    }

    pub fn pop_freezed_leaf(&mut self) -> Option<Constraint> {
        while let Some(constraint_id) = self.leafs.pop_front() {
            if let Some(constraint) = self.remove_node(constraint_id) {
                return Some(constraint);
            }
        }
        None
    }

    pub fn merge(&mut self, absorbed: TypeVarId, into: TypeVarId) {
        self.type_var_to_constraints.update_left_key(absorbed, into);
    }

    /// Wakes frozen constraints whose base resolves to `root`.
    /// Only base-bound constraints are woken: they validate and never
    /// re-freeze.
    pub fn unfreeze(&mut self, root: TypeVarId, union_find: &mut UnionFind) {
        let Some(ids) = self.type_var_to_constraints.get_right(root) else {
            return;
        };
        let mut ids: Vec<ConstraintId> = ids.iter().copied().collect();
        ids.sort();

        for id in ids {
            let Some(constraint) = self.freezed_map.get(&id) else {
                continue;
            };
            let Some(base) = constraint_base(constraint) else {
                continue;
            };
            if union_find.find(base) != root {
                continue;
            }
            if let Some(constraint) = self.remove_node(id) {
                self.queue.push_back(constraint);
            }
        }
    }

    fn next_cond_id(&mut self) -> ConstraintId {
        let id = self.next_cond_id;
        self.next_cond_id = self.next_cond_id.increment();
        id
    }

    /// Removes a node from the frozen graph, promoting new leafs.
    fn remove_node(&mut self, constraint_id: ConstraintId) -> Option<Constraint> {
        let constraint = self.freezed_map.remove(&constraint_id)?;

        for type_var in constraint_deps(&constraint) {
            self.type_var_to_constraints.unlink(type_var, constraint_id);
        }

        let dependencies: Vec<ConstraintId> = self
            .freezed_graph
            .get_right(constraint_id)
            .map(|set| set.iter().copied().collect())
            .unwrap_or_default();
        for dependency_id in dependencies {
            self.freezed_graph.unlink(constraint_id, dependency_id);
        }

        let dependents: Vec<ConstraintId> = self
            .freezed_graph
            .get_left(constraint_id)
            .map(|set| set.iter().copied().collect())
            .unwrap_or_default();
        for dependent_id in dependents {
            let unconnected = self.freezed_graph.unlink(dependent_id, constraint_id);
            if let Some(leaf_id) = unconnected.0 {
                self.leafs.push_back(leaf_id);
            }
        }

        Some(constraint)
    }
}

/// Helper data structure to represent a many-to-many relation
/// between two id types.
struct M2MRelation<T1, T2>
where
    T1: std::hash::Hash + Eq + Copy,
    T2: std::hash::Hash + Eq + Copy,
{
    t1_to_t2: HashMap<T1, HashSet<T2>>,
    t2_to_t1: HashMap<T2, HashSet<T1>>,
}

impl<T1, T2> M2MRelation<T1, T2>
where
    T1: std::hash::Hash + Eq + Copy,
    T2: std::hash::Hash + Eq + Copy,
{
    pub fn new() -> Self {
        Self {
            t1_to_t2: HashMap::new(),
            t2_to_t1: HashMap::new(),
        }
    }

    pub fn link(&mut self, t1: T1, t2: T2) {
        self.t1_to_t2.entry(t1).or_default().insert(t2);
        self.t2_to_t1.entry(t2).or_default().insert(t1);
    }

    /// Unlink the relation between t1 and t2.
    /// Returns unconnected t1 and/or t2 if they are no longer connected
    /// to any other nodes.
    pub fn unlink(&mut self, t1: T1, t2: T2) -> (Option<T1>, Option<T2>) {
        let mut unconnected_t1 = None;
        let mut unconnected_t2 = None;

        if let Some(set) = self.t1_to_t2.get_mut(&t1) {
            set.remove(&t2);
            if set.is_empty() {
                self.t1_to_t2.remove(&t1);
                unconnected_t1 = Some(t1);
            }
        }
        if let Some(set) = self.t2_to_t1.get_mut(&t2) {
            set.remove(&t1);
            if set.is_empty() {
                self.t2_to_t1.remove(&t2);
                unconnected_t2 = Some(t2);
            }
        }

        (unconnected_t1, unconnected_t2)
    }

    pub fn get_left(&self, t2: T2) -> Option<&HashSet<T1>> {
        self.t2_to_t1.get(&t2)
    }

    pub fn get_right(&self, t1: T1) -> Option<&HashSet<T2>> {
        self.t1_to_t2.get(&t1)
    }

    pub fn update_left_key(&mut self, old_t1: T1, new_t1: T1) {
        if let Some(right_set) = self.t1_to_t2.remove(&old_t1) {
            for right_key in &right_set {
                let Some(left_set) = self.t2_to_t1.get_mut(right_key) else {
                    continue;
                };
                left_set.remove(&old_t1);
                left_set.insert(new_t1);
            }
            self.t1_to_t2.entry(new_t1).or_default().extend(right_set);
        }
    }

    #[allow(dead_code)]
    pub fn update_right_key(&mut self, old_t2: T2, new_t2: T2) {
        if let Some(left_set) = self.t2_to_t1.remove(&old_t2) {
            for left_key in &left_set {
                let Some(right_set) = self.t1_to_t2.get_mut(left_key) else {
                    continue;
                };
                right_set.remove(&old_t2);
                right_set.insert(new_t2);
            }
            self.t2_to_t1.entry(new_t2).or_default().extend(left_set);
        }
    }
}

/// Returns the variable a constraint must resolve before it can be validated.
fn constraint_base(constraint: &Constraint) -> Option<TypeVarId> {
    let infer = match constraint {
        Constraint::Equality(..) => return None,
        Constraint::InClass(infer_type, _) => infer_type,
        Constraint::StructShape(base, _) => base,
        Constraint::TupleShape(base, _) => base,
        Constraint::HasField { base, .. } => base,
        Constraint::HasIndex { base, .. } => base,
    };
    match infer.node {
        InferType::Var(id) => Some(id),
        InferType::Known(_) => None,
    }
}

/// Returns all type variables of a constraint.
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
        Constraint::TupleShape(base, elements) => var(&base.node)
            .into_iter()
            .chain(elements.iter().filter_map(|ty| var(&ty.node)))
            .collect(),
        Constraint::HasField {
            base, field_type, ..
        } => var(&base.node)
            .into_iter()
            .chain(var(&field_type.node))
            .collect(),
        Constraint::HasIndex {
            base, element_type, ..
        } => var(&base.node)
            .into_iter()
            .chain(var(&element_type.node))
            .collect(),
    }
}

// --- Debug Helpers --- //

#[allow(dead_code)]
#[cfg(debug_assertions)]
impl WorkList {
    pub fn print_dot(&self, arena: &crate::types::HIRTypeArena) {
        println!("{}", self.to_dot(arena));
    }

    pub fn to_dot(&self, arena: &crate::types::HIRTypeArena) -> String {
        let nodes_str = self
            .freezed_map
            .iter()
            .map(|(constraint_id, constraint)| {
                let constraint_str = constraint.format(arena).replace("\"", "\\\"");
                format!("    {} [label=\"{}\"];", constraint_id, constraint_str)
            })
            .collect::<Vec<String>>()
            .join("\n");

        let edges_str = self
            .freezed_graph
            .t2_to_t1
            .iter()
            .flat_map(|(dependency, dependent_set)| {
                dependent_set.iter().map(move |dependent| {
                    format!("    {} -> {};", dependent, dependency)
                })
            })
            .collect::<Vec<String>>()
            .join("\n");

        let node_style = "    node [shape=box, style=\"rounded,filled\", fillcolor=\"#f0f0f0\", fontname=\"Helvetica\"];";
        let edge_style = "    edge [color=\"#555555\"];";
        format!(
            "digraph WorkList {{\n{}\n{}\n\n{}\n\n{}\n}}\n",
            node_style, edge_style, nodes_str, edges_str
        )
    }
}
