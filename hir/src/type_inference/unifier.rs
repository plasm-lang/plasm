use std::collections::HashMap;

use diagnostic::{MaybeSpanned, Span, Spanned};
use utils::ids::TypeVarId;

use super::constraint_gen::Equality;
use super::type_var::InferType;

// For brevity
type S<T> = Spanned<T>;
type MS<T> = MaybeSpanned<T>;

pub enum UnifyError {
    Conflict(S<InferType>, S<InferType>),
    Recursion(S<InferType>, S<InferType>),
}

/// Union–find (disjoint-set) data structure to group type variables that must
/// have the same type. Each group has at most one concrete type binding.
///
/// See also: <https://en.wikipedia.org/wiki/Disjoint-set_data_structure>
#[derive(Debug)]
pub struct Unifier {
    pub parent: HashMap<TypeVarId, TypeVarId>,
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
        let (a_node, a_span) = a.unwrap();
        let (b_node, b_span) = b.unwrap();
        match (a_node, b_node) {
            (InferType::Var(type_var_id_a), InferType::Var(type_var_id_b)) => {
                self.unify_var_var(type_var_id_a, type_var_id_b)
            }
            (InferType::Var(type_var_id), node) => {
                self.bind(type_var_id, S::new(node, b_span))
            }
            (node, InferType::Var(type_var_id)) => {
                self.bind(type_var_id, S::new(node, a_span))
            }
            (InferType::Named(type_id_a, _), InferType::Named(type_id_b, _))
                if type_id_a == type_id_b =>
            {
                Vec::new()
            }
            (InferType::Scalar(scalar_a), InferType::Scalar(scalar_b))
                if scalar_a == scalar_b =>
            {
                Vec::new()
            }
            (InferType::Struct(struct_a), InferType::Struct(struct_b)) => {
                self.unify_structs(struct_a, struct_b, a_span, b_span)
            }
            (a_node, b_node) => {
                let span = a_span.max(b_span);
                vec![S::new(
                    UnifyError::Conflict(
                        S::new(a_node, a_span),
                        S::new(b_node, b_span),
                    ),
                    span,
                )]
            }
        }
    }

    fn unify_structs(
        &mut self,
        struct_a: Vec<(S<String>, S<InferType>)>,
        struct_b: Vec<(S<String>, S<InferType>)>,
        span_a: Span,
        span_b: Span,
    ) -> Vec<S<UnifyError>> {
        // Fields order matters here.
        let field_names_order_mismatch: bool = struct_a.iter().zip(&struct_b).any(
            |((field_a_name, _), (field_b_name, _))| field_a_name != field_b_name,
        );

        let fields_count_mismatch = struct_a.len() != struct_b.len();

        // If the field names mismatch, we don't unify the structs.
        if field_names_order_mismatch || fields_count_mismatch {
            let span = span_a.max(span_b);
            return vec![S::new(
                UnifyError::Conflict(
                    S::new(InferType::Struct(struct_a), span_a),
                    S::new(InferType::Struct(struct_b), span_b),
                ),
                span,
            )];
        }

        let mut errors = Vec::new();
        for ((field_a_name, field_a_type), (field_b_name, field_b_type)) in
            struct_a.into_iter().zip(struct_b)
        {
            if field_a_name != field_b_name {
                todo!("Field order mismatch")
            }
            errors.extend(self.unify(field_a_type, field_b_type));
        }
        errors
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
