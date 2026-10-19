use diagnostic::Spanned;
use utils::ids::{HIRTypeId, create_id_type};

use super::type_class::TypeClass;
use crate::types::HIRTypeArena;

// For brevity
type S<T> = Spanned<T>;

create_id_type!(
    /// Type variable id, used by hir (type inference module)
    TypeVarId, TypeVarMarker, "TVAR"
);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InferType {
    Var(TypeVarId),
    Known(HIRTypeId),
}

/// Knowledge about the type of a group of type variables.
#[derive(Debug, Clone)]
pub enum Binding {
    /// The type is fully known.
    Known(HIRTypeId),
    /// The type of a literal of the type class (e.g. `5` belongs to `Int`).
    Class(TypeClass),
    /// The type of a struct literal, it can be a named or an anonymous struct.
    StructShape(Vec<(S<String>, S<InferType>)>),
    /// The type of a tuple literal, it can be a named or an anonymous tuple.
    TupleShape(Vec<S<InferType>>),
}

// --- Debug Helpers --- //

#[allow(dead_code)]
#[cfg(debug_assertions)]
impl InferType {
    pub fn format(&self, arena: &HIRTypeArena) -> String {
        match self {
            InferType::Var(type_var_id) => format!("{}", type_var_id),
            InferType::Known(hir_type_id) => {
                arena.get_by_id(*hir_type_id).unwrap().format(arena)
            }
        }
    }
}
