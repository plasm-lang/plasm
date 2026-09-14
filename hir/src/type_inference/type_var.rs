use utils::ids::{HIRTypeId, create_id_type};

use crate::types::HIRTypeArena;

create_id_type!(
    /// Type variable id, used by hir (type inference module)
    TypeVarId, TypeVarMarker, "TVAR"
);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InferType {
    Var(TypeVarId),
    Known(HIRTypeId),
}

impl InferType {
    #[allow(dead_code)]
    #[cfg(debug_assertions)]
    pub fn format(&self, arena: &HIRTypeArena) -> String {
        match self {
            InferType::Var(type_var_id) => format!("{}", type_var_id),
            InferType::Known(hir_type_id) => {
                arena.get_by_id(*hir_type_id).unwrap().format(arena)
            }
        }
    }
}
