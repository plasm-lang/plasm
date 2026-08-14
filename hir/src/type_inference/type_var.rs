use utils::ids::{HIRTypeId, TypeVarId};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InferType {
    Var(TypeVarId),
    Known(HIRTypeId),
}

impl InferType {
    pub fn type_var_id_unchecked(&self) -> TypeVarId {
        match self {
            InferType::Var(type_var_id) => *type_var_id,
            InferType::Known(_) => unreachable!(),
        }
    }
}
