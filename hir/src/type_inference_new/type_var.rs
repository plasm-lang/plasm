use utils::ids::{HIRTypeId, TypeVarId};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InferType {
    Var(TypeVarId),
    Known(HIRTypeId),
}
