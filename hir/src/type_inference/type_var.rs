use diagnostic::{MaybeSpanned, Spanned};
use utils::ids::{HIRTypeId, TypeVarId};

// For brevity
type S<T> = Spanned<T>;
type MS<T> = MaybeSpanned<T>;

pub enum InferType {
    Var(TypeVarId),
    /// Invariant: it's always id of scalar type, never constructed
    /// type (like struct or tuple).
    Scalar(HIRTypeId),
    Struct(Vec<(S<String>, S<InferType>)>),
}
