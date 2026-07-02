use std::collections::HashMap;

use diagnostic::{MaybeSpanned, Span, Spanned};
use utils::ids::{ExprId, FuncId, HIRTypeId, LocalId, TypeVarId};
use utils::primitive_types::PrimitiveType;

use crate::hir::{
    Block, Expr, ExprArena, ExprKind, FunctionSignature, HIRLocal, InternalFunction,
    Statement,
};
use crate::types::HIRType;
use crate::types::HIRTypeArena;

use super::engine::ModuleCtx;
use super::type_class::TypeClass;
use super::type_var::InferType;

// For brevity
type S<T> = Spanned<T>;
type MS<T> = MaybeSpanned<T>;
type OT = Option<S<HIRTypeId>>;

pub struct Constraints {
    pub equalities: Vec<Equality>,
    pub obligations: Vec<Obligation>,
    pub maps: InferTypeMaps,
}

pub struct Equality(pub S<InferType>, pub S<InferType>);

pub enum Obligation {
    InClass(S<InferType>, TypeClass),
}

pub struct InferTypeMaps {
    pub expr_ty: HashMap<ExprId, S<InferType>>,
    pub local_ty: HashMap<LocalId, S<InferType>>,
}

pub(super) struct FunctionConstraintGen<'a> {
    next_type_var_id: TypeVarId,
    func: &'a InternalFunction<OT>,
    module_ctx: &'a ModuleCtx,
    type_arena: &'a HIRTypeArena,
}

impl<'a> FunctionConstraintGen<'a> {
    pub fn new(
        func: &'a InternalFunction<OT>,
        module_ctx: &'a ModuleCtx,
        type_arena: &'a HIRTypeArena,
    ) -> Self {
        FunctionConstraintGen {
            next_type_var_id: TypeVarId::one(),
            func,
            module_ctx,
            type_arena,
        }
    }

    pub fn generate_constraints(self) -> Constraints {
        todo!()
    }

    fn fresh_type_var(&mut self, span: Span) -> S<InferType> {
        let id = self.next_type_var_id;
        self.next_type_var_id = self.next_type_var_id.increment();
        S::new(InferType::Var(id), span)
    }
}
