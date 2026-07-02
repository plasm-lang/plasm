use std::collections::HashMap;

use bimap::BiHashMap;

use diagnostic::{MaybeSpanned, Spanned};
use utils::ids::{FuncId, HIRTypeId};

use crate::error::Error;
use crate::hir::{
    Function, FunctionSignature, InternalFunction, Item, OptHIR, OptTyped, THIR,
    Typed,
};
use crate::types::HIRTypeArena;

use super::constraint_gen::{
    Constraints, Equality, FunctionConstraintGen, Obligation,
};
use super::type_class::TypeClass;
use super::type_solver::FunctionTypeSolver;
use super::type_var::InferType;

// For brevity
type MS<T> = MaybeSpanned<T>;
type S<T> = Spanned<T>;

pub fn opt_hir_to_t_hir(opt_hir: OptHIR) -> (THIR, Vec<S<Error>>) {
    TypeInferenceEngine::new(opt_hir).infer_thir()
}

pub struct TypeInferenceEngine {
    items: Vec<Item<OptTyped>>,
    module_ctx: ModuleCtx,
    type_arena: HIRTypeArena,
}

impl TypeInferenceEngine {
    fn new(opt_hir: OptHIR) -> Self {
        let OptHIR {
            items,
            funcs_map,
            type_arena,
        } = opt_hir;

        let func_signatures = Self::collect_signatures(&items);

        Self {
            items,
            module_ctx: ModuleCtx {
                func_map: funcs_map,
                func_signatures,
            },
            type_arena,
        }
    }

    fn collect_signatures(
        items: &[Item<OptTyped>],
    ) -> HashMap<FuncId, FunctionSignature> {
        let mut func_signatures = HashMap::new();
        for item in items {
            if let Item::Function(func) = item {
                let sig = func.signature();
                func_signatures.insert(sig.id, sig.clone());
            }
        }
        func_signatures
    }

    fn infer_thir(mut self) -> (THIR, Vec<S<Error>>) {
        let in_items = std::mem::take(&mut self.items);
        let mut out_items: Vec<Item<Typed>> = Vec::with_capacity(in_items.len());
        let mut errors = Vec::new();
        for item in in_items.into_iter() {
            match item {
                Item::Function(Function::Internal(func)) => {
                    let (typed_func, func_errors) =
                        infer_func(func, &self.module_ctx, &mut self.type_arena);
                    out_items.push(Item::Function(Function::Internal(typed_func)));
                    errors.extend(func_errors);
                }
                Item::Function(Function::External(func)) => {
                    out_items.push(Item::Function(Function::External(func)))
                }
                Item::TypeDefinition(d) => out_items.push(Item::TypeDefinition(d)),
            }
        }
        let thir = THIR {
            items: out_items,
            funcs_map: self.module_ctx.func_map,
            type_arena: self.type_arena,
        };
        (thir, errors)
    }
}

/// This function is segregated from `TypeInferenceEngine` to simplify
/// the parallelism implementation later.
fn infer_func(
    func: InternalFunction<OptTyped>,
    module_ctx: &ModuleCtx,
    type_arena: &mut HIRTypeArena,
) -> (InternalFunction<Typed>, Vec<S<Error>>) {
    let constraints = FunctionConstraintGen::new(&func, module_ctx, type_arena)
        .generate_constraints();
    FunctionTypeSolver::new(func, constraints).solve(type_arena)
}

pub struct ModuleCtx {
    func_map: BiHashMap<FuncId, S<String>>,
    func_signatures: HashMap<FuncId, FunctionSignature>,
}
