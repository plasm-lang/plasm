use std::collections::HashMap;

use bimap::BiHashMap;
use diagnostic::Spanned;
use utils::ids::FuncId;

use super::annotator::annotate_function;
use super::constraint_gen::generate_function_constraints;
use super::error::TypeInferenceError;
use super::type_solver::solve_function_types;
use crate::error::Error;
use crate::hir::{
    Function, FunctionSignature, InternalFunction, Item, OptHIR, OptTyped, THIR,
    Typed,
};
use crate::types::HIRTypeArena;

// For brevity
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
                    let type_infer_res =
                        infer_func(func, &self.module_ctx, &mut self.type_arena);
                    match type_infer_res {
                        Ok(typed_func) => {
                            out_items.push(Item::Function(Function::Internal(
                                typed_func,
                            )));
                        }
                        Err(func_errors) => {
                            errors.extend(func_errors);
                        }
                    };
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
        let errors = errors
            .into_iter()
            .map(|error| error.map(Into::into))
            .collect();
        (thir, errors)
    }
}

/// This function is segregated from `TypeInferenceEngine` to simplify
/// the parallelism implementation later.
fn infer_func(
    func: InternalFunction<OptTyped>,
    module_ctx: &ModuleCtx,
    type_arena: &mut HIRTypeArena,
) -> Result<InternalFunction<Typed>, Vec<S<TypeInferenceError>>> {
    let constraints = generate_function_constraints(&func, module_ctx);
    let (solution, errors) = solve_function_types(constraints, type_arena);
    if !errors.is_empty() {
        return Err(errors);
    }
    let annotated_func = annotate_function(func, solution);
    Ok(annotated_func)
}

pub struct ModuleCtx {
    pub func_map: BiHashMap<FuncId, S<String>>,
    pub func_signatures: HashMap<FuncId, FunctionSignature>,
}
