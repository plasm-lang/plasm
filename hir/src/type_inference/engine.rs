use std::collections::HashMap;

use diagnostic::Spanned;
use utils::ids::FuncId;

use super::annotator::annotate_function;
use super::constraint_gen::generate_function_constraints;
use super::error::TypeInferenceError;
use super::type_solver::solve_function_types;
use crate::error::Error;
use crate::hir::{
    FuncMap, Function, FunctionSignature, InternalFunction, OptHIR, OptTyped, THIR,
    TypeDefinition, Typed,
};
use crate::types::HIRTypeArena;

// For brevity
type S<T> = Spanned<T>;

pub fn opt_hir_to_t_hir(opt_hir: OptHIR) -> (THIR, Vec<S<Error>>) {
    TypeInferenceEngine::new(opt_hir).infer_thir()
}

pub struct TypeInferenceEngine {
    type_defs: Vec<TypeDefinition>,
    funcs: Vec<Function<OptTyped>>,
    module_ctx: ModuleCtx,
    type_arena: HIRTypeArena,
}

impl TypeInferenceEngine {
    fn new(opt_hir: OptHIR) -> Self {
        let OptHIR {
            type_defs,
            funcs,
            funcs_map,
            type_arena,
        } = opt_hir;

        let func_signatures = Self::collect_signatures(&funcs);

        Self {
            type_defs,
            funcs,
            module_ctx: ModuleCtx {
                func_map: funcs_map,
                func_signatures,
            },
            type_arena,
        }
    }

    fn collect_signatures(
        funcs: &[Function<OptTyped>],
    ) -> HashMap<FuncId, FunctionSignature> {
        let mut func_signatures = HashMap::new();
        for func in funcs {
            let sig = func.signature();
            func_signatures.insert(sig.id, sig.clone());
        }
        func_signatures
    }

    fn infer_thir(mut self) -> (THIR, Vec<S<Error>>) {
        let in_funcs = std::mem::take(&mut self.funcs);
        let mut out_funcs: Vec<Function<Typed>> = Vec::with_capacity(in_funcs.len());
        let mut errors = Vec::new();
        for func in in_funcs.into_iter() {
            match func {
                Function::Internal(func) => {
                    let type_infer_res =
                        infer_func(func, &self.module_ctx, &mut self.type_arena);
                    match type_infer_res {
                        Ok(typed_func) => {
                            out_funcs.push(Function::Internal(typed_func));
                        }
                        Err(func_errors) => {
                            errors.extend(func_errors);
                        }
                    };
                }
                Function::External(func) => out_funcs.push(Function::External(func)),
            }
        }
        let thir = THIR {
            type_defs: self.type_defs,
            funcs: out_funcs,
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
    let (solution, errors) =
        solve_function_types(constraints, type_arena, module_ctx);
    if !errors.is_empty() {
        return Err(errors);
    }
    let annotated_func = annotate_function(func, solution);
    Ok(annotated_func)
}

pub struct ModuleCtx {
    pub func_map: FuncMap,
    pub func_signatures: HashMap<FuncId, FunctionSignature>,
}
