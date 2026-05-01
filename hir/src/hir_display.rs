use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};

use utils::ids::{ExprId, FuncId, LocalId};

use super::hir::{
    Block, Expr, ExprArena, ExprKind, ExternalFunction, Function, FunctionCall, FunctionSignature,
    InternalFunction, Item, Statement, THIR, TypeDefinition, Typed,
};
use super::types::{HIRType, HIRTypeArena, StructType};

fn format_hir_type(ty: &HIRType) -> String {
    format_hir_type_at(ty, 0)
}

fn format_hir_type_at(ty: &HIRType, lvl: usize) -> String {
    match ty {
        HIRType::Primitive(p) => format!("{p}"),
        HIRType::Struct(s) => format_struct(s, lvl + 1),
        HIRType::Named(name, _sub_ty) => format!("{name}"),
    }
}

/// Context for printing a function
struct FnCtx<'a, T> {
    arena: &'a ExprArena<T>,
    local_names: HashMap<LocalId, String>,
    local_types: HashMap<LocalId, HIRType>,
    func_names: &'a HashMap<FuncId, String>,
    type_arena: &'a HIRTypeArena,
}

impl Display for THIR {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let mut func_names = HashMap::<FuncId, String>::new();
        for item in &self.items {
            if let Item::Function(func) = item {
                let signature = func.signature();
                func_names.insert(signature.id, signature.name.node.clone());
            }
        }

        for (i, item) in self.items.iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }
            match item {
                Item::Function(fun) => {
                    let s = format_function(fun, &func_names, &self.type_arena);
                    f.write_str(&s)?;
                }
                Item::TypeDefinition(def) => {
                    let s = format_type_definition(def, &self.type_arena);
                    f.write_str(&s)?;
                }
            }
        }
        Ok(())
    }
}

fn format_type_definition(def: &TypeDefinition, type_arena: &HIRTypeArena) -> String {
    let ty = type_arena.get_by_id(def.ty.node).unwrap();
    format!("type {} = {}\n", def.name.node, format_hir_type(ty))
}

fn format_struct(s: &StructType, lvl: usize) -> String {
    let mut out = String::from("struct {\n");
    for field in &s.fields {
        let name = &field.name.node;
        let ty = &field.ty.node;
        out.push_str(&indent(lvl));
        out.push_str(&format!("{name}: {},\n", format_hir_type_at(ty, lvl)));
    }
    out.push_str(&indent(lvl.saturating_sub(1)));
    out.push('}');
    out
}

fn format_function(
    func: &Function<Typed>,
    func_names: &HashMap<FuncId, String>,
    type_arena: &HIRTypeArena,
) -> String {
    match func {
        Function::Internal(internal) => format_internal_function(internal, func_names, type_arena),
        Function::External(external) => format_external_function(external, type_arena),
    }
}

fn format_internal_function(
    func: &InternalFunction<Typed>,
    func_names: &HashMap<FuncId, String>,
    type_arena: &HIRTypeArena,
) -> String {
    // Build function context
    let mut local_names = HashMap::<LocalId, String>::new();
    let mut local_types = HashMap::<LocalId, HIRType>::new();

    for a in &func.signature.args {
        let arg = &a.node;
        local_names.insert(arg.local_id, arg.name.node.clone());
        let ty = type_arena.get_by_id(arg.ty.node).unwrap();
        local_types.insert(arg.local_id, ty.clone());
    }

    for local in &func.body.locals {
        local_names.insert(local.id, local.name.node.clone());
        let ty = type_arena.get_by_id(local.ty.node).unwrap();
        local_types.insert(local.id, ty.clone());
    }

    let ctx = FnCtx {
        arena: &func.expr_arena,
        local_names,
        local_types,
        func_names,
        type_arena,
    };

    // Function signature
    let mut out = format_function_signature(&func.signature, type_arena);

    if func.body.statements.is_empty() {
        out.push_str(" {}\n");
        return out;
    }

    out.push_str(" {\n");

    // Body
    out.push_str(&format_block(&func.body, &ctx, 1));

    out.push_str("}\n");
    out
}

fn format_external_function(func: &ExternalFunction, type_arena: &HIRTypeArena) -> String {
    let mut out = format_function_signature(&func.signature, type_arena);
    out.push('\n');
    out
}

fn indent(n: usize) -> String {
    const IND: &str = "    ";
    let mut s = String::with_capacity(n * IND.len());
    for _ in 0..n {
        s.push_str(IND);
    }
    s
}

fn format_function_signature(signature: &FunctionSignature, type_arena: &HIRTypeArena) -> String {
    let mut out = String::new();
    out.push_str("fn ");
    out.push_str(&signature.name.node);
    out.push('(');
    for (i, a) in signature.args.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        let arg = &a.node;
        out.push_str(&arg.name.node);
        out.push_str(": ");
        let ty = type_arena.get_by_id(arg.ty.node).unwrap();
        out.push_str(&format_hir_type(ty));
    }
    out.push(')');

    let ret_ty = type_arena.get_by_id(signature.ret_ty.node).unwrap();
    out.push_str(" -> ");
    out.push_str(&format_hir_type(ret_ty));
    out
}

fn format_block(block: &Block<Typed>, ctx: &FnCtx<Typed>, lvl: usize) -> String {
    let mut out = String::new();
    for stmt in &block.statements {
        out.push_str(&indent(lvl));
        match stmt {
            Statement::VariableDeclaration(v) => {
                let name = ctx
                    .local_names
                    .get(&v.local_id)
                    .cloned()
                    .unwrap_or_else(|| "/*unknown_local*/".into());
                let ty = ctx
                    .local_types
                    .get(&v.local_id)
                    .map(|ty| format_hir_type_at(ty, lvl))
                    .unwrap_or_else(|| "undefined".into());

                out.push_str("let ");
                out.push_str(&name);
                out.push_str(": ");
                out.push_str(&ty);
                out.push_str(" = ");
                out.push_str(&format_expr_id(&v.expr_id, ctx, lvl));
                out.push('\n');
            }
            Statement::Expr(eid) => {
                out.push_str(&format_expr_id(eid, ctx, lvl));
                out.push('\n');
            }
            Statement::Return(eid) => {
                out.push_str("return ");
                out.push_str(&format_expr_id(eid, ctx, lvl));
                out.push('\n');
            }
        }
    }
    out
}

fn format_expr_id(id: &ExprId, ctx: &FnCtx<Typed>, lvl: usize) -> String {
    if let Some(e) = ctx.arena.get(*id) {
        format_expr(&e.node, ctx, lvl)
    } else {
        "/*unknown_expr*/".into()
    }
}

fn format_expr(e: &Expr<Typed>, ctx: &FnCtx<Typed>, lvl: usize) -> String {
    match &e.kind {
        ExprKind::Literal(l) => format!("{l}"),
        ExprKind::Local(lid) => ctx
            .local_names
            .get(lid)
            .cloned()
            .unwrap_or_else(|| "/*unknown_local*/".into()),
        ExprKind::FunctionCall(call) => format_call(call, ctx, lvl),
        ExprKind::Block(b) => {
            let mut out = String::from("{\n");
            out.push_str(&format_block(b, ctx, lvl + 1));
            out.push('}');
            out
        }
        ExprKind::StructLiteral(lit) => {
            let type_label = lit
                .type_name
                .and_then(|tn| ctx.type_arena.get_by_id(tn.node))
                .map(|ty| format_hir_type(ty))
                .unwrap_or_else(|| "struct".into());
            let mut out = format!("{type_label} {{\n");
            for field in &lit.fields {
                let name = &field.name.node;
                let value = format_expr_id(&field.value, ctx, lvl + 1);
                out.push_str(&indent(lvl + 1));
                out.push_str(&format!("{name}: {value},\n"));
            }
            out.push_str(&indent(lvl));
            out.push('}');
            out
        }
    }
}

fn format_call(call: &FunctionCall, ctx: &FnCtx<Typed>, lvl: usize) -> String {
    let mut out = ctx
        .func_names
        .get(&call.func_id)
        .cloned()
        .unwrap_or_else(|| "/*unknown_fn*/".into());
    out.push('(');
    for (i, arg) in call.args.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        out.push_str(&format_expr_id(arg, ctx, lvl));
    }
    out.push(')');
    out
}
