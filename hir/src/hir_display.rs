use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};

use utils::ids::{ExprId, FuncId, LocalId};

use super::hir::{
    Block, Expr, ExprArena, ExprKind, ExternalFunction, Function, FunctionCall,
    FunctionSignature, HIRLocal, InternalFunction, Item, Statement, THIR,
    TypeDefinition, Typed,
};
use super::types::{HIRType, HIRTypeArena, StructType};

fn format_hir_type(ty: &HIRType, type_arena: &HIRTypeArena) -> String {
    format_hir_type_at(ty, 0, type_arena)
}

fn format_hir_type_at(
    ty: &HIRType,
    lvl: usize,
    type_arena: &HIRTypeArena,
) -> String {
    match ty {
        HIRType::Primitive(p) => format!("{p}"),
        HIRType::Struct(s) => format_struct(s, lvl + 1, type_arena),
        HIRType::Tuple(t) => format_tuple(t, lvl + 1, type_arena),
        HIRType::Named(name, _sub_ty) => name.to_string(),
    }
}

/// Context for printing a function body.
struct FnCtx<'a> {
    expr_arena: &'a ExprArena<Typed>,
    type_arena: &'a HIRTypeArena,
    func_names: &'a HashMap<FuncId, String>,
    locals: HashMap<LocalId, &'a HIRLocal<Typed>>,
}

impl<'a> FnCtx<'a> {
    fn new(
        expr_arena: &'a ExprArena<Typed>,
        type_arena: &'a HIRTypeArena,
        func_names: &'a HashMap<FuncId, String>,
    ) -> Self {
        Self {
            expr_arena,
            type_arena,
            func_names,
            locals: HashMap::new(),
        }
    }

    /// Rebuild the context with the locals in scope for `block`.
    fn with_block(&self, block: &'a Block<Typed>) -> Self {
        let locals = block.locals.iter().map(|l| (l.id, l)).collect();
        Self {
            expr_arena: self.expr_arena,
            type_arena: self.type_arena,
            func_names: self.func_names,
            locals,
        }
    }
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

fn format_type_definition(
    def: &TypeDefinition,
    type_arena: &HIRTypeArena,
) -> String {
    let ty = type_arena.get_by_id(def.ty.node).unwrap();
    format!(
        "type {} = {}\n",
        def.name.node,
        format_hir_type(ty, type_arena)
    )
}

fn format_struct(s: &StructType, lvl: usize, type_arena: &HIRTypeArena) -> String {
    if s.fields.is_empty() {
        return "struct {}".to_string();
    }
    let mut out = String::from("struct {\n");
    for field in &s.fields {
        let name = &field.name.node;
        let ty_id = &field.ty_id.node;
        let ty = type_arena.get_by_id(*ty_id).unwrap();
        out.push_str(&indent(lvl));
        out.push_str(&format!(
            "{name}: {},\n",
            format_hir_type_at(ty, lvl, type_arena)
        ));
    }
    out.push_str(&indent(lvl.saturating_sub(1)));
    out.push('}');
    out
}

fn format_tuple(
    t: &super::types::TupleType,
    lvl: usize,
    type_arena: &HIRTypeArena,
) -> String {
    if t.0.is_empty() {
        return "()".to_string();
    }
    let mut out = String::from("(");
    for (i, ty_id) in t.0.iter().enumerate() {
        let ty = type_arena.get_by_id(ty_id.node).unwrap();
        if i > 0 {
            out.push_str(", ");
        }
        out.push_str(&format_hir_type_at(ty, lvl, type_arena));
    }
    out.push(')');
    out
}

fn format_function(
    func: &Function<Typed>,
    func_names: &HashMap<FuncId, String>,
    type_arena: &HIRTypeArena,
) -> String {
    match func {
        Function::Internal(internal) => {
            format_internal_function(internal, func_names, type_arena)
        }
        Function::External(external) => {
            format_external_function(external, type_arena)
        }
    }
}

fn format_internal_function(
    func: &InternalFunction<Typed>,
    func_names: &HashMap<FuncId, String>,
    type_arena: &HIRTypeArena,
) -> String {
    let ctx = FnCtx::new(&func.expr_arena, type_arena, func_names);

    let mut out = format_function_signature(&func.signature, type_arena);
    out.push(' ');
    // The body is an expression (always a `Block`) stored in the arena.
    out.push_str(&format_expr_id(func.body, &ctx, 0));
    out.push('\n');
    out
}

fn format_external_function(
    func: &ExternalFunction,
    type_arena: &HIRTypeArena,
) -> String {
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

fn format_function_signature(
    signature: &FunctionSignature,
    type_arena: &HIRTypeArena,
) -> String {
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
        out.push_str(&format_hir_type(ty, type_arena));
    }
    out.push(')');

    let ret_ty = type_arena.get_by_id(signature.ret_ty.node).unwrap();
    out.push_str(" -> ");
    out.push_str(&format_hir_type(ret_ty, type_arena));
    out
}

fn format_block<'a>(block: &'a Block<Typed>, ctx: &FnCtx<'a>, lvl: usize) -> String {
    // The locals in scope for a block live in the block itself.
    let ctx = ctx.with_block(block);

    if block.statements.is_empty() {
        return "{}".to_string();
    }

    let inner = lvl + 1;
    let mut out = String::from("{\n");
    for stmt in &block.statements {
        out.push_str(&indent(inner));
        match stmt {
            Statement::VariableDeclaration(v) => {
                let local = ctx.locals.get(&v.local_id);
                let name = local
                    .map(|l| l.name.node.as_str())
                    .unwrap_or("/*unknown_local*/");
                let ty = local
                    .and_then(|l| ctx.type_arena.get_by_id(l.ty.node))
                    .map(|ty| format_hir_type_at(ty, inner, ctx.type_arena))
                    .unwrap_or_else(|| "undefined".into());

                out.push_str("let ");
                out.push_str(name);
                out.push_str(": ");
                out.push_str(&ty);
                out.push_str(" = ");
                out.push_str(&format_expr_id(v.expr_id, &ctx, inner));
                out.push('\n');
            }
            Statement::Assignment(lhs, rhs) => {
                out.push_str(&format_expr_id(*lhs, &ctx, inner));
                out.push_str(" = ");
                out.push_str(&format_expr_id(*rhs, &ctx, inner));
                out.push('\n');
            }
            Statement::Expr(eid) => {
                out.push_str(&format_expr_id(*eid, &ctx, inner));
                out.push('\n');
            }
            Statement::Return(eid) => {
                out.push_str("return ");
                out.push_str(&format_expr_id(*eid, &ctx, inner));
                out.push('\n');
            }
        }
    }
    out.push_str(&indent(lvl));
    out.push('}');
    out
}

fn format_expr_id<'a>(id: ExprId, ctx: &FnCtx<'a>, lvl: usize) -> String {
    match ctx.expr_arena.get(id) {
        Some(e) => format_expr(&e.node, ctx, lvl),
        None => "/*unknown_expr*/".into(),
    }
}

fn format_expr<'a>(e: &'a Expr<Typed>, ctx: &FnCtx<'a>, lvl: usize) -> String {
    match &e.kind {
        ExprKind::Literal(l) => format!("{l}"),
        ExprKind::Local(lid) => ctx
            .locals
            .get(lid)
            .map(|l| l.name.node.clone())
            .unwrap_or_else(|| "/*unknown_local*/".into()),
        ExprKind::FunctionCall(call) => format_call(call, ctx, lvl),
        ExprKind::Block(b) => format_block(b, ctx, lvl),
        ExprKind::StructLiteral(lit) => {
            let mut out = String::from("{\n");
            for field in &lit.fields {
                let name = &field.name.node;
                let value = format_expr_id(field.value, ctx, lvl + 1);
                out.push_str(&indent(lvl + 1));
                out.push_str(&format!("{name}: {value},\n"));
            }
            out.push_str(&indent(lvl));
            out.push('}');
            out
        }
        ExprKind::TupleLiteral(lit) => {
            let mut out = String::from("(");

            if lit.0.is_empty() {
                out.push(')');
                return out;
            }

            if lit.0.len() == 1 {
                let value = format_expr_id(lit.0[0], ctx, lvl);
                out.push_str(&value);
                out.push_str(",)");
                return out;
            }

            for (i, elem) in lit.0.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                let value = format_expr_id(*elem, ctx, lvl);
                out.push_str(&value);
            }
            out.push(')');
            out
        }
        ExprKind::FieldAccess(access) => {
            let struct_expr = format_expr_id(access.base, ctx, lvl);
            let field_name = &access.field_name.node;
            format!("{struct_expr}.{field_name}")
        }
        ExprKind::IndexAccess(access) => {
            let base_expr = format_expr_id(access.base, ctx, lvl);
            let index = access.index;
            format!("{base_expr}.{index}")
        }
    }
}

fn format_call<'a>(call: &FunctionCall, ctx: &FnCtx<'a>, lvl: usize) -> String {
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
        out.push_str(&format_expr_id(*arg, ctx, lvl));
    }
    out.push(')');
    out
}
