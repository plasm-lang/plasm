use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};

use diagnostic::{MaybeSpanned, Spanned};
use utils::ids::{ExprId, FuncId, LocalId};

use super::hir::{
    Block, Expr, ExprArena, ExprKind, Function, FunctionCall, FunctionSignature, HIR, HIRType,
    Item, Statement,
};

/// Render type for THIR/OptHIR.
pub trait RenderType {
    fn render_ty(&self) -> String;
}

impl RenderType for HIRType {
    fn render_ty(&self) -> String {
        match self {
            HIRType::Primitive(p) => format!("{p}"),
        }
    }
}

impl RenderType for MaybeSpanned<HIRType> {
    fn render_ty(&self) -> String {
        match self.node {
            HIRType::Primitive(p) => format!("{p}"),
        }
    }
}

impl RenderType for Spanned<HIRType> {
    fn render_ty(&self) -> String {
        match self.node {
            HIRType::Primitive(p) => format!("{p}"),
        }
    }
}

impl RenderType for Option<Spanned<HIRType>> {
    fn render_ty(&self) -> String {
        match self {
            Some(t) => t.render_ty(),
            None => "undefined".into(),
        }
    }
}

/// Fast access to Expr by id
struct ArenaView<'a, T> {
    by_id: HashMap<ExprId, &'a Expr<T>>,
}

impl<'a, T> ArenaView<'a, T> {
    fn build(arena: &'a ExprArena<T>) -> Self {
        let mut by_id = HashMap::with_capacity(arena.0.len());
        for e in &arena.0 {
            by_id.insert(e.id, &e.node);
        }
        Self { by_id }
    }

    fn get(&self, id: &ExprId) -> Option<&'a Expr<T>> {
        self.by_id.get(id).copied()
    }
}

/// Context for printing a function
struct FnCtx<'a, T> {
    arena: ArenaView<'a, T>,
    local_names: HashMap<LocalId, String>,
    local_types: HashMap<LocalId, String>,
    func_names: &'a HashMap<FuncId, String>,
}

impl<T: RenderType> Display for HIR<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let mut func_names = HashMap::<FuncId, String>::new();
        for item in &self.items {
            if let Item::Function(fun) = item {
                func_names.insert(fun.signature.id, fun.signature.name.node.clone());
            }
        }

        for (i, item) in self.items.iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }
            match item {
                Item::Function(fun) => {
                    let s = format_function(fun, &func_names);
                    f.write_str(&s)?;
                }
            }
        }
        Ok(())
    }
}

fn format_function<T: RenderType>(
    fun: &Function<T>,
    func_names: &HashMap<FuncId, String>,
) -> String {
    // Build function context
    let mut local_names = HashMap::<LocalId, String>::new();
    let mut local_types = HashMap::<LocalId, String>::new();

    for a in &fun.signature.args {
        let arg = &a.node;
        local_names.insert(arg.local_id, arg.name.node.clone());
        local_types.insert(arg.local_id, arg.ty.node.render_ty());
    }

    for l in &fun.body.locals {
        local_names.insert(l.id, l.name.node.clone());
        local_types.insert(l.id, l.ty.render_ty());
    }

    let ctx = FnCtx {
        arena: ArenaView::build(&fun.expr_arena),
        local_names,
        local_types,
        func_names,
    };

    let mut out = String::new();

    // Function signature
    format_function_signature(&mut out, &fun.signature);

    if fun.body.statements.is_empty() {
        out.push_str(" {}\n");
        return out;
    }

    out.push_str(" {\n");

    // Body
    format_block_into(&mut out, &fun.body, &ctx, 1);

    out.push_str("}\n");
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

fn format_function_signature(out: &mut String, signature: &FunctionSignature) {
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
        out.push_str(&arg.ty.node.render_ty());
    }
    out.push(')');

    let ret = signature.ret_ty.render_ty();
    out.push_str(" -> ");
    out.push_str(&ret);
}

fn format_block_into<T: RenderType>(
    out: &mut String,
    block: &Block<T>,
    ctx: &FnCtx<T>,
    lvl: usize,
) {
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
                    .cloned()
                    .unwrap_or_else(|| "undefined".into());

                out.push_str("let ");
                out.push_str(&name);
                out.push_str(": ");
                out.push_str(&ty);
                out.push_str(" = ");
                format_expr_id_into(out, &v.expr_id, ctx);
                out.push('\n');
            }
            Statement::Expr(eid) => {
                format_expr_id_into(out, eid, ctx);
                out.push('\n');
            }
            Statement::Return(eid) => {
                out.push_str("return ");
                format_expr_id_into(out, eid, ctx);
                out.push('\n');
            }
        }
    }
}

fn format_expr_id_into<T: RenderType>(out: &mut String, id: &ExprId, ctx: &FnCtx<T>) {
    if let Some(e) = ctx.arena.get(id) {
        format_expr_into(out, e, ctx);
    } else {
        out.push_str("/*unknown_expr*/");
    }
}

fn format_expr_into<T: RenderType>(out: &mut String, e: &Expr<T>, ctx: &FnCtx<T>) {
    match &e.kind {
        ExprKind::Literal(l) => out.push_str(&format!("{l}")),
        ExprKind::Local(lid) => {
            let name = ctx
                .local_names
                .get(lid)
                .cloned()
                .unwrap_or_else(|| "/*unknown_local*/".into());
            out.push_str(&name);
        }
        ExprKind::FunctionCall(call) => format_call_into(out, call, ctx),
        ExprKind::Block(b) => {
            out.push_str("{\n");
            format_block_into(out, b, ctx, 2);
            out.push('}');
        }
    }
}

fn format_call_into<T: RenderType>(out: &mut String, call: &FunctionCall, ctx: &FnCtx<T>) {
    let fname = ctx
        .func_names
        .get(&call.func_id)
        .cloned()
        .unwrap_or_else(|| "/*unknown_fn*/".into());
    out.push_str(&fname);
    out.push('(');
    for (i, arg) in call.args.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        format_expr_id_into(out, arg, ctx);
    }
    out.push(')');
}
