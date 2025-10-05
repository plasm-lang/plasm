use std::fmt::{Display, Formatter, Result};

use super::hir::{
    Argument, Block, Expr, ExprArena, ExprKind, Function, FunctionCall, HIR, HIRLocal, HIRType,
    Item, Statement, VariableDeclaration,
};

impl<T: DisplayHIRType> Display for HIR<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for (i, item) in self.items.iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }
            writeln!(f, "{item}")?;
        }
        Ok(())
    }
}

trait DisplayHIRType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result;
}

impl DisplayHIRType for HIRType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            HIRType::Primitive(p) => write!(f, "{p}"),
        }
    }
}

impl DisplayHIRType for Option<HIRType> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Some(ty) => ty.fmt(f),
            None => write!(f, "UNDEFINED"),
        }
    }
}

impl<T: DisplayHIRType> Display for Item<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Item::Function(func) => write!(f, "{func}"),
        }
    }
}

impl<T: DisplayHIRType> Display for Function<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "fn {}(", self.name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{arg}")?;
        }
        write!(f, ") -> ",)?;
        self.ret_ty.fmt(f)?;
        self.body.fmt(f, Some(&self.expr_arena))
    }
}

impl Display for Argument {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}: ", self.name)?;
        self.ty.fmt(f)
    }
}

trait DisplayHIRBlock<T: DisplayHIRType> {
    fn fmt(&self, f: &mut Formatter<'_>, arena: Option<&ExprArena<T>>) -> Result;
}

impl<T: DisplayHIRType> DisplayHIRBlock<T> for Block<T> {
    fn fmt(&self, f: &mut Formatter<'_>, arena: Option<&ExprArena<T>>) -> Result {
        writeln!(f, "{{")?;

        if !self.locals.is_empty() {
            writeln!(f, "    // Locals")?;
        }
        for local in &self.locals {
            writeln!(f, "    let {}", local)?;
        }

        if let Some(arena) = arena {
            if !arena.0.is_empty() {
                writeln!(f)?;
                writeln!(f, "    // Expressions")?;
                for expr in &arena.0 {
                    writeln!(f, "    {}", expr)?;
                }
            }
        }

        if !self.statements.is_empty() {
            writeln!(f)?;
            writeln!(f, "    // Statements")?;
        }
        for stmt in &self.statements {
            writeln!(f, "    {stmt}")?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl<T: DisplayHIRType> Display for HIRLocal<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}({}): ", self.name, self.id)?;
        self.ty.fmt(f)
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Statement::VariableDeclaration(var_decl) => write!(f, "{}", var_decl),
            Statement::Expr(expr_id) => write!(f, "{}", expr_id),
            Statement::Return(expr_id) => write!(f, "return {}", expr_id),
        }
    }
}

impl Display for VariableDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "let {} = {}", self.local_id, self.expr_id)
    }
}

// --- Expression-related --- //

impl<T: DisplayHIRType> Display for Expr<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}: ", self.id)?;
        self.ty.fmt(f)?;
        write!(f, " = ")?;
        match &self.kind {
            ExprKind::Literal(lit) => write!(f, "{lit}"),
            ExprKind::Local(id) => write!(f, "{id}"),
            ExprKind::FunctionCall(func_call) => write!(f, "{func_call}"),
            ExprKind::Block(block) => block.fmt(f, None),
        }
    }
}

impl Display for FunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}(", self.func_id)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{arg}")?;
        }
        write!(f, ")")
    }
}
