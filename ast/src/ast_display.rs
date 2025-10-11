use std::fmt::{Display, Formatter, Result};

use super::ast::{
    AST, Argument, BinaryExpr, BinaryOp, CallArgument, Expr, Function, FunctionCall, Item, Literal,
    PrimitiveType, Statement, Type, VariableDeclaration, UnaryExpr, UnaryOp
};

impl Display for AST {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for (i, item) in self.items.iter().enumerate() {
            if i == self.items.len() - 1 {
                write!(f, "{item}")?;
            } else {
                writeln!(f, "{item}")?;
            }
        }
        Ok(())
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Item::Function(func) => write!(f, "{func}"),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "fn {}(", self.name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{arg}")?;
        }
        write!(f, ")")?;
        if let Some(ret_type) = &self.return_type {
            write!(f, " -> {ret_type}")?;
        }

        if self.body.is_empty() {
            return writeln!(f, " {{}}");
        }

        writeln!(f, " {{")?;
        for stmt in &self.body {
            write!(f, "{}", Indent::new(&stmt.node))?;
        }
        writeln!(f, "}}")
    }
}

impl Display for Argument {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Type::Primitive(primitive_type) => write!(f, "{primitive_type}"),
        }
    }
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            PrimitiveType::Void => write!(f, "void"),
            PrimitiveType::Bool => write!(f, "bool"),
            PrimitiveType::I8 => write!(f, "i8"),
            PrimitiveType::I16 => write!(f, "i16"),
            PrimitiveType::I32 => write!(f, "i32"),
            PrimitiveType::I64 => write!(f, "i64"),
            PrimitiveType::I128 => write!(f, "i128"),
            PrimitiveType::I256 => write!(f, "i256"),
            PrimitiveType::I512 => write!(f, "i512"),
            PrimitiveType::I1024 => write!(f, "i1024"),
            PrimitiveType::U8 => write!(f, "u8"),
            PrimitiveType::U16 => write!(f, "u16"),
            PrimitiveType::U32 => write!(f, "u32"),
            PrimitiveType::U64 => write!(f, "u64"),
            PrimitiveType::U128 => write!(f, "u128"),
            PrimitiveType::U256 => write!(f, "u256"),
            PrimitiveType::U512 => write!(f, "u512"),
            PrimitiveType::U1024 => write!(f, "u1024"),
            PrimitiveType::F8 => write!(f, "f8"),
            PrimitiveType::F16 => write!(f, "f16"),
            PrimitiveType::F32 => write!(f, "f32"),
            PrimitiveType::F64 => write!(f, "f64"),
            PrimitiveType::F128 => write!(f, "f128"),
            PrimitiveType::F256 => write!(f, "f256"),
            PrimitiveType::F512 => write!(f, "f512"),
            PrimitiveType::F1024 => write!(f, "f1024"),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Literal::Void => write!(f, "void"),
            Literal::Bool(value) => write!(f, "{value}"),
            Literal::Integer(value) => write!(f, "{value}"),
            Literal::Float(spanned) => write!(f, "{spanned}"),
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let op_str = match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::DivInt => "\\",
            BinaryOp::Pow => "**",

            BinaryOp::BitAnd => "&",
            BinaryOp::BitOr => "|",
            BinaryOp::BitXor => "^",
            BinaryOp::Shl => "<<",
            BinaryOp::Shr => ">>",

            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
            BinaryOp::Eq => "==",
            BinaryOp::Neq => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Gt => ">",
            BinaryOp::Geq => ">=",
            BinaryOp::Leq => "<=",
        };
        write!(f, "{op_str}")
    }
}

/// A helper struct to display nodes with indentation
struct Indent<'a, T> {
    node: &'a T,
    indent: usize,
}

impl<'a, T> Indent<'a, T> {
    fn new(node: &'a T) -> Self {
        Self { node, indent: 1 }
    }

    fn with_indent(self, indent: usize) -> Self {
        Self { indent, ..self }
    }
}

fn write_indent(f: &mut Formatter<'_>, depth: usize) -> Result {
    for _ in 0..depth {
        f.write_str("    ")?;
    }
    Ok(())
}

// Nodes with indentation (always have indent)

impl<'a> Display for Indent<'a, Statement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use super::ast::Statement::*;
        match self.node {
            VariableDeclaration(v) => {
                write_indent(f, self.indent)?;
                writeln!(f, "{}", Indent::new(v).with_indent(self.indent))
            }
            Expr(e) => {
                write_indent(f, self.indent)?;
                write!(f, "{}", Indent::new(e).with_indent(self.indent))?;
                writeln!(f)
            }
            Return(Some(e)) => {
                write_indent(f, self.indent)?;
                writeln!(
                    f,
                    "return {}",
                    Indent::new(&e.node).with_indent(self.indent)
                )
            }
            Return(None) => {
                write_indent(f, self.indent)?;
                writeln!(f, "return")
            }
        }
    }
}

impl<'a> Display for Indent<'a, VariableDeclaration> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if let Some(ty) = &self.node.ty {
            write!(
                f,
                "let {}: {} = {}",
                self.node.name,
                ty,
                Indent::new(&self.node.value.node).with_indent(self.indent)
            )
        } else {
            write!(
                f,
                "let {} = {}",
                self.node.name,
                Indent::new(&self.node.value.node).with_indent(self.indent)
            )
        }
    }
}

impl<'a> Display for Indent<'a, FunctionCall> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}(", self.node.name)?;
        for (i, arg) in self.node.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", Indent::new(arg).with_indent(self.indent))?;
        }
        write!(f, ")")
    }
}

impl<'a> Display for Indent<'a, CallArgument> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if let Some(name) = &self.node.name {
            write!(
                f,
                "{}={}",
                name,
                Indent::new(&self.node.value.node).with_indent(self.indent)
            )
        } else {
            write!(
                f,
                "{}",
                Indent::new(&self.node.value.node).with_indent(self.indent)
            )
        }
    }
}

impl<'a> Display for Indent<'a, Expr> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use super::ast::Expr::*;
        match self.node {
            Literal(l) => write!(f, "{l}"),
            Variable(n) => write!(f, "{n}"),
            FunctionCall(fc) => write!(f, "{}", Indent::new(fc).with_indent(self.indent)),
            Unary(u) => write!(f, "{}", Indent::new(u).with_indent(self.indent)),
            Binary(b) => write!(f, "{}", Indent::new(b).with_indent(self.indent)),
            Block(stmts) => {
                if stmts.is_empty() {
                    return write!(f, "{{}}");
                }
                writeln!(f, "{{")?;
                for s in stmts {
                    write!(f, "{}", Indent::new(&s.node).with_indent(self.indent + 1))?;
                }
                write_indent(f, self.indent)?;
                write!(f, "}}")
            }
        }
    }
}

impl<'a> Display for Indent<'a, UnaryExpr> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let op_str = match self.node.op {
            UnaryOp::Negate => "-",
            UnaryOp::Not => "!",
            UnaryOp::BitNot => "~",
        };
        write!(f, "{}{}", op_str, Indent::new(&self.node.expr.node).with_indent(self.indent))
    }
}

impl<'a> Display for Indent<'a, BinaryExpr> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "({} {} {})",
            Indent::new(&self.node.left.node).with_indent(self.indent),
            self.node.op,
            Indent::new(&self.node.right.node).with_indent(self.indent)
        )
    }
}
