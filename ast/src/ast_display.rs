use std::fmt::{Display, Formatter, Result};

use super::ast::{
    AST, Argument, CallArgument, Expr, Function, FunctionCall, Item, Literal, PrimitiveType,
    Statement, Type, VariableDeclaration,
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
            write!(f, "    {stmt}")?;
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

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Statement::VariableDeclaration(var_decl) => writeln!(f, "{var_decl}"),
            Statement::Expr(expr) => writeln!(f, "{expr}"),
            Statement::Return(expr) => {
                if let Some(expr) = expr {
                    writeln!(f, "return {expr}")
                } else {
                    writeln!(f, "return")
                }
            }
        }
    }
}

impl Display for VariableDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if let Some(ty) = &self.ty {
            write!(f, "let {}: {} = {}", self.name, ty, self.value)
        } else {
            write!(f, "let {} = {}", self.name, self.value)
        }
    }
}

impl Display for FunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}(", self.name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{arg}")?;
        }
        write!(f, ")")
    }
}

impl Display for CallArgument {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if let Some(name) = &self.name {
            write!(f, "{}={}", name, self.value)
        } else {
            write!(f, "{}", self.value)
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expr::Literal(lit) => write!(f, "{lit}"),
            Expr::Variable(name) => write!(f, "{name}"),
            Expr::FunctionCall(func_call) => write!(f, "{func_call}"),
            Expr::Block(block) => {
                writeln!(f, "{{")?;
                for stmt in block {
                    write!(f, "    {stmt}")?;
                }
                writeln!(f, "}}")
            }
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
