use bimap::BiHashMap;

use diagnostic::Spanned;

use crate::hir::{Statement, VariableDeclaration};

use super::error::Error;
use super::hir::{
    Argument, Block, Expr, ExprArena, Function, HIRLocal, HIRType, Item, OptHIR, THIR,
};
use super::ids::{ExprId, FuncId, LocalId};
use super::type_annotator::TypeAnnotator;

/// For brevity
type OT = Option<HIRType>;

/// For brevity
type S<T> = Spanned<T>;

pub fn ast_to_hir(ast: ast::AST) -> (Option<THIR>, Vec<S<Error>>) {
    let (opt_hir, errors) = ast_to_opt_hir(ast);
    if !errors.is_empty() {
        return (None, errors);
    }

    let (t_hir, errors) = opt_hir_to_t_hir(opt_hir);
    (Some(t_hir), errors)
}

fn ast_to_opt_hir(ast: ast::AST) -> (OptHIR, Vec<S<Error>>) {
    let translator = ASTTranslator::new();
    translator.translate(ast)
}

fn opt_hir_to_t_hir(opt_hir: OptHIR) -> (THIR, Vec<S<Error>>) {
    let annotator = TypeAnnotator::new();
    annotator.annotate(opt_hir)
}

struct ASTTranslator {
    hir: OptHIR,
    errors: Vec<S<Error>>,
    funcs_map: BiHashMap<S<String>, FuncId>,
    next_func_id: FuncId,
    next_local_id: LocalId,
    next_expr_id: ExprId,
}

impl ASTTranslator {
    pub fn new() -> Self {
        ASTTranslator {
            hir: OptHIR::default(),
            errors: Vec::new(),
            funcs_map: BiHashMap::new(),
            next_func_id: FuncId::one(),
            next_local_id: LocalId::one(),
            next_expr_id: ExprId::one(),
        }
    }

    fn get_next_func_id(&mut self) -> FuncId {
        let id = self.next_func_id;
        self.next_func_id = self.next_func_id.increment();
        id
    }

    fn get_next_local_id(&mut self) -> LocalId {
        let id = self.next_local_id;
        self.next_local_id = self.next_local_id.increment();
        id
    }

    fn get_next_expr_id(&mut self) -> ExprId {
        let id = self.next_expr_id;
        self.next_expr_id = self.next_expr_id.increment();
        id
    }

    pub fn translate(mut self, ast: ast::AST) -> (OptHIR, Vec<S<Error>>) {
        // First pass: register root definitions
        for item in ast.items.iter() {
            match item {
                ast::Item::Function(func) => {
                    let id = self.get_next_func_id();

                    // Check for duplicate function definitions
                    if let Some(prev) = self.funcs_map.get_by_left(&func.name) {
                        let first = self
                            .funcs_map
                            .get_by_right(prev)
                            .cloned()
                            .expect("BiHashMap invariant broken");

                        let error = Error::FunctionMultipleDefinitions {
                            first,
                            second: func.name.clone(),
                        };
                        self.errors.push(S::new(error, func.name.span));
                        continue;
                    }

                    self.funcs_map.insert(func.name.clone(), id);
                }
            }
        }

        // Second pass: translate items
        for item in ast.items {
            match item {
                ast::Item::Function(func) => {
                    self.translate_function(func);
                }
            }
        }

        (self.hir, self.errors)
    }

    fn translate_function(&mut self, func: ast::Function) {
        let id = self.funcs_map.get_by_left(&func.name).cloned().unwrap();
        let name = func.name;
        let ret_ty = func
            .return_type
            .map(|ty| self.translate_type(ty.unwrap().0))
            .unwrap_or(HIRType::Primitive(ast::ast::PrimitiveType::Void));

        let mut locals: Vec<HIRLocal<OT>> = Vec::new();

        // Translate arguments

        // TODO: Handle duplicate argument names
        let mut args = Vec::new();
        for ast_arg in func.args.into_iter() {
            let (hir_arg, local) = self.translate_arg(ast_arg);
            args.push(hir_arg);
            locals.push(local);
        }

        // Translate body

        let (body, expr_arena) = self.translate_block(func.body, locals);

        let hir_func = Function {
            id,
            name,
            args,
            ret_ty,
            body,
            expr_arena,
        };
        self.hir.items.push(Item::Function(hir_func));
    }

    fn translate_arg(&mut self, ast_arg: S<ast::Argument>) -> (S<Argument>, HIRLocal<OT>) {
        let local_id = self.get_next_local_id();
        let (ast_arg, arg_span) = ast_arg.unwrap();
        let (ast_ty, ty_span) = ast_arg.ty.unwrap();
        let hir_ty = self.translate_type(ast_ty);

        let hir_arg = Argument {
            name: ast_arg.name.clone(),
            local_id,
            ty: S::new(hir_ty.clone(), ty_span),
        };

        let local = HIRLocal {
            id: local_id,
            ty: Some(hir_ty),
            name: ast_arg.name,
        };

        (S::new(hir_arg, arg_span), local)
    }

    fn translate_type(&self, ty: ast::Type) -> HIRType {
        match ty {
            ast::Type::Primitive(p) => HIRType::Primitive(p),
        }
    }

    fn translate_block(
        &mut self,
        ast_block: ast::Block,
        locals: Vec<HIRLocal<OT>>,
    ) -> (Block<OT>, ExprArena<OT>) {
        let mut locals = locals.clone();
        let mut expr_arena = ExprArena::<OT>::default();
        let mut statements = Vec::new();
        for ast_stmt in ast_block.into_iter() {
            let hir_stmt = match ast_stmt {
                ast::Statement::VariableDeclaration(variable_declaration) => {
                    // Build HIRLocal
                    // TODO: Handle duplicate variable names
                    let local_id = self.get_next_local_id();
                    let name = variable_declaration.name.clone();
                    let ty = variable_declaration
                        .ty
                        .map(|t| self.translate_type(t.unwrap().0));
                    let local = HIRLocal {
                        id: local_id,
                        ty,
                        name,
                    };
                    locals.push(local);

                    // Translate Expr and build VariableDeclaration
                    let (expr_id, local_expr_arena) =
                        self.translate_expr(variable_declaration.value);
                    expr_arena = expr_arena.join(local_expr_arena);

                    Statement::VariableDeclaration(VariableDeclaration { local_id, expr_id })
                }
                ast::Statement::Expr(expr) => {
                    let (expr_id, local_expr_arena) = self.translate_expr(expr);
                    expr_arena = expr_arena.join(local_expr_arena);
                    Statement::Expr(expr_id)
                }
                ast::Statement::Return(expr) => {
                    let (expr_id, local_expr_arena) = self.translate_expr(expr);
                    expr_arena = expr_arena.join(local_expr_arena);
                    Statement::Return(expr_id)
                }
            };

            statements.push(hir_stmt);
        }

        let block = Block { locals, statements };
        (block, expr_arena)
    }

    fn translate_expr(&mut self, expr: ast::Expr) -> (ExprId, ExprArena<OT>) {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use diagnostic::{Span, Spanned};

    fn test() {
        let ast = {
            use ast::ast::*;
            AST {
                items: vec![Item::Function(Function {
                    name: Spanned {
                        node: "main".to_string(),
                        span: Span { start: 52, end: 56 },
                    },
                    args: vec![],
                    return_type: None,
                    body: vec![
                        Statement::VariableDeclaration(VariableDeclaration {
                            name: Spanned {
                                node: "x".to_string(),
                                span: Span { start: 69, end: 70 },
                            },
                            ty: Some(Spanned {
                                node: Type::Primitive(PrimitiveType::I32),
                                span: Span { start: 72, end: 75 },
                            }),
                            value: Expr::Literal(Literal::Integer(Spanned {
                                node: "5".to_string(),
                                span: Span { start: 78, end: 79 },
                            })),
                        }),
                        Statement::Expr(Expr::FunctionCall(FunctionCall {
                            name: Spanned {
                                node: "print".to_string(),
                                span: Span { start: 84, end: 89 },
                            },
                            args: vec![CallArgument {
                                name: None,
                                value: Expr::Variable(Spanned {
                                    node: "x".to_string(),
                                    span: Span { start: 90, end: 91 },
                                }),
                            }],
                        })),
                    ],
                })],
            }
        };
    }
}
