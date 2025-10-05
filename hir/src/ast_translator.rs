use bimap::BiHashMap;

use diagnostic::{Span, Spanned};

use crate::hir::FunctionCall;

use super::error::Error;
use super::hir::{
    Argument, Block, Expr, ExprArena, ExprKind, Function, HIRLocal, HIRType, Item, OptHIR,
    Statement, THIR, VariableDeclaration,
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

#[derive(Default, Clone)]
struct LocalsMap {
    map: BiHashMap<S<String>, LocalId>,
}

struct ASTTranslator {
    hir: OptHIR,
    errors: Vec<S<Error>>,
    funcs_map: BiHashMap<S<String>, FuncId>,
    next_func_id: FuncId,
    next_local_id: LocalId,
    next_expr_id: ExprId,
}

fn print_func_stub(id: FuncId) -> Function<OT> {
    Function {
        id,
        name: S::new("print".into(), Span::zero()),
        args: vec![S::new(
            Argument {
                name: S::new("value".into(), Span::zero()),
                local_id: LocalId::one(),
                ty: S::new(
                    HIRType::Primitive(ast::ast::PrimitiveType::I32),
                    Span::zero(),
                ),
            },
            Span::zero(),
        )],
        ret_ty: HIRType::Primitive(ast::ast::PrimitiveType::Void),
        body: Block {
            locals: vec![],
            statements: vec![],
        },
        expr_arena: ExprArena::default(),
    }
}

impl ASTTranslator {
    pub fn new() -> Self {
        // Temporary hardcode build-in functions
        // TODO: Load from stdlib
        let mut funcs_map = BiHashMap::new();
        let print_func_id = FuncId::one();
        funcs_map.insert(S::new("print".into(), Span::zero()), print_func_id);

        Self {
            hir: OptHIR::default().with_function(print_func_stub(print_func_id)),
            errors: Vec::new(),
            funcs_map,
            next_func_id: FuncId::one().increment(),
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

        let (body, expr_arena) = self.translate_block(func.body, &locals);

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
        locals: &Vec<HIRLocal<OT>>,
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
                        self.translate_expr(variable_declaration.value, &locals);
                    expr_arena = expr_arena.join(local_expr_arena);

                    Statement::VariableDeclaration(VariableDeclaration { local_id, expr_id })
                }
                ast::Statement::Expr(expr) => {
                    let (expr_id, local_expr_arena) = self.translate_expr(expr, &locals);
                    expr_arena = expr_arena.join(local_expr_arena);
                    Statement::Expr(expr_id)
                }
                ast::Statement::Return(expr) => {
                    let (expr_id, local_expr_arena) = self.translate_expr(expr, &locals);
                    expr_arena = expr_arena.join(local_expr_arena);
                    Statement::Return(expr_id)
                }
            };

            statements.push(hir_stmt);
        }

        let block = Block { locals, statements };
        (block, expr_arena)
    }

    fn translate_expr(
        &mut self,
        expr: ast::Expr,
        locals: &Vec<HIRLocal<OT>>,
    ) -> (ExprId, ExprArena<OT>) {
        let expr_id = self.get_next_expr_id();
        let mut expr_arena = ExprArena::<OT>::default();
        match expr {
            ast::Expr::Literal(lit) => {
                let hir_expr = Expr::<OT> {
                    id: expr_id,
                    ty: None,
                    kind: ExprKind::Literal(lit),
                };
                expr_arena.add(hir_expr);
            }
            ast::Expr::Variable(var) => {
                // Look up local_id
                // TODO: Change Vec to HashMap or BiHashMap for efficiency
                let local_id = locals
                    .iter()
                    .find(|local| local.name.node == var.node)
                    .map(|local| local.id);

                if let Some(local_id) = local_id {
                    let hir_expr = Expr::<OT> {
                        id: expr_id,
                        ty: None,
                        kind: ExprKind::Local(local_id),
                    };
                    expr_arena.add(hir_expr);
                } else {
                    let (name, span) = var.unwrap();
                    let err = Error::UnknownVariable { name };
                    self.errors.push(S::new(err, span));
                }
            }
            ast::Expr::FunctionCall(func_call) => {
                let (name, span) = func_call.name.unwrap();
                let func_id = self.funcs_map.get_by_left(&name).cloned();
                if let Some(func_id) = func_id {
                    // Translate arguments into ExprIds
                    let mut args = Vec::new();
                    for ast_arg in func_call.args.into_iter() {
                        let (arg_expr_id, local_expr_arena) =
                            self.translate_expr(ast_arg.value, locals);
                        expr_arena = expr_arena.join(local_expr_arena);
                        args.push(arg_expr_id);
                    }

                    // Build FunctionCall and Expr
                    let func_call = FunctionCall { func_id, args };
                    let hir_expr = Expr::<OT> {
                        id: expr_id,
                        ty: None,
                        kind: ExprKind::FunctionCall(func_call),
                    };
                    expr_arena.add(hir_expr);
                } else {
                    let err = Error::UnknownFunction { name };
                    self.errors.push(S::new(err, span));
                }
            }
            ast::Expr::Block(block) => {
                let (hir_block, local_expr_arena) = self.translate_block(block, locals);
                expr_arena = expr_arena.join(local_expr_arena);
                let hir_expr = Expr::<OT> {
                    id: expr_id,
                    ty: None,
                    kind: ExprKind::Block(hir_block),
                };
                expr_arena.add(hir_expr);
            }
        }
        (expr_id, expr_arena)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use diagnostic::{Span, Spanned};

    #[test]
    fn basic_test() {
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

        let (hir, errors) = ast_to_opt_hir(ast);
        assert!(errors.is_empty());
        println!("{hir}");
    }
}
