use bimap::BiHashMap;

use diagnostic::{MaybeSpanned, Spanned};

use super::error::Error;
use super::hir::{
    Argument, Block, Expr, ExprArena, ExprKind, Function, FunctionCall, FunctionSignature,
    HIRLocal, HIRType, Item, OptHIR, Statement, THIR, VariableDeclaration,
};
use super::ids::{ExprId, FuncId, LocalId};
use super::type_annotator::opt_hir_to_t_hir;

/// For brevity
type OT = Option<S<HIRType>>;
type S<T> = Spanned<T>;
type MaybeS<T> = MaybeSpanned<T>;

pub fn ast_to_hir(ast: ast::AST) -> (THIR, Vec<S<Error>>) {
    let (opt_hir, translation_errors) = ast_to_opt_hir(ast);

    if !translation_errors.is_empty() {
        return (THIR::empty(), translation_errors);
    }

    let (t_hir, annotation_errors) = opt_hir_to_t_hir(opt_hir);
    (t_hir, annotation_errors)
}

fn ast_to_opt_hir(ast: ast::AST) -> (OptHIR, Vec<S<Error>>) {
    let translator = ASTTranslator::new();
    translator.translate(ast)
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
        Self {
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
            .map(|ty| ty.map(|t| self.translate_type(t)).into_maybe())
            .unwrap_or(MaybeS::new(HIRType::Primitive(
                ast::ast::PrimitiveType::Void,
            )));

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

        let signature = FunctionSignature {
            id,
            name,
            args,
            ret_ty,
        };

        let hir_func = Function {
            signature,
            body,
            expr_arena,
        };
        self.hir.items.push(Item::Function(hir_func));
    }

    fn translate_arg(&mut self, ast_arg: S<ast::Argument>) -> (S<Argument>, HIRLocal<OT>) {
        let local_id = self.get_next_local_id();
        let (ast_arg, arg_span) = ast_arg.unwrap();
        let hir_ty = ast_arg.ty.map(|t| self.translate_type(t));

        let hir_arg = Argument {
            name: ast_arg.name.clone(),
            local_id,
            ty: hir_ty.clone(),
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
                    let opt_ty = variable_declaration
                        .ty
                        .map(|t| t.map(|t| self.translate_type(t)));
                    let local = HIRLocal {
                        id: local_id,
                        ty: opt_ty,
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
        expr: S<ast::Expr>,
        locals: &Vec<HIRLocal<OT>>,
    ) -> (ExprId, ExprArena<OT>) {
        let expr_id = self.get_next_expr_id();
        let mut expr_arena = ExprArena::<OT>::default();
        match expr.node {
            ast::Expr::Literal(lit) => {
                let hir_expr = Expr::<OT> {
                    id: expr_id,
                    ty: None,
                    kind: ExprKind::Literal(lit),
                };
                expr_arena.add(S::new(hir_expr, expr.span));
            }
            ast::Expr::Variable(var) => {
                // Look up local_id
                // TODO: Change Vec to HashMap or BiHashMap for efficiency
                let local_id = locals
                    .iter()
                    .find(|local| local.name.node == var)
                    .map(|local| local.id);

                if let Some(local_id) = local_id {
                    let hir_expr = Expr::<OT> {
                        id: expr_id,
                        ty: None,
                        kind: ExprKind::Local(local_id),
                    };
                    expr_arena.add(S::new(hir_expr, expr.span));
                } else {
                    let err = Error::UnknownVariable { name: var };
                    self.errors.push(S::new(err, expr.span));
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
                    expr_arena.add(S::new(hir_expr, expr.span));
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
                expr_arena.add(S::new(hir_expr, expr.span));
            }
        }
        (expr_id, expr_arena)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::parse;
    use indoc::indoc;
    use tokenizer::tokenize;

    #[test]
    fn basic_types_inference_test() {
        let code = indoc! {"
            fn print(x: i32) {}
            fn main() {
                let a = 5
                let b = a
                let c = b
                print(c)
            }"};

        let (ast, errors) = parse(&mut tokenize(code.char_indices()));
        assert!(errors.is_empty());

        let (hir, errors) = ast_to_hir(ast);
        let expected_hir_display = indoc! {"
            fn print(x: i32) -> void {
            }

            fn main() -> void {
                let a: i32 = 5
                let b: i32 = a
                let c: i32 = b
                print(c)
            }
        "};
        assert!(errors.is_empty());
        assert_eq!(hir.to_string(), expected_hir_display);
    }
}
