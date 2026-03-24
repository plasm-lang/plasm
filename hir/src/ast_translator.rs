use diagnostic::{MaybeSpanned, Spanned};
use utils::ids::{ExprId, FuncId, LocalId};
use utils::primitive_types::PrimitiveType;

use super::error::Error;
use super::hir::{
    Argument, Block, Expr, ExprArena, ExprKind, ExternalFunction, Function, FunctionCall,
    FunctionSignature, HIRLocal, HIRType, InternalFunction, Item, OptHIR, Statement, THIR,
    VariableDeclaration,
};
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
    next_func_id: FuncId,
    next_local_id: LocalId,
    next_expr_id: ExprId,
}

impl ASTTranslator {
    pub fn new() -> Self {
        Self {
            hir: OptHIR::default(),
            errors: Vec::new(),
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
                    if let Some(prev) = self.hir.funcs_map.get_by_right(&func.signature().name) {
                        let first = self
                            .hir
                            .funcs_map
                            .get_by_left(prev)
                            .cloned()
                            .expect("BiHashMap invariant broken");

                        let error = Error::FunctionMultipleDefinitions {
                            first,
                            second: func.signature().name.clone(),
                        };
                        self.errors.push(S::new(error, func.signature().name.span));
                        continue;
                    }

                    self.hir.funcs_map.insert(id, func.signature().name.clone());
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
        match func {
            ast::Function::Internal(func) => self.translate_internal_function(func),
            ast::Function::External(func) => self.translate_external_function(func),
        }
    }

    fn translate_signature(
        &mut self,
        signature: ast::FunctionSignature,
    ) -> (FunctionSignature, Vec<HIRLocal<OT>>) {
        let id = self
            .hir
            .funcs_map
            .get_by_right(&signature.name)
            .cloned()
            .unwrap();
        let ret_ty = signature
            .return_type
            .map(|ty| ty.map(|t| self.translate_type(t)).into_maybe())
            .unwrap_or(MaybeS::new(HIRType::Primitive(PrimitiveType::Void)));

        let mut locals: Vec<HIRLocal<OT>> = Vec::new();

        // Translate arguments

        // TODO: Handle duplicate argument names
        let mut args = Vec::new();
        for ast_arg in signature.args.into_iter() {
            let (hir_arg, local) = self.translate_arg(ast_arg);
            args.push(hir_arg);
            locals.push(local);
        }

        let signature = FunctionSignature {
            id,
            name: signature.name,
            args,
            ret_ty,
        };

        (signature, locals)
    }

    fn translate_internal_function(&mut self, func: ast::InternalFunction) {
        let (signature, locals) = self.translate_signature(func.signature);
        let ret_ty = &signature.ret_ty;

        // Translate body

        let (mut body, mut expr_arena) = self.translate_block(func.body, &locals);

        if ret_ty.node == HIRType::Primitive(PrimitiveType::Void) {
            // Check if there are return statements with expressions in void functions
            let mut has_return = false;
            for stmt in &body.statements {
                if let Statement::Return(_) = stmt {
                    has_return = true;
                    break;
                }
            }
            if !has_return {
                let expr_id = self.get_next_expr_id();
                let return_stmt = Statement::Return(expr_id);
                let return_expr = Expr::<OT> {
                    ty: None,
                    kind: ExprKind::Literal(ast::Literal::Void),
                };
                expr_arena.insert(expr_id, S::new(return_expr, signature.name.span));
                body.statements.push(return_stmt);
            }
        }

        let hir_func = Function::Internal(InternalFunction {
            signature,
            body,
            expr_arena,
        });
        self.hir.items.push(Item::Function(hir_func));
    }

    fn translate_external_function(&mut self, func: ast::ExternalFunction) {
        let (signature, _) = self.translate_signature(func.signature);
        let hir_func = Function::External(ExternalFunction { signature });
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
            let hir_stmt = match ast_stmt.node {
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
                    let (expr_id, local_expr_arena) =
                        self.translate_expr(S::new(expr, ast_stmt.span), &locals);
                    expr_arena = expr_arena.join(local_expr_arena);
                    Statement::Expr(expr_id)
                }
                ast::Statement::Return(opt_expr) => {
                    let (expr_id, local_expr_arena) = if let Some(expr) = opt_expr {
                        self.translate_expr(expr, &locals)
                    } else {
                        let void_expr = ast::Expr::Literal(ast::Literal::Void);
                        self.translate_expr(S::new(void_expr, ast_stmt.span), &locals)
                    };
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
                    ty: None,
                    kind: ExprKind::Literal(lit),
                };
                expr_arena.insert(expr_id, S::new(hir_expr, expr.span));
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
                        ty: None,
                        kind: ExprKind::Local(local_id),
                    };
                    expr_arena.insert(expr_id, S::new(hir_expr, expr.span));
                } else {
                    let err = Error::UnknownVariable { name: var };
                    self.errors.push(S::new(err, expr.span));
                }
            }
            ast::Expr::FunctionCall(func_call) => {
                let (name, span) = func_call.name.unwrap();
                let func_id = self.hir.funcs_map.get_by_right(&name).cloned();
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
                        ty: None,
                        kind: ExprKind::FunctionCall(func_call),
                    };
                    expr_arena.insert(expr_id, S::new(hir_expr, expr.span));
                } else {
                    let err = Error::UnknownFunction { name };
                    self.errors.push(S::new(err, span));
                }
            }
            ast::Expr::Block(block) => {
                let (hir_block, local_expr_arena) = self.translate_block(block, locals);
                expr_arena = expr_arena.join(local_expr_arena);
                let hir_expr = Expr::<OT> {
                    ty: None,
                    kind: ExprKind::Block(hir_block),
                };
                expr_arena.insert(expr_id, S::new(hir_expr, expr.span));
            }
            // ast::Expr::Binary(expr) => {

            // }
            _ => todo!(),
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

    fn check_by_display(code: &str, expected_display: &str) {
        let (ast, errors) = parse(&mut tokenize(code.char_indices()));
        assert!(errors.is_empty());
        let (hir, errors) = ast_to_hir(ast);
        assert!(errors.is_empty());
        assert_eq!(hir.to_string(), expected_display);
    }

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
        let expected_hir_display = indoc! {"
            fn print(x: i32) -> void {
                return void
            }

            fn main() -> void {
                let a: i32 = 5
                let b: i32 = a
                let c: i32 = b
                print(c)
                return void
            }
        "};
        check_by_display(code, expected_hir_display);
    }

    #[test]
    fn bool_type_inference_test() {
        let code = indoc! {"
            fn is_true(x: bool) {}
            fn main() {
                let a = true
                let b = a
                is_true(a)
                is_true(b)
                is_true(false)
            }"};
        let expected_hir_display = indoc! {"
            fn is_true(x: bool) -> void {
                return void
            }

            fn main() -> void {
                let a: bool = true
                let b: bool = a
                is_true(a)
                is_true(b)
                is_true(false)
                return void
            }
        "};
        check_by_display(code, expected_hir_display);
    }

    #[test]
    fn void_type_inference_test() {
        let code = indoc! {"
            fn do_nothing() {}
            fn main() {
                let a = do_nothing()
                let b = a
            }"};
        let expected_hir_display = indoc! {"
            fn do_nothing() -> void {
                return void
            }

            fn main() -> void {
                let a: void = do_nothing()
                let b: void = a
                return void
            }
        "};
        check_by_display(code, expected_hir_display);
    }

    #[test]
    fn external_function_translation_test() {
        let code = indoc! {"
            fn print(x: i32)
            fn main() {
                print(5)
            }
        "};
        let expected_hir_display = indoc! {"
            fn print(x: i32) -> void

            fn main() -> void {
                print(5)
                return void
            }
        "};
        check_by_display(code, expected_hir_display);
    }
}
