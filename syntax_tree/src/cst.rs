use tokenizer::{Span, Token};

#[derive(Debug, Default)]
pub struct CST {
    pub items: Vec<CSTNode>,
}

impl CST {
    pub fn new() -> Self {
        CST { items: Vec::new() }
    }

    pub fn add_node(&mut self, node: CSTNode) {
        self.items.push(node);
    }
}

#[derive(Debug)]
pub enum CSTNode {
    Token {
        token: Token,
        span: Span,
    },
    Node {
        kind: CSTNodeKind,
        children: Vec<CSTNode>,
        span: Span,
    },
}

#[derive(Debug)]
pub enum CSTNodeKind {
    Function,
    VariableDeclaration,
    Assignment,
    Block,
}
