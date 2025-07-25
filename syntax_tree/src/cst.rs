use tokenizer::{Span, Token};

pub struct CST {
    pub root: CSTNode,
}

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

pub enum CSTNodeKind {
    Function,
    VariableDeclaration,
    Assignment,
    Block,
}
