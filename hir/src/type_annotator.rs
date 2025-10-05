use diagnostic::Spanned;

use super::error::Error;
use super::hir::{HIRType, OptHIR, THIR};

/// For brevity
type OT = Option<HIRType>;

/// For brevity
type S<T> = Spanned<T>;

pub struct TypeAnnotator {}

impl TypeAnnotator {
    pub fn new() -> Self {
        TypeAnnotator {}
    }

    pub fn annotate(&self, opt_hir: OptHIR) -> (THIR, Vec<S<Error>>) {
        todo!()
    }
}
