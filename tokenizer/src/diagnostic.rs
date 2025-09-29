use std::borrow::{Borrow, BorrowMut};
use std::ops::{Deref, DerefMut};

use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }

    pub fn join(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T: Copy> Copy for Spanned<T> {}

impl<T: std::fmt::Display> std::fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.node)
    }
}

impl<T> Spanned<T> {
    #[inline]
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }

    #[inline]
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            node: f(self.node),
            span: self.span,
        }
    }

    #[inline]
    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            node: &self.node,
            span: self.span,
        }
    }

    #[inline]
    pub fn as_mut(&mut self) -> Spanned<&mut T> {
        Spanned {
            node: &mut self.node,
            span: self.span,
        }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;
    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

impl<T> DerefMut for Spanned<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.node
    }
}

impl<T> AsRef<T> for Spanned<T> {
    fn as_ref(&self) -> &T {
        &self.node
    }
}

impl<T> AsMut<T> for Spanned<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.node
    }
}

impl<T> Borrow<T> for Spanned<T> {
    fn borrow(&self) -> &T {
        &self.node
    }
}

impl<T> BorrowMut<T> for Spanned<T> {
    fn borrow_mut(&mut self) -> &mut T {
        &mut self.node
    }
}

#[derive(Debug, Clone)]
pub struct LinesTable {
    offsets: Vec<usize>,
}

impl LinesTable {
    pub fn new() -> Self {
        LinesTable { offsets: vec![0] }
    }

    pub fn offsets(&self) -> &[usize] {
        &self.offsets
    }

    pub fn len(&self) -> usize {
        self.offsets.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 1
    }

    pub fn add_line(&mut self, start_offset: usize) {
        self.offsets.push(start_offset);
    }

    pub fn last(&self) -> usize {
        self.offsets().last().copied().unwrap_or(0)
    }
}

impl Default for LinesTable {
    fn default() -> Self {
        Self::new()
    }
}
