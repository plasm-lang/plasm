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

    pub fn zero() -> Self {
        Span { start: 0, end: 0 }
    }

    pub fn join(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    pub fn max(self, other: Span) -> Span {
        if self.start == other.start {
            if self.end >= other.end { self } else { other }
        } else if self.start > other.start {
            self
        } else {
            other
        }
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.start, self.end)
    }
}

#[derive(Debug, Eq, Clone, Serialize)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T: Copy> Copy for Spanned<T> {}

impl<T: std::cmp::PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
    }
}

impl<T: std::hash::Hash> std::hash::Hash for Spanned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.node.hash(state);
    }
}

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
    pub fn zero(node: T) -> Self {
        Self {
            node,
            span: Span::zero(),
        }
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

    #[inline]
    pub fn unwrap(self) -> (T, Span) {
        (self.node, self.span)
    }

    #[inline]
    pub fn into_maybe(self) -> MaybeSpanned<T> {
        MaybeSpanned {
            node: self.node,
            span: Some(self.span),
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

#[derive(Debug, Eq, Clone, Serialize)]
pub struct MaybeSpanned<T> {
    pub node: T,
    pub span: Option<Span>,
}

impl<T: Copy> Copy for MaybeSpanned<T> {}

impl<T: std::cmp::PartialEq> PartialEq for MaybeSpanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
    }
}

impl<T: std::hash::Hash> std::hash::Hash for MaybeSpanned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.node.hash(state);
    }
}

impl<T: std::fmt::Display> std::fmt::Display for MaybeSpanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.node)
    }
}

impl<T> MaybeSpanned<T> {
    #[inline]
    pub fn new(node: T) -> Self {
        Self { node, span: None }
    }

    #[inline]
    pub fn with_span(self, span: Span) -> Self {
        Self {
            span: Some(span),
            ..self
        }
    }

    #[inline]
    pub fn into_spanned_or(self, default_span: Span) -> Spanned<T> {
        Spanned {
            node: self.node,
            span: self.span.unwrap_or(default_span),
        }
    }

    #[inline]
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> MaybeSpanned<U> {
        MaybeSpanned {
            node: f(self.node),
            span: self.span,
        }
    }

    #[inline]
    pub fn as_ref(&self) -> MaybeSpanned<&T> {
        MaybeSpanned {
            node: &self.node,
            span: self.span,
        }
    }

    #[inline]
    pub fn as_mut(&mut self) -> MaybeSpanned<&mut T> {
        MaybeSpanned {
            node: &mut self.node,
            span: self.span,
        }
    }

    #[inline]
    pub fn unwrap(self) -> (T, Option<Span>) {
        (self.node, self.span)
    }
}

impl<T> Deref for MaybeSpanned<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

impl<T> DerefMut for MaybeSpanned<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.node
    }
}

impl<T> AsRef<T> for MaybeSpanned<T> {
    fn as_ref(&self) -> &T {
        &self.node
    }
}

impl<T> AsMut<T> for MaybeSpanned<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.node
    }
}

impl<T> Borrow<T> for MaybeSpanned<T> {
    fn borrow(&self) -> &T {
        &self.node
    }
}

impl<T> BorrowMut<T> for MaybeSpanned<T> {
    fn borrow_mut(&mut self) -> &mut T {
        &mut self.node
    }
}
