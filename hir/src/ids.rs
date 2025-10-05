use std::{marker::PhantomData, num::NonZeroU32};

#[derive(Debug)]
pub struct Id<T> {
    raw: NonZeroU32,
    _marker: PhantomData<*const T>,
}

impl<T> Copy for Id<T> {}
impl<T> Clone for Id<T> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> PartialEq for Id<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}
impl<T> Eq for Id<T> {}

impl<T> PartialOrd for Id<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        self.raw.partial_cmp(&other.raw)
    }
}

impl<T> Ord for Id<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.raw.cmp(&other.raw)
    }
}

impl<T> core::hash::Hash for Id<T> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.raw.get().hash(state)
    }
}

impl<T> Id<T> {
    #[inline]
    pub fn new(nz: NonZeroU32) -> Self {
        Self {
            raw: nz,
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn one() -> Self {
        Self::new(NonZeroU32::new(1).unwrap())
    }

    #[inline]
    pub fn increment(&self) -> Self {
        Self::new(NonZeroU32::new(self.raw.get() + 1).unwrap())
    }
}

#[derive(Debug, Default)]
pub struct FuncMarker;
#[derive(Debug, Default)]
pub struct LocalMarker;
#[derive(Debug, Default)]
pub struct ExprMarker;
#[derive(Debug, Default)]
pub struct TypeMarker;

pub type FuncId = Id<FuncMarker>;
pub type LocalId = Id<LocalMarker>;
pub type ExprId = Id<ExprMarker>;
pub type TypeId = Id<TypeMarker>;

impl std::fmt::Display for FuncMarker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FN")
    }
}

impl std::fmt::Display for LocalMarker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "LOCAL")
    }
}

impl std::fmt::Display for ExprMarker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "EXPR")
    }
}

impl std::fmt::Display for TypeMarker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TYPE")
    }
}

impl<T: std::fmt::Display + Default> std::fmt::Display for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}[{}]", T::default(), self.raw)
    }
}
