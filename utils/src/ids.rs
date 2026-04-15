use std::{
    fmt::{Debug, Display, Formatter},
    marker::PhantomData,
    num::NonZeroUsize,
};

use serde::{Serialize, Serializer};

pub struct Id<T> {
    raw: NonZeroUsize,
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
        Some(self.cmp(other))
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

impl<T> Default for Id<T> {
    #[inline]
    fn default() -> Self {
        Self::one()
    }
}

impl<T> Id<T> {
    #[inline]
    pub fn new(nz: NonZeroUsize) -> Self {
        Self {
            raw: nz,
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn one() -> Self {
        Self::new(NonZeroUsize::new(1).unwrap())
    }

    #[inline]
    pub fn increment(&self) -> Self {
        Self::new(NonZeroUsize::new(self.raw.get() + 1).unwrap())
    }

    #[inline]
    pub fn raw(&self) -> usize {
        self.raw.get()
    }
}

impl<T: Display + Default> Display for Id<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", T::default(), self.raw)
    }
}

impl<T: Display + Default> Debug for Id<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self)
    }
}

impl<T: Display + Default> Serialize for Id<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_str(self)
    }
}

// --- particular Id types --- //

// Marker structs

#[derive(Default, Clone, Copy)]
pub struct FuncMarker;

#[derive(Default, Clone, Copy)]
pub struct LocalMarker;

#[derive(Default, Clone, Copy)]
pub struct ExprMarker;

#[derive(Default, Clone, Copy)]
pub struct HIRTypeMarker;

#[derive(Default, Clone, Copy)]
pub struct MIRTypeMarker;

#[derive(Default, Clone, Copy)]
pub struct TypeVarMarker;

#[derive(Default, Clone, Copy)]
pub struct ValueMarker;

// Type aliases

pub type FuncId = Id<FuncMarker>;
pub type LocalId = Id<LocalMarker>;
pub type ExprId = Id<ExprMarker>;
pub type HIRTypeId = Id<HIRTypeMarker>;
pub type MIRTypeId = Id<MIRTypeMarker>;
pub type TypeVarId = Id<TypeVarMarker>;
pub type ValueId = Id<ValueMarker>;

// Display impls for marker structs

impl Display for FuncMarker {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "FN")
    }
}

impl Display for LocalMarker {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "LOCAL")
    }
}

impl Display for ExprMarker {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "EXPR")
    }
}

impl Display for HIRTypeMarker {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "HIRT")
    }
}

impl Display for MIRTypeMarker {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "MIRT")
    }
}

impl Display for TypeVarMarker {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "TVAR")
    }
}

impl Display for ValueMarker {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "V")
    }
}
