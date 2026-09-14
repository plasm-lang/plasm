use std::fmt::{Debug, Display, Formatter};
use std::marker::PhantomData;
use std::num::NonZeroUsize;

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
        write!(f, "{}", self)
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

// --- Concrete Id types --- //

#[macro_export]
macro_rules! create_id_type {
    (
        $(#[$meta:meta])*
        $type_name:ident, $marker_name:ident, $display:literal
    ) => {
        #[derive(Default, Clone, Copy)]
        pub struct $marker_name;

        $(#[$meta])*
        pub type $type_name = $crate::ids::Id<$marker_name>;

        impl std::fmt::Display for $marker_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, $display)
            }
        }
    };
}

pub use create_id_type;

create_id_type!(
    /// Function id, used by hir and mir crates
    FuncId, FuncMarker, "FN"
);
create_id_type!(
    /// Local variable id, used by hir and mir crates
    LocalId, LocalMarker, "LOCAL"
);
create_id_type!(
    /// Expression id, used by hir and mir crates
    ExprId, ExprMarker, "EXPR"
);
create_id_type!(
    /// HIR type id, used by hir and mir crates
    HIRTypeId, HIRTypeMarker, "HIRT"
);
create_id_type!(
    /// MIR type id, used by mir crate
    MIRTypeId, MIRTypeMarker, "MIRT"
);
create_id_type!(
    /// Value id, used by mir and codegen crates
    ValueId, ValueMarker, "vreg"
);
