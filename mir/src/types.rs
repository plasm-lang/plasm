use std::num::NonZeroUsize;

use utils::ids::TypeId;
use utils::primitive_types::PrimitiveType;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRType {
    Primitive(PrimitiveType),
}

pub struct TypeArena {
    // Structs & enums
    // named_types: Vec<MIRType>, // TODO global named types
    /// Unnamed types e.g. tuples, arrays, *i32, &u8, anonymous enums
    /// "Unnamed" means they are not binded to a single named definition,
    /// might be multiple definitions, only type construction matters.
    unnamed_types: Vec<MIRType>,
}

impl TypeArena {
    pub fn new() -> Self {
        Self {
            unnamed_types: vec![],
        }
    }

    pub fn get(&self, id: TypeId) -> Option<&MIRType> {
        self.unnamed_types.get(id.raw() - 1)
    }

    // pub fn insert(&mut self, ty: MIRType) -> TypeId {
    //     self.types.push(ty);
    //     TypeId::new(NonZeroUsize::new(self.types.len()).unwrap())
    // }
}
