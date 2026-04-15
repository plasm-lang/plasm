use bimap::BiHashMap;
use serde::Serialize;

use hir::HIRType;
use utils::ids::MIRTypeId;
use utils::primitive_types::PrimitiveType;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum MIRType {
    Primitive(PrimitiveType),
}

impl MIRType {
    pub fn from_hir(ty: HIRType) -> Self {
        match ty {
            hir::HIRType::Primitive(p) => MIRType::Primitive(p),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct TypeArena {
    // Structs & enums
    // named_types: Vec<MIRType>, // TODO global named types
    /// Unnamed types e.g. tuples, arrays, *i32, &u8, anonymous enums
    /// "Unnamed" means they are not binded to a single named definition,
    /// might be multiple definitions, only type construction matters.
    unnamed_types: BiHashMap<MIRTypeId, MIRType>,
    next_type_id: MIRTypeId,
}

impl TypeArena {
    pub fn new() -> Self {
        Self {
            unnamed_types: BiHashMap::new(),
            next_type_id: MIRTypeId::one(),
        }
    }

    fn next_id(&mut self) -> MIRTypeId {
        let id = self.next_type_id;
        self.next_type_id = self.next_type_id.increment();
        id
    }

    pub fn insert_unnamed(&mut self, ty: MIRType) -> MIRTypeId {
        let id = self.next_id();
        self.unnamed_types.insert(id, ty);
        id
    }

    pub fn get(&self, id: MIRTypeId) -> Option<&MIRType> {
        self.unnamed_types.get_by_left(&id)
    }

    pub fn get_or_insert(&mut self, ty: MIRType) -> MIRTypeId {
        if let Some(id) = self.unnamed_types.get_by_right(&ty) {
            *id
        } else {
            self.insert_unnamed(ty)
        }
    }
}

impl Default for TypeArena {
    fn default() -> Self {
        Self::new()
    }
}
