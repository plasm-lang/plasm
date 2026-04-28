use bimap::BiHashMap;
use serde::Serialize;

use hir::HIRType;
use utils::ids::MIRTypeId;
use utils::primitive_types::PrimitiveType;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum MIRType {
    Primitive(PrimitiveType),
    Tuple(Vec<MIRType>),
    Named(String),
}

impl MIRType {
    pub fn from_hir(ty: HIRType) -> Self {
        match ty {
            hir::HIRType::Primitive(p) => MIRType::Primitive(p),
            hir::HIRType::Struct(s) => {
                let fields = s
                    .fields
                    .into_iter()
                    .map(|field| MIRType::from_hir(field.node.ty.node))
                    .collect();
                MIRType::Tuple(fields)
            }
        }
    }
}

#[derive(Debug, Serialize)]
pub struct MIRTypeArena {
    next_type_id: MIRTypeId,
    types: BiHashMap<MIRTypeId, MIRType>,
    named_type_defs: BiHashMap<String, MIRTypeId>,
}

impl MIRTypeArena {
    pub fn new() -> Self {
        Self {
            next_type_id: MIRTypeId::one(),
            types: BiHashMap::new(),
            named_type_defs: BiHashMap::new(),
        }
    }

    fn next_id(&mut self) -> MIRTypeId {
        let id = self.next_type_id;
        self.next_type_id = self.next_type_id.increment();
        id
    }

    pub fn insert(&mut self, ty: MIRType) -> MIRTypeId {
        let id = self.next_id();
        self.types.insert(id, ty);
        id
    }

    pub fn insert_named(&mut self, name: String, ty: MIRType) -> MIRTypeId {
        let id = self.insert(ty);
        self.named_type_defs.insert(name, id);
        id
    }

    pub fn get_by_id(&self, id: MIRTypeId) -> Option<&MIRType> {
        self.types.get_by_left(&id)
    }

    pub fn get_or_insert(&mut self, ty: MIRType) -> MIRTypeId {
        if let Some(id) = self.types.get_by_right(&ty) {
            *id
        } else {
            self.insert(ty)
        }
    }

    pub fn iter_named_types(&self) -> impl Iterator<Item = (&str, &MIRType)> {
        self.named_type_defs
            .iter()
            .map(|(name, type_id)| (name.as_str(), self.types.get_by_left(type_id).unwrap()))
    }
}

impl Default for MIRTypeArena {
    fn default() -> Self {
        Self::new()
    }
}
