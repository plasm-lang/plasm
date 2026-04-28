use bimap::BiHashMap;
use serde::Serialize;

use diagnostic::Spanned;
use utils::ids::HIRTypeId;
use utils::primitive_types::PrimitiveType;

type S<T> = Spanned<T>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum HIRType {
    Primitive(PrimitiveType),
    Struct(StructType),
    // Named(),
}

impl std::fmt::Display for HIRType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HIRType::Primitive(p) => write!(f, "{}", p),
            HIRType::Struct(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct StructType {
    pub fields: Vec<S<StructField>>,
}

impl std::fmt::Display for StructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fields_str = self
            .fields
            .iter()
            .map(|field| field.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "struct {{ {} }}", fields_str)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct StructField {
    pub name: S<String>,
    pub ty: S<HIRType>,
}

impl std::fmt::Display for StructField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name.node, self.ty)
    }
}

#[derive(Debug, Default, Serialize)]
pub struct HIRTypeArena {
    pub types: BiHashMap<HIRTypeId, HIRType>,
    next_type_id: HIRTypeId,
}

impl HIRTypeArena {
    pub fn new() -> Self {
        Self {
            types: BiHashMap::new(),
            next_type_id: HIRTypeId::one(),
        }
    }

    fn next_id(&mut self) -> HIRTypeId {
        let id = self.next_type_id;
        self.next_type_id = self.next_type_id.increment();
        id
    }

    pub fn insert(&mut self, ty: HIRType) -> HIRTypeId {
        let id = self.next_id();
        self.types.insert(id, ty);
        id
    }

    pub fn get_by_id(&self, id: HIRTypeId) -> Option<&HIRType> {
        self.types.get_by_left(&id)
    }

    pub fn get_or_insert(&mut self, ty: HIRType) -> HIRTypeId {
        if let Some(id) = self.types.get_by_right(&ty) {
            *id
        } else {
            self.insert(ty)
        }
    }

    pub fn void_id(&mut self) -> HIRTypeId {
        self.get_or_insert(HIRType::Primitive(PrimitiveType::Void))
    }
}
