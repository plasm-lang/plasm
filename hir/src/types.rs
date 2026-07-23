use bimap::BiHashMap;
use diagnostic::Spanned;
use serde::Serialize;
use utils::ids::HIRTypeId;
use utils::primitive_types::PrimitiveType;

type S<T> = Spanned<T>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum HIRType {
    Primitive(PrimitiveType),
    Struct(StructType),
    Named(String, Box<HIRType>),
}

impl HIRType {
    /// Follows `Named(_, inner)` chains until a non-Named type is reached.
    pub fn peel_named(&self) -> &HIRType {
        match self {
            HIRType::Named(_, inner) => inner.peel_named(),
            other => other,
        }
    }

    pub fn format(&self, arena: &HIRTypeArena) -> String {
        match self {
            HIRType::Primitive(p) => format!("{p}"),
            HIRType::Struct(s) => s.format(arena),
            HIRType::Named(name, _sub_ty) => name.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct StructType {
    pub fields: Vec<S<StructField>>,
}

impl StructType {
    pub fn format(&self, arena: &HIRTypeArena) -> String {
        let fields_str = self
            .fields
            .iter()
            .map(|field| {
                format!(
                    "{}: {}",
                    field.name.node,
                    arena.get_by_id(field.ty_id.node).unwrap().format(arena)
                )
            })
            .collect::<Vec<_>>()
            .join(", ");
        format!("struct {{ {fields_str} }}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct StructField {
    pub name: S<String>,
    pub ty_id: S<HIRTypeId>,
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
