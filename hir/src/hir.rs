use std::collections::HashMap;

use ast::ast::Literal;
use bimap::BiHashMap;
use diagnostic::{MaybeSpanned, Span, Spanned};
use serde::Serialize;
use utils::ids::{ExprId, FuncId, HIRTypeId, LocalId};

use super::types::HIRTypeArena;

type S<T> = Spanned<T>;
type MS<T> = MaybeSpanned<T>;

/// Optionally typed HIR
pub type OptTyped = Option<S<HIRTypeId>>;
pub type OptHIR = HIR<OptTyped>;

/// Typed HIR
pub type Typed = MS<HIRTypeId>;
pub type THIR = HIR<Typed>;

/// High-level Intermediate Representation
/// TODO: Later rename to HIRModule, and make HIR be a collection of modules
/// For now, we only support one module, so HIR and HIRModule are the same
#[derive(Debug, Default, Serialize)]
pub struct HIR<T: Typing> {
    pub type_defs: Vec<TypeDefinition>,
    pub funcs: Vec<Function<T>>,
    /// Name and associated type of the function
    pub funcs_map: FuncMap,
    pub type_arena: HIRTypeArena,
}

impl<T: Typing> HIR<T> {
    pub fn empty() -> Self {
        Self {
            type_defs: Vec::new(),
            funcs: Vec::new(),
            funcs_map: FuncMap::new(),
            type_arena: HIRTypeArena::new(),
        }
    }
}

#[derive(Debug, Default, Serialize)]
pub struct FuncMap {
    map: BiHashMap<FuncId, (String, Option<HIRTypeId>)>,
    /// Spans of the name and the associated type
    spans: HashMap<FuncId, (Span, Option<Span>)>,
}

impl FuncMap {
    pub fn new() -> Self {
        Self {
            map: BiHashMap::new(),
            spans: HashMap::new(),
        }
    }

    /// On collision returns the id of the already registered function
    pub fn insert(
        &mut self,
        id: FuncId,
        name: S<String>,
        associated_type: Option<S<HIRTypeId>>,
    ) -> Result<(), FuncId> {
        let (name, name_span) = name.unwrap();
        let ty = associated_type.map(|ty| ty.node);
        let ty_span = associated_type.map(|ty| ty.span);
        match self.map.insert_no_overwrite(id, (name, ty)) {
            Ok(()) => {
                self.spans.insert(id, (name_span, ty_span));
                Ok(())
            }
            Err((_, key)) => {
                Err(*self.map.get_by_right(&key).expect("FuncId must be unique"))
            }
        }
    }

    pub fn get_func_id(
        &self,
        name: &str,
        associated_type: Option<HIRTypeId>,
    ) -> Option<FuncId> {
        self.map
            .get_by_right(&(name.to_string(), associated_type))
            .cloned()
    }

    pub fn get_name(&self, id: FuncId) -> Option<S<&str>> {
        let (name, _) = self.map.get_by_left(&id)?;
        let (name_span, _) = self.spans.get(&id)?;
        Some(S::new(name.as_str(), *name_span))
    }

    pub fn get_by_id(&self, id: FuncId) -> Option<(&str, Option<HIRTypeId>)> {
        self.map
            .get_by_left(&id)
            .map(|(name, ty)| (name.as_str(), *ty))
    }

    pub fn into_entries(
        self,
    ) -> impl Iterator<Item = (FuncId, S<String>, Option<S<HIRTypeId>>)> {
        let Self { map, mut spans } = self;
        map.into_iter().map(move |(id, (name, ty))| {
            let (name_span, ty_span) =
                spans.remove(&id).expect("FuncMap invariant broken");
            let ty = ty.zip(ty_span).map(|(ty, span)| S::new(ty, span));
            (id, S::new(name, name_span), ty)
        })
    }
}

#[derive(Debug, Serialize)]
pub enum Function<T: Typing> {
    Internal(InternalFunction<T>),
    External(ExternalFunction),
}

impl<T: Typing> Function<T> {
    pub fn signature(&self) -> &FunctionSignature {
        match self {
            Function::Internal(func) => &func.signature,
            Function::External(func) => &func.signature,
        }
    }
}

#[derive(Debug, Serialize)]
pub struct TypeDefinition {
    pub name: S<String>,
    pub ty: S<HIRTypeId>,
}

#[derive(Debug, Serialize)]
pub struct InternalFunction<T: Typing> {
    pub signature: FunctionSignature,
    pub body: ExprId,
    pub expr_arena: ExprArena<T>,
}

#[derive(Debug, Serialize)]
pub struct ExternalFunction {
    pub signature: FunctionSignature,
}

#[derive(Debug, Clone, Serialize)]
pub struct FunctionSignature {
    pub id: FuncId,
    pub name: S<String>,
    pub associated_type: Option<S<HIRTypeId>>,
    pub args: Vec<S<Argument>>,
    pub ret_ty: MS<HIRTypeId>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Argument {
    pub name: S<String>,
    pub local_id: LocalId,
    pub ty: S<HIRTypeId>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Block<T> {
    pub locals: Vec<HIRLocal<T>>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, Serialize)]
pub struct HIRLocal<T> {
    pub id: LocalId,
    pub ty: T,
    pub name: S<String>,
}

#[derive(Debug, Clone, Serialize)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    Assignment(ExprId, ExprId),
    Expr(ExprId),
    Return(ExprId),
}

#[derive(Debug, Clone, Serialize)]
pub struct VariableDeclaration {
    pub local_id: LocalId,
    pub expr_id: ExprId,
}

// --- Expression-related --- //

#[derive(Debug, Default, Serialize)]
pub struct ExprArena<T: Typing>(pub HashMap<ExprId, S<Expr<T>>>);

impl<T: Typing> ExprArena<T> {
    pub fn join(self, other: Self) -> Self {
        let mut v = self.0;
        v.extend(other.0);
        Self(v)
    }

    pub fn insert(&mut self, id: ExprId, expr: S<Expr<T>>) {
        self.0.insert(id, expr);
    }

    pub fn get(&self, id: ExprId) -> Option<&S<Expr<T>>> {
        self.0.get(&id)
    }
}

#[derive(Debug, Serialize)]
pub struct Expr<T: Typing> {
    pub ty: T,
    pub kind: ExprKind<T>,
}

#[derive(Debug, Clone, Serialize)]
pub enum ExprKind<T: Typing> {
    Literal(Literal),
    Local(LocalId),
    FunctionCall(FunctionCall<T>),
    Block(Block<T>),
    // Binary(Binary<T>),
    StructLiteral(StructLiteral),
    TupleLiteral(TupleLiteral),
    FieldAccess(FieldAccess),
    IndexAccess(IndexAccess),
}

#[derive(Debug, Clone, Serialize)]
pub struct FunctionCall<T: Typing> {
    pub callee: T::FunctionCallee,
    pub args: Vec<ExprId>,
}

pub trait Typing {
    type FunctionCallee: std::fmt::Debug + Clone + Serialize;
}

impl Typing for OptTyped {
    type FunctionCallee = OptTypedCallee;
}

impl Typing for Typed {
    type FunctionCallee = FuncId;
}

#[derive(Debug, Clone, Serialize)]
pub enum OptTypedCallee {
    /// `print(x)`, `Vec2.new(x, y)`
    Func(FuncId),
    /// `v.x()`, found by the type of the receiver
    Method(S<String>),
}

#[derive(Debug, Clone, Serialize)]
pub struct StructLiteral {
    pub fields: Vec<S<StructLiteralField>>,
}

#[derive(Debug, Clone, Serialize)]
pub struct StructLiteralField {
    pub name: S<String>,
    pub value: ExprId,
}

#[derive(Debug, Clone, Serialize)]
pub struct TupleLiteral(pub Vec<ExprId>);

#[derive(Debug, Clone, Serialize)]
pub struct FieldAccess {
    pub base: ExprId,
    pub field_name: S<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct IndexAccess {
    pub base: ExprId,
    pub index: S<usize>,
}
