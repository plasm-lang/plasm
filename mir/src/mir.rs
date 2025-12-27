struct MIR;

pub struct Module {
    globals: Vec<Global>,
    functions: Vec<Function>,
}

enum Function {
    External(ExternalFunction),
    Internal(InternalFunction),
}

struct ExternalFunction {
    name: String,
}

struct InternalFunction {
    name: String,
    body: Vec<BasicBlock>,
    locals: Vec<Local>,
}

struct Global {
    name: String,
    raw_data: Vec<u8>,
}

struct Local {
    name: String,
}

type BlockLabel = String;

struct BasicBlock {
    label: BlockLabel,
    instructions: Vec<Instruction>,
    terminator: Terminator,
}

enum Terminator {
    GoTo(BlockLabel),
    Return,
    Call(Call),
    Unreachable,
}

struct Call {
    function: usize,
    args: Vec<Operand>,
    destination: Place,
    target: BlockLabel,
}

enum Instruction {
    Assign(Place, RValue),
    // Call(Place, String, Vec<Place>),
}

/// Operand represents a atomic value used in expressions
enum Operand {
    Load(Place), // Local variable
    Constant(Constant),
}

/// L-Value, i.e., a memory location
struct Place {
    base: PlaceBase,
    projection: Vec<ProjectionElem>,
}

pub enum PlaceBase {
    Local(usize),
    Global(usize),
}

pub enum ProjectionElem {
    /// *ptr
    Deref,
    /// struct.field
    Field(usize),
    /// arr[i]
    Index(Operand),
}

enum RValue {
    /// x = y
    Use(Operand),
    /// &y / raw pointer
    Ref(Place),
    BinaryOp(BinOp, Operand, Operand),
    UnaryOp(UnOp, Operand),
}

enum BinOp {
    /// +
    Add,
    /// -
    Sub,
    /// *
    Mul,
    /// /
    Div,
}

enum UnOp {
    /// -
    Neg,
    /// !
    Not,
}

pub enum Constant {

}
