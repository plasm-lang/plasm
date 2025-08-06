#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    Fn,
    New,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Number {
    Integer(String),
    Float(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpecialSymbol {
    Colon,  // :
    Equals, // =
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Bracket {
    RoundOpen,   // (
    RoundClose,  // )
    SquareOpen,  // [
    SquareClose, // ]
    CurlyOpen,   // {
    CurlyClose,  // }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Comment {
    SingleLine(String),
    MultiLine(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    Number(Number),
    // StringLiteral(String),
    SpecialSymbol(SpecialSymbol),
    Bracket(Bracket),
    Whitespace(usize),
    Comment(Comment),
    NewLine,
    Impossible(String), // For unhandled cases
}
