#[derive(Debug, PartialEq, Eq)]
pub enum Keyword {
    Fn,
    Let,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Number(pub String);

#[derive(Debug, PartialEq, Eq)]
pub enum SpecialSymbol {
    Colon,  // :
    Equals, // =
}

#[derive(Debug, PartialEq, Eq)]
pub enum Bracket {
    RoundOpen,   // (
    RoundClose,  // )
    SquareOpen,  // [
    SquareClose, // ]
    CurlyOpen,   // {
    CurlyClose,  // }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Comment {
    SingleLine(String),
    MultiLine(String),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    Number(Number),
    // StringLiteral(String),
    SpecialSymbol(SpecialSymbol),
    Bracket(Bracket),
    Whitespace(u16),
    Comment(Comment),
    NewLine,
    Impossible(String), // For unhandled cases
}
