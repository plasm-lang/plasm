#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    Fn,
    Let,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Number {
    Integer(String),
    Float(String),
}

impl Number {
    pub fn raw_value(&self) -> &str {
        match self {
            Self::Integer(value) => value,
            Self::Float(value) => value,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpecialSymbol {
    /// :
    Colon,
    /// =
    Equals,
    /// ,
    Comma,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Bracket {
    /// (
    RoundOpen,
    /// )
    RoundClose,
    /// [
    SquareOpen,
    /// ]
    SquareClose,
    /// {
    CurlyOpen,
    /// }
    CurlyClose,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Comment {
    SingleLine(String),
    MultiLine(String),
}

impl Comment {
    pub fn raw_value(&self) -> &str {
        match self {
            Self::SingleLine(value) => value,
            Self::MultiLine(value) => value,
        }
    }
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
