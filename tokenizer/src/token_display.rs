use std::fmt::{Display, Formatter, Result};

use super::token::{Bracket, Comment, Keyword, Number, SpecialSymbol, Token};

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Token::Keyword(keyword) => write!(f, "{keyword}"),
            Token::Identifier(name) => write!(f, "{name}"),
            Token::Number(number) => write!(f, "{number}"),
            Token::SpecialSymbol(special_symbol) => write!(f, "{special_symbol}"),
            Token::Bracket(bracket) => write!(f, "{bracket}"),
            Token::Whitespace(whitespace) => write!(f, "{}", " ".repeat(*whitespace)),
            Token::Comment(comment) => write!(f, "{comment}"),
            Token::NewLine => writeln!(f),
            Token::Impossible(s) => write!(f, "{s}"),
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let s = match self {
            Keyword::Fn => "fn",
            Keyword::Let => "let",
        };
        write!(f, "{s}")
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Number::Integer(value) => write!(f, "{value}"),
            Number::Float(value) => write!(f, "{value}"),
        }
    }
}

impl Display for SpecialSymbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let s = match self {
            SpecialSymbol::Colon => ":",
            SpecialSymbol::Equals => "=",
            SpecialSymbol::Comma => ",",
        };
        write!(f, "{s}")
    }
}

impl Display for Bracket {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let s = match self {
            Bracket::RoundOpen => "(",
            Bracket::RoundClose => ")",
            Bracket::SquareOpen => "[",
            Bracket::SquareClose => "]",
            Bracket::CurlyOpen => "{",
            Bracket::CurlyClose => "}",
        };
        write!(f, "{s}")
    }
}

impl Display for Comment {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Comment::SingleLine(value) => write!(f, "//{value}"),
            Comment::MultiLine(value) => write!(f, "/*{value}*/"),
        }
    }
}
