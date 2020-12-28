#![allow(dead_code)]
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result};

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    //Identifiers + literals
    IDENT,
    INT,

    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG, // !
    ASTERISK,
    SLASH,
    EQ, // ==
    NotEq,

    // Delimeters
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LT,
    GT,

    //Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

pub fn lookup_ident(ident: &str) -> TokenType {
    //TODO: Move this out of function
    let keywords: HashMap<&'static str, TokenType> = [
        ("let", TokenType::LET),
        ("fn", TokenType::FUNCTION),
        ("true", TokenType::TRUE),
        ("false", TokenType::FALSE),
        ("if", TokenType::IF),
        ("else", TokenType::ELSE),
        ("return", TokenType::RETURN),
    ]
    .iter()
    .cloned()
    .collect();

    if keywords.contains_key(ident) {
        return keywords.get(ident).unwrap().clone();
    }
    TokenType::IDENT
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token {
    pub type_token: TokenType,
    pub literal: String,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} {}", self.type_token, self.literal)
    }
}
