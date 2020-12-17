#![allow(dead_code)]
use std::collections::HashMap;

#[derive(Debug,Eq, PartialEq, Clone, Hash)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    //Identifiers + literals
    IDENT,
    INT,

    // Operators
    ASSIGN,
    PLUS,

    // Delimeters
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    //Keywords
    FUNCTION,
    LET,
}

pub  fn lookup_ident(ident: &str) -> TokenType {
    //TODO: Move this out of function
    let keywords: HashMap<&'static str, TokenType> = [("let", TokenType::LET), ("fn", TokenType::FUNCTION)]
        .iter().cloned().collect();

    if keywords.contains_key(ident) {
        return keywords.get(ident).unwrap().clone();
    }
    TokenType::IDENT
}

#[derive(Debug)]
pub struct Token {
    pub type_token: TokenType,
    pub literal: String,
}
