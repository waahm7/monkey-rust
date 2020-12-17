#[derive(Debug,Eq, PartialEq)]
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
#[derive(Debug)]
pub struct Token {
    pub type_token: TokenType,
    pub literal: String,
}
