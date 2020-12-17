#![allow(dead_code)]

use crate::token::{self, Token, TokenType};
use std::vec::Vec;
struct Lexer {
    input: String,
    position: usize,
    read_pos: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_pos: 0,
            ch: '\0',
        };
        l.read_char();
        l
    }

    pub fn read_char(&mut self) {
        if self.read_pos >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.read_pos).unwrap();
        }
        self.position = self.read_pos;
        self.read_pos += 1;
    }

    pub fn next_token(&mut self) -> Token {
        //ignore spaces
        Lexer::skip_whitespace(self);

        let tok = match self.ch {
            '=' => Token {
                type_token: TokenType::ASSIGN,
                literal: self.ch.to_string(),
            },
            ';' => Token {
                type_token: TokenType::SEMICOLON,
                literal: self.ch.to_string(),
            },
            '(' => Token {
                type_token: TokenType::LPAREN,
                literal: self.ch.to_string(),
            },
            ')' => Token {
                type_token: TokenType::RPAREN,
                literal: self.ch.to_string(),
            },
            ',' => Token {
                type_token: TokenType::COMMA,
                literal: self.ch.to_string(),
            },
            '+' => Token {
                type_token: TokenType::PLUS,
                literal: self.ch.to_string(),
            },
            '{' => Token {
                type_token: TokenType::LBRACE,
                literal: self.ch.to_string(),
            },
            '}' => Token {
                type_token: TokenType::RBRACE,
                literal: self.ch.to_string(),
            },
            '\0' => Token {
                type_token: TokenType::EOF,
                literal: String::new(),
            },
            ch if ch.is_digit(10) => return Token {
                type_token: TokenType::INT,
                literal: Lexer::read_number(self),
            },
            //match identifier start
            ch if Lexer::is_letter(ch) =>  {
                let literal = Lexer::read_identifier(self);
                return Token {
                    type_token: token::lookup_ident(&literal),
                    literal,
                }
            },
            _ => Token {
                type_token: TokenType::ILLEGAL,
                literal: self.ch.to_string(),
            },
        };

        self.read_char();

        tok
    }
    fn read_number(&mut self) -> String {
        let mut vector = Vec::new();
        while self.ch.is_digit(10) {
            vector.push(self.ch);
            self.read_char();
        }
        vector.into_iter().collect()
    }
    fn read_identifier(&mut self) -> String {
        let mut vector = Vec::new();
        while Lexer::is_letter(self.ch) {
            vector.push(self.ch);
            self.read_char();
        }

        vector.into_iter().collect()
    }

    fn is_letter(ch: char) -> bool {
        ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z' || ch == '_'
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\n' || self.ch == '\r' || self.ch == '\t' {
            self.read_char();
        }
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = String::from("=+(){},;");
        let tests = vec![
            Token {
                type_token: TokenType::ASSIGN,
                literal: "=".to_string(),
            },
            Token {
                type_token: TokenType::PLUS,
                literal: "+".to_string(),
            },
            Token {
                type_token: TokenType::LPAREN,
                literal: "(".to_string(),
            },
            Token {
                type_token: TokenType::RPAREN,
                literal: ")".to_string(),
            },
            Token {
                type_token: TokenType::LBRACE,
                literal: "{".to_string(),
            },
            Token {
                type_token: TokenType::RBRACE,
                literal: "}".to_string(),
            },
            Token {
                type_token: TokenType::COMMA,
                literal: ",".to_string(),
            },
            Token {
                type_token: TokenType::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                type_token: TokenType::EOF,
                literal: String::new(),
            },
        ];

        let mut lex = Lexer::new(input);
        for (_, e) in tests.iter().enumerate() {
            let tok = lex.next_token();
            assert_eq!(tok.type_token, e.type_token);
            assert_eq!(tok.literal, e.literal);
        }
    }


    #[test]
    fn test_next_token32() {
        let input = String::from("let five = 5;
let ten = 10;
   let add = fn(x, y) {
     x + y;
};
   let result = add(five, ten);
");
        let result = vec![
            Token {
                literal: "let".to_string(),
                type_token: TokenType::LET,

            },
            Token {
                type_token: TokenType::IDENT,
                literal: "five".to_string(),
            },
            Token {
                type_token: TokenType::ASSIGN,
                literal: "=".to_string(),
            },
            Token {
                type_token: TokenType::INT,
                literal: "5".to_string(),
            },
            Token {
                type_token: TokenType::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                type_token: TokenType::LET,
                literal: "let".to_string(),
            },
            Token {
                type_token: TokenType::IDENT,
                literal: "ten".to_string(),
            },
            Token {
                type_token: TokenType::ASSIGN,
                literal: "=".to_string(),
            },
            Token {
                type_token: TokenType::INT,
                literal: "10".to_string(),
            },
            Token {
                type_token: TokenType::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                type_token: TokenType::LET,
                literal: "let".to_string(),
            },
            Token {
                type_token: TokenType::IDENT,
                literal: "add".to_string(),
            },
            Token {
                type_token: TokenType::ASSIGN,
                literal: "=".to_string(),
            },
            Token {
                type_token: TokenType::FUNCTION,
                literal: "fn".to_string(),
            },
            Token {
                type_token: TokenType::LPAREN,
                literal: "(".to_string(),
            },
            Token {
                type_token: TokenType::IDENT,
                literal: "x".to_string(),
            },
            Token {
                type_token: TokenType::COMMA,
                literal: ",".to_string(),
            },
            Token {
                type_token: TokenType::IDENT,
                literal: "y".to_string(),
            },
            Token {
                type_token: TokenType::RPAREN,
                literal: ")".to_string(),
            },
            Token {
                type_token: TokenType::LBRACE,
                literal: "{".to_string(),
            },
            Token {
                type_token: TokenType::IDENT,
                literal: "x".to_string(),
            },
            Token {
                type_token: TokenType::PLUS,
                literal: "+".to_string(),
            },
            Token {
                type_token: TokenType::IDENT,
                literal: "y".to_string(),
            },
            Token {
                type_token: TokenType::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                type_token: TokenType::RBRACE,
                literal: "}".to_string(),
            },
            Token {
                type_token: TokenType::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                type_token: TokenType::LET,
                literal: "let".to_string(),
            },
            Token {
                type_token: TokenType::IDENT,
                literal: "result".to_string(),
            },
            Token {
                type_token: TokenType::ASSIGN,
                literal: "=".to_string(),
            },
            Token {
                type_token: TokenType::IDENT,
                literal: "add".to_string(),
            },
            Token {
                type_token: TokenType::LPAREN,
                literal: "(".to_string(),
            },
            Token {
                type_token: TokenType::IDENT,
                literal: "five".to_string(),
            },
            Token {
                type_token: TokenType::COMMA,
                literal: ",".to_string(),
            },
            Token {
                type_token: TokenType::IDENT,
                literal: "ten".to_string(),
            },
            Token {
                type_token: TokenType::RPAREN,
                literal: ")".to_string(),
            },
            Token {
                type_token: TokenType::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                type_token: TokenType::EOF,
                literal: String::new(),
            },
        ];

        let mut lex = Lexer::new(input);
        for (_, e) in result.iter().enumerate() {
            let tok = lex.next_token();
            assert_eq!(tok.type_token, e.type_token);
            assert_eq!(tok.literal, e.literal);
        }
    }

}
