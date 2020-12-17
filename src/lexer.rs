use crate::token::{Token, TokenType};
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
        let mut tok: Token;
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
            _ => Token {
                type_token: TokenType::ILLEGAL,
                literal: self.ch.to_string(),
            },
        };

        self.read_char();

        tok
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
}
