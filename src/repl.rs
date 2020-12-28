use crate::lexer::Lexer;
use crate::token::TokenType;
use std::io::stdin;
#[allow(dead_code)]

const PROMPT: &str = ">> ";

pub fn start() {
    loop {
        let mut line = String::new();
        match stdin().read_line(&mut line) {
            Ok(0) => break,
            Err(_) => break,
            _ => {}
        };
        if line.len() == 0 {
            break;
        }
        let mut lexer = Lexer::new(line);
        let mut token = lexer.next_token();
        while token.type_token != TokenType::EOF {
            println!("{:?}", token);
            token = lexer.next_token();
        }
    }
}
