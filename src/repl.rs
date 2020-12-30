use crate::ast::ast::Program;
use crate::lexer::Lexer;
use crate::parser::parser::Parser;
use crate::token::TokenType;
use std::io::stdin;

#[allow(dead_code)]

const PROMPT: &str = ">> ";

pub fn start() {
    loop {
        println!("{}", PROMPT);
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
        let mut parser = Parser::new(lexer);
        let mut program = Program::new();
        program.parse_program(&mut parser);
        if parser.errors.len() > 0 {
            println!("{:?}", parser.errors);
            continue;
        }
        println!("{}", program.to_string());
    }
}
