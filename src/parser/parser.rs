#![allow(dead_code)]

use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use crate::ast::ast::{Program, Statement, LetStatement, Identifier, ReturnStatement};
use crate::ast::ast::Expression;


pub struct Parser {
    pub lexer: Lexer,
    pub cur_token: Token,
    pub peek_token: Token,
    pub errors: Vec<String>,

}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            cur_token: Token { type_token: TokenType::ILLEGAL, literal: String::new() },
            peek_token: Token { type_token: TokenType::ILLEGAL, literal: String::new() },
            errors: Vec::new(),
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.type_token {
            TokenType::ILLEGAL => {}
            TokenType::EOF => {}
            TokenType::IDENT => {}
            TokenType::INT => {}
            TokenType::ASSIGN => {}
            TokenType::PLUS => {}
            TokenType::MINUS => {}
            TokenType::BANG => {}
            TokenType::ASTERISK => {}
            TokenType::SLASH => {}
            TokenType::EQ => {}
            TokenType::NotEq => {}
            TokenType::COMMA => {}
            TokenType::SEMICOLON => {}
            TokenType::LPAREN => {}
            TokenType::RPAREN => {}
            TokenType::LBRACE => {}
            TokenType::RBRACE => {}
            TokenType::LT => {}
            TokenType::GT => {}
            TokenType::FUNCTION => {}
            TokenType::LET => {
                return self.parse_let_statement();
            }
            TokenType::TRUE => {}
            TokenType::FALSE => {}
            TokenType::IF => {}
            TokenType::ELSE => {}
            TokenType::RETURN => {
                return self.parse_return_statement();
            }
        }
        None
    }
    fn parse_return_statement(&mut self) -> Option<Statement> {
        let  stmt = ReturnStatement {
            token: self.cur_token.clone(),
            return_value: Expression::Identifier(String::new()),
        };
        self.next_token();

        //TODO: we are skipping expressions for now
        while !self.cur_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        return Some(Statement::Return(Box::new(stmt)));
    }
    fn parse_let_statement(&mut self) -> Option<Statement>{

        let mut stmt = LetStatement {
            token: self.cur_token.clone(),
            name: Identifier { token: self.cur_token.clone(), value: self.cur_token.literal.to_string() },
            value: Expression::Identifier(String::new()),
        };
        if !self.expect_peek(&TokenType::IDENT) {
            return None;
        }
        stmt.name.token = self.cur_token.clone();
        stmt.name.value = self.cur_token.literal.to_string();

        if !self.expect_peek(&TokenType::ASSIGN) {
            return None;
        }

        //TODO: we are skipping expressions for now
        while !self.cur_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        return Some(Statement::Let(Box::new(stmt)))
    }

    fn cur_token_is(&self, token_type: &TokenType) -> bool {
        self.cur_token.type_token == *token_type
    }

    fn peek_token_is(&self, token_type: &TokenType) -> bool {
        self.peek_token.type_token == *token_type
    }

    fn expect_peek(&mut self, token_type: &TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();
            return true;
        }else {
            self.peek_error(token_type);
        }
        false
    }

    pub fn get_errors(&self) -> &Vec<String>{
        &self.errors
    }

    fn peek_error(&mut self, next_token: &TokenType) {
        let error_msg = String::from(format!("expected next token to be {:?}, got {:?} instead.",next_token, self.peek_token.type_token));
        self.errors.push(error_msg);
    }

    fn prefix_parse_fn () -> Expression {

    }

    fn infix_parse_fn(expression: Expression) -> Expression {

    }

}

#[cfg(test)]
mod tests{
    use super::*;

    #[test]
    fn test_return_statements(){
        let input = String::from("
        return 5;
        return 10;
        return 993322;
        ");
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let mut program = Program::new();
        program.parse_program(&mut parser);
        check_parse_errors(&parser);

        if program.statements.len() != 3 {
            panic!("Program statements length is not 3.")
        }

        for statement in program.statements.iter() {
            let  _return_stmt = match statement {
                Statement::Return(st) => { assert_eq!(st.token.literal, "return") },
                _ => panic!("Not a return statement;"),
            };

        }
    }


    #[test]
    fn test_let_statements(){
        let input = String::from("
        let x = 5;
        let y = 10;
        let foobar = 10;
        ");
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let mut program = Program::new();
        program.parse_program(&mut parser);
        check_parse_errors(&parser);
        // let program = program.unwrap_or_else({
        //    panic!("ParseProgram() returned null")
        // });
        if program.statements.len() != 3 {
            panic!("Program statements length is not 3.")
        }
        let tests = vec!["x","y","foobar"];
        for (i, e) in tests.iter().enumerate() {
            let statement = program.statements.get(i).unwrap();
            assert_eq!(test_let_statement(statement, e), true);
        }
    }

    fn test_let_statement(s: &Statement, name: &str) -> bool {
        let  let_stmt = match s {
            Statement::Let(st) => { st },
            _ => return false,
        };

        if let_stmt.token.literal != "let" {
            panic!("token literal not let");
        }

        if let_stmt.name.value != name {
            panic!("let_stmt.name.value {} not equal to {}", let_stmt.name.value, name);
        }

        if let_stmt.name.token.literal != name {
            panic!("let_stmt.name.token_literal {} not equal to {}", let_stmt.name.token.literal, name);
        }

        return true;

    }

    fn check_parse_errors(parser: &Parser) {
        let errors = &parser.errors;
        if errors.len() == 0 {
            return;
        }
        println!("parser has {} errors", errors.len());
        for str in errors {
            eprintln!("parser errors: {}", str);
        }
        panic!("fail");
    }


}
