#![allow(dead_code)]

use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use crate::ast::ast::{Program, Statement, LetStatement, IdentifierExpression, ReturnStatement, Expression};
use crate::ast::ast::ExpressionStatement;
use std::collections::HashMap;
use std::num::ParseIntError;

type ParseError = String;
type ParseErrors = Vec<ParseError>;
pub type ParseResult<T> = Result<T, ParseError>;
type PrefixFn = fn(parse: &mut Parser) -> ParseResult<Expression>;
type InfixFn = fn(parse: &mut Parser, left: Expression) -> ParseResult<Expression>;



#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

impl Precedence {
    fn token_precedence(tok: &TokenType) -> Precedence {
        match tok {
            TokenType::EQ => Precedence::Equals,
            TokenType::NotEq => Precedence::Equals,
            TokenType::LT => Precedence::LessGreater,
            TokenType::GT => Precedence::LessGreater,
            TokenType::PLUS => Precedence::Sum,
            TokenType::MINUS => Precedence::Sum,
            TokenType::SLASH => Precedence::Product,
            TokenType::ASTERISK => Precedence::Product,
            TokenType::LPAREN => Precedence::Call,
            TokenType::LBRACE => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }
}

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

    pub fn parse_statement(&mut self) -> ParseResult<Statement> {
        return match self.cur_token.type_token {
            TokenType::LET => {
                self.parse_let_statement()
            }
            TokenType::RETURN => {
                self.parse_return_statement()
            }
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> ParseResult<Statement>{
        let expression = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::Expression(Box::new(ExpressionStatement {expression})))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ParseResult<Expression> {
        let mut left_exp;
        if let Some(f) = self.prefix_fn() {
            left_exp = f(self)?;
        } else {
            return Err(format!("no prefix parse function for {:?} found", self.cur_token.type_token));
        }
        return Ok(left_exp);
    }

    fn prefix_fn(&mut self) -> Option<PrefixFn> {
        match self.cur_token.type_token {
            TokenType::ILLEGAL => {}
            TokenType::EOF => {}
            TokenType::IDENT => { return Some(Parser::parse_identifier); }
            TokenType::INT => { return Some(Parser::parse_int_literal); }
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
            TokenType::LET => {}
            TokenType::TRUE => {}
            TokenType::FALSE => {}
            TokenType::IF => {}
            TokenType::ELSE => {}
            TokenType::RETURN => {}
        }

        None
    }

    fn parse_int_literal(&mut self) -> ParseResult<Expression> {
        if let Ok(val) = self.cur_token.literal.parse::<i64>() {
            return Ok(Expression::Integer(val));
        }

        Err(format!("Error, Unable to parse to Int {}.",self.cur_token.literal.to_string()))
    }

    fn parse_identifier(&mut self) -> ParseResult<Expression> {
        return Ok(Expression::Identifier(self.cur_token.literal.to_string()));
    }

    fn parse_return_statement(&mut self) -> ParseResult<Statement> {
        let  stmt = ReturnStatement {
            token: self.cur_token.clone(),
            return_value: ExpressionStatement { expression: Expression::Identifier(String::new()) },
        };
        self.next_token();

        //TODO: we are skipping expressions for now
        while !self.cur_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }
        return Ok(Statement::Return(Box::new(stmt)));
    }

    fn parse_let_statement(&mut self) -> ParseResult<Statement>{

        let mut stmt = LetStatement {
            token: self.cur_token.clone(),
            name: IdentifierExpression { token: self.cur_token.clone(), value: self.cur_token.literal.to_string() },
            value: ExpressionStatement { expression: Expression::Identifier(String::new()) },
        };
        if !self.expect_peek(&TokenType::IDENT) {
            return Err(format!("invalid peek"));
        }
        stmt.name.token = self.cur_token.clone();
        stmt.name.value = self.cur_token.literal.to_string();

        if !self.expect_peek(&TokenType::ASSIGN) {
            return Err(format!("invalid peek"));
        }

        //TODO: we are skipping expressions for now
        while !self.cur_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }
        return Ok(Statement::Let(Box::new(stmt)))
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
}

#[cfg(test)]
mod tests{
    use super::*;
    use std::ops::Deref;
    use crate::ast::ast::Expression;

    #[test]
    fn test_identifier_expression(){
        let input = String::from("
        foobar;
        ");
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let mut program = Program::new();
        program.parse_program(&mut parser);
        check_parse_errors(&parser);

        if program.statements.len() != 1 {
            panic!("Program statements length is not 1.")
        }

        let stmt = program.statements.get(0).unwrap();
        let  stmt = match stmt {
            Statement::Expression(st) => { (*st).deref() },
            _ => panic!("Not a expression statement;"),
        };
        let identifier = match &stmt.expression {
            Expression::Identifier(ident) => { ident },
            _ => panic!("not an identifier"),
        };

        assert_eq!(identifier, "foobar");
    }


    #[test]
    fn test_integer_literal_expression(){
        let input = String::from("
        5;
        ");
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let mut program = Program::new();
        program.parse_program(&mut parser);
        check_parse_errors(&parser);

        if program.statements.len() != 1 {
            panic!("Program statements length is not 1.")
        }

        let stmt = program.statements.get(0).unwrap();
        let  stmt = match stmt {
            Statement::Expression(st) => { (*st).deref() },
            _ => panic!("Not a expression statement;"),
        };
        let identifier = match &stmt.expression {
            Expression::Integer(ident) => { ident },
            _ => panic!("not an identifier"),
        };
        assert_eq!(*identifier, 5);
    }

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
