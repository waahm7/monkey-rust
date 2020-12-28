#![allow(dead_code)]

use std::collections::HashMap;
use std::num::ParseIntError;

use crate::ast::ast::{
    Expression, IdentifierExpression, LetStatement, PrefixExpression, Program, ReturnStatement,
    Statement,
};
use crate::ast::ast::{ExpressionStatement, InfixExpression};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

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
            cur_token: Token {
                type_token: TokenType::ILLEGAL,
                literal: String::new(),
            },
            peek_token: Token {
                type_token: TokenType::ILLEGAL,
                literal: String::new(),
            },
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
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        };
    }

    fn parse_expression_statement(&mut self) -> ParseResult<Statement> {
        let expression = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::Expression(Box::new(ExpressionStatement {
            expression,
        })))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ParseResult<Expression> {
        let mut left_exp;
        if let Some(f) = self.prefix_fn() {
            left_exp = f(self)?;
        } else {
            self.no_prefix_parse_fn_error();
            return Err(format!(
                "no prefix parse function for {:?} found",
                self.cur_token.type_token
            ));
        }
        while !self.peek_token_is(&TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            match self.infix_fn() {
                None => {
                    return Ok(left_exp);
                }
                Some(f) => {
                    self.next_token();
                    left_exp = f(self, left_exp)?;
                }
            }
        }

        return Ok(left_exp);
    }

    fn infix_fn(&mut self) -> Option<InfixFn> {
        return match self.peek_token.type_token {
            TokenType::PLUS => Some(Parser::parse_infix_expression),
            TokenType::MINUS => Some(Parser::parse_infix_expression),
            TokenType::ASTERISK => Some(Parser::parse_infix_expression),
            TokenType::SLASH => Some(Parser::parse_infix_expression),
            TokenType::EQ => Some(Parser::parse_infix_expression),
            TokenType::NotEq => Some(Parser::parse_infix_expression),
            TokenType::LT => Some(Parser::parse_infix_expression),
            TokenType::GT => Some(Parser::parse_infix_expression),
            _ => None,
        };
    }

    fn parse_infix_expression(&mut self, left: Expression) -> ParseResult<Expression> {
        let precedence = self.cur_precedence();
        let operator = self.cur_token.literal.to_string();
        self.next_token();
        let right = self.parse_expression(precedence)?;
        return Ok(Expression::Infix(Box::new(InfixExpression {
            left,
            operator,
            right,
        })));
    }

    fn peek_precedence(&mut self) -> Precedence {
        Precedence::token_precedence(&self.peek_token.type_token)
    }
    fn cur_precedence(&mut self) -> Precedence {
        Precedence::token_precedence(&self.cur_token.type_token)
    }

    fn prefix_fn(&mut self) -> Option<PrefixFn> {
        match self.cur_token.type_token {
            TokenType::ILLEGAL => {}
            TokenType::EOF => {}
            TokenType::IDENT => {
                return Some(Parser::parse_identifier);
            }
            TokenType::INT => {
                return Some(Parser::parse_int_literal);
            }
            TokenType::ASSIGN => {}
            TokenType::PLUS => {}
            TokenType::MINUS => {
                return Some(Parser::parse_prefix_expression);
            }
            TokenType::BANG => {
                return Some(Parser::parse_prefix_expression);
            }
            TokenType::ASTERISK => {}
            TokenType::SLASH => {}
            TokenType::EQ => {}
            TokenType::NotEq => {}
            TokenType::COMMA => {}
            TokenType::SEMICOLON => {}
            TokenType::LPAREN => {
                return Some(Parser::parse_grouped_expression);
            }
            TokenType::RPAREN => {}
            TokenType::LBRACE => {}
            TokenType::RBRACE => {}
            TokenType::LT => {}
            TokenType::GT => {}
            TokenType::FUNCTION => {}
            TokenType::LET => {}
            TokenType::TRUE | TokenType::FALSE => {
                return Some(Parser::parse_boolean);
            }
            TokenType::IF => {}
            TokenType::ELSE => {}
            TokenType::RETURN => {}
        }

        None
    }

    fn parse_grouped_expression(&mut self) -> ParseResult<Expression> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest);
        if !self.expect_peek(&TokenType::RPAREN) {
            return Err(format!("Corresponding RPAREN not found."));
        }

        return exp;
    }

    fn parse_boolean(&mut self) -> ParseResult<Expression> {
        return Ok(Expression::Boolean(self.cur_token_is(&TokenType::TRUE)));
    }

    fn parse_prefix_expression(&mut self) -> ParseResult<Expression> {
        let operator = self.cur_token.literal.clone();
        self.next_token();

        return Ok(Expression::Prefix(Box::new(PrefixExpression {
            operator,
            right: self.parse_expression(Precedence::Prefix)?,
        })));
    }

    fn parse_int_literal(&mut self) -> ParseResult<Expression> {
        if let Ok(val) = self.cur_token.literal.parse::<i64>() {
            return Ok(Expression::Integer(val));
        }

        Err(format!(
            "Error, Unable to parse to Int {}.",
            self.cur_token.literal.to_string()
        ))
    }

    fn parse_identifier(&mut self) -> ParseResult<Expression> {
        return Ok(Expression::Identifier(self.cur_token.literal.to_string()));
    }

    fn no_prefix_parse_fn_error(&mut self) {
        self.errors.push(format!(
            "no prefix parse function for {} found",
            self.cur_token.type_token
        ));
    }

    fn parse_return_statement(&mut self) -> ParseResult<Statement> {
        let stmt = ReturnStatement {
            token: self.cur_token.clone(),
            return_value: ExpressionStatement {
                expression: Expression::Identifier(String::new()),
            },
        };
        self.next_token();

        //TODO: we are skipping expressions for now
        while !self.cur_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }
        return Ok(Statement::Return(Box::new(stmt)));
    }

    fn parse_let_statement(&mut self) -> ParseResult<Statement> {
        let mut stmt = LetStatement {
            token: self.cur_token.clone(),
            name: IdentifierExpression {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.to_string(),
            },
            value: ExpressionStatement {
                expression: Expression::Identifier(String::new()),
            },
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
        return Ok(Statement::Let(Box::new(stmt)));
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
        } else {
            self.peek_error(token_type);
        }
        false
    }

    pub fn get_errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn peek_error(&mut self, next_token: &TokenType) {
        let error_msg = String::from(format!(
            "expected next token to be {:?}, got {:?} instead.",
            next_token, self.peek_token.type_token
        ));
        self.errors.push(error_msg);
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Deref;

    use crate::ast::ast::Expression;

    use super::*;

    #[test]
    fn operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("(a + b) / c", "((a + b) / c)"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("!(true == true)", "(!(true == true))"),
        ];
        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let mut program = Program::new();
            program.parse_program(&mut parser);
            check_parse_errors(&parser);
            assert_eq!(expected, program.to_string());
        }
    }

    #[test]
    fn parse_infix_expressions() {
        let prefix_tests = vec![
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];
        for (input, left, operator, right) in prefix_tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let mut program = Program::new();
            program.parse_program(&mut parser);
            check_parse_errors(&parser);

            if program.statements.len() != 1 {
                panic!("Program statements length is not 1.")
            }

            let stmt = program.statements.get(0).unwrap();
            let stmt = match stmt {
                Statement::Expression(st) => (*st).deref(),
                _ => panic!("Not a expression statement;"),
            };

            let prefix_statement = match &stmt.expression {
                Expression::Infix(ident) => (*ident).deref(),
                _ => panic!("not a prefix statement"),
            };
            let left_expression = match &prefix_statement.left {
                Expression::Integer(x) => x,
                _ => panic!("left is not a integer."),
            };
            let right_expression = match &prefix_statement.right {
                Expression::Integer(x) => x,
                _ => panic!("right is not a integer."),
            };

            assert_eq!(left, *left_expression);
            assert_eq!(operator, prefix_statement.operator);
            assert_eq!(right, *right_expression);
        }
    }

    #[test]
    fn parse_prefix_boolean_expressions() {
        let prefix_tests = vec![
            ("!true", "!", true),
            ("!false", "!", false),
        ];
        for (input, operator, value) in prefix_tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let mut program = Program::new();
            program.parse_program(&mut parser);
            check_parse_errors(&parser);

            if program.statements.len() != 1 {
                panic!("Program statements length is not 1.")
            }

            let stmt = program.statements.get(0).unwrap();
            let stmt = match stmt {
                Statement::Expression(st) => (*st).deref(),
                _ => panic!("Not a expression statement;"),
            };

            let prefix_statement = match &stmt.expression {
                Expression::Prefix(ident) => (*ident).deref(),
                _ => panic!("not a prefix statement"),
            };

            let right = match &prefix_statement.right {
                Expression::Boolean(x) => x,
                _ => panic!("right is not a boolean."),
            };

            assert_eq!(operator, prefix_statement.operator);
            assert_eq!(value, *right);
        }
    }


    #[test]
    fn parse_prefix_expressions() {
        let prefix_tests = vec![
            ("!5;", "!", 5),
            ("-15;", "-", 15),
        ];
        for (input, operator, int_value) in prefix_tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let mut program = Program::new();
            program.parse_program(&mut parser);
            check_parse_errors(&parser);

            if program.statements.len() != 1 {
                panic!("Program statements length is not 1.")
            }

            let stmt = program.statements.get(0).unwrap();
            let stmt = match stmt {
                Statement::Expression(st) => (*st).deref(),
                _ => panic!("Not a expression statement;"),
            };

            let prefix_statement = match &stmt.expression {
                Expression::Prefix(ident) => (*ident).deref(),
                _ => panic!("not a prefix statement"),
            };

            let right = match &prefix_statement.right {
                Expression::Integer(x) => x,
                _ => panic!("right is not a integer."),
            };

            assert_eq!(operator, prefix_statement.operator);
            assert_eq!(int_value, *right);
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = String::from(
            "
        foobar;
        ",
        );
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let mut program = Program::new();
        program.parse_program(&mut parser);
        check_parse_errors(&parser);

        if program.statements.len() != 1 {
            panic!("Program statements length is not 1.")
        }

        let stmt = program.statements.get(0).unwrap();
        let stmt = match stmt {
            Statement::Expression(st) => (*st).deref(),
            _ => panic!("Not a expression statement;"),
        };
        let identifier = match &stmt.expression {
            Expression::Identifier(ident) => ident,
            _ => panic!("not an identifier"),
        };

        assert_eq!(identifier, "foobar");
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = String::from(
            "
        5;
        ",
        );
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let mut program = Program::new();
        program.parse_program(&mut parser);
        check_parse_errors(&parser);

        if program.statements.len() != 1 {
            panic!("Program statements length is not 1.")
        }

        let stmt = program.statements.get(0).unwrap();
        let stmt = match stmt {
            Statement::Expression(st) => (*st).deref(),
            _ => panic!("Not a expression statement;"),
        };
        let identifier = match &stmt.expression {
            Expression::Integer(ident) => ident,
            _ => panic!("not an identifier"),
        };
        assert_eq!(*identifier, 5);
    }

    #[test]
    fn test_return_statements() {
        let input = String::from(
            "
        return 5;
        return 10;
        return 993322;
        ",
        );
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let mut program = Program::new();
        program.parse_program(&mut parser);
        check_parse_errors(&parser);

        if program.statements.len() != 3 {
            panic!("Program statements length is not 3.")
        }

        for statement in program.statements.iter() {
            let _return_stmt = match statement {
                Statement::Return(st) => {
                    assert_eq!(st.token.literal, "return")
                }
                _ => panic!("Not a return statement;"),
            };
        }
    }

    #[test]
    fn test_let_statements() {
        let input = String::from(
            "
        let x = 5;
        let y = 10;
        let foobar = 10;
        ",
        );
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
        let tests = vec!["x", "y", "foobar"];
        for (i, e) in tests.iter().enumerate() {
            let statement = program.statements.get(i).unwrap();
            assert_eq!(test_let_statement(statement, e), true);
        }
    }

    fn test_let_statement(s: &Statement, name: &str) -> bool {
        let let_stmt = match s {
            Statement::Let(st) => st,
            _ => return false,
        };

        if let_stmt.token.literal != "let" {
            panic!("token literal not let");
        }

        if let_stmt.name.value != name {
            panic!(
                "let_stmt.name.value {} not equal to {}",
                let_stmt.name.value, name
            );
        }

        if let_stmt.name.token.literal != name {
            panic!(
                "let_stmt.name.token_literal {} not equal to {}",
                let_stmt.name.token.literal, name
            );
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
