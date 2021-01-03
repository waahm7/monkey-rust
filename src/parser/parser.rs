#![allow(dead_code)]

use std::collections::HashMap;
use std::fmt::Debug;
use std::num::ParseIntError;

use crate::ast::ast::{
    BlockStatement, CallExpression, Expression, FunctionLiteral, IdentifierExpression,
    IfExpression, LetStatement, PrefixExpression, Program, ReturnStatement, Statement,
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
            TokenType::LPAREN => Some(Parser::parse_call_expression),
            _ => None,
        };
    }

    fn parse_call_expression(&mut self, function: Expression) -> ParseResult<Expression> {
        let arguments = self.parse_call_arguments()?;
        Ok(Expression::Call(Box::new(CallExpression {
            function,
            arguments,
        })))
    }

    fn parse_call_arguments(&mut self) -> ParseResult<Vec<Expression>> {
        let mut parameters = Vec::new();
        if self.peek_token_is(&TokenType::RPAREN) {
            self.next_token();
            return Ok(parameters);
        }
        self.next_token();
        parameters.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(&TokenType::COMMA) {
            self.next_token();
            self.next_token();
            parameters.push(self.parse_expression(Precedence::Lowest)?);
        }

        if !self.expect_peek(&TokenType::RPAREN) {
            return Err(format!(") not found in fn parameters"));
        }

        Ok(parameters)
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
            TokenType::FUNCTION => {
                return Some(Parser::parse_fn);
            }
            TokenType::LET => {}
            TokenType::TRUE | TokenType::FALSE => {
                return Some(Parser::parse_boolean);
            }
            TokenType::IF => {
                return Some(Parser::parse_if);
            }
            TokenType::ELSE => {}
            TokenType::RETURN => {}
        }

        None
    }

    fn parse_fn(&mut self) -> ParseResult<Expression> {
        if !self.expect_peek(&TokenType::LPAREN) {
            return Err(format!("( not found in fn statement"));
        }

        let parameters = self.parse_fn_parameters()?;

        if !self.expect_peek(&TokenType::LBRACE) {
            return Err(format!("{{ not found in fn block"));
        }

        let body = self.parse_block_statement();

        Ok(Expression::Fn(Box::new(FunctionLiteral {
            parameters,
            body,
        })))
    }

    fn parse_fn_parameters(&mut self) -> ParseResult<Vec<IdentifierExpression>> {
        let mut parameters = Vec::new();
        if self.peek_token_is(&TokenType::RPAREN) {
            self.next_token();
            return Ok(parameters);
        }
        self.next_token();
        let ident = IdentifierExpression {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.to_string(),
        };
        parameters.push(ident);

        while self.peek_token_is(&TokenType::COMMA) {
            self.next_token();
            self.next_token();
            parameters.push(IdentifierExpression {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.to_string(),
            })
        }
        if !self.expect_peek(&TokenType::RPAREN) {
            return Err(format!(") not found in fn parameters"));
        }

        Ok(parameters)
    }

    fn parse_if(&mut self) -> ParseResult<Expression> {
        if !self.expect_peek(&TokenType::LPAREN) {
            return Err(format!("( not found in if statement"));
        }

        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(&TokenType::RPAREN) {
            return Err(format!(") not found in if statement"));
        }
        if !self.expect_peek(&TokenType::LBRACE) {
            return Err(format!("{{ not found in if statement"));
        }

        let consequence = self.parse_block_statement();

        let mut alternative: Option<BlockStatement> = None;

        if self.peek_token_is(&TokenType::ELSE) {
            self.next_token();

            if !self.expect_peek(&TokenType::LBRACE) {
                return Err(format!("{{ not found in else statement"));
            }

            alternative = Some(self.parse_block_statement());
        }

        return Ok(Expression::If(Box::new(IfExpression {
            condition,
            consequence,
            alternative,
        })));
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut statements = Vec::new();
        self.next_token();
        while !self.cur_token_is(&TokenType::RBRACE) && !self.cur_token_is(&TokenType::EOF) {
            if let Ok(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }

        BlockStatement { statements }
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
        let mut stmt = ReturnStatement {
            token: self.cur_token.clone(),
            return_value: Expression::Identifier(String::new()),
        };
        self.next_token();

        stmt.return_value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&TokenType::SEMICOLON) {
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
            value: Expression::Identifier(String::new()),
        };
        if !self.expect_peek(&TokenType::IDENT) {
            return Err(format!("invalid peek"));
        }
        stmt.name.token = self.cur_token.clone();
        stmt.name.value = self.cur_token.literal.to_string();

        if !self.expect_peek(&TokenType::ASSIGN) {
            return Err(format!("invalid peek"));
        }

        self.next_token();
        stmt.value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&TokenType::SEMICOLON) {
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
    fn call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let mut program = Program::new();
        program.parse_program(&mut parser);
        check_parse_errors(&parser);

        if program.statements.len() != 1 {
            panic!("Program statements length is not 1.")
        }

        let stmt = program.statements.get(0).unwrap();
        let exp = match stmt {
            Statement::Expression(st) => (*st).deref(),
            _ => panic!("Not a expression statement;"),
        };

        match &exp.expression {
            Expression::Call(call) => {
                test_identifier(&call.function, "add");
                assert_eq!(call.arguments.len(), 3);
                let mut args = (&call.arguments).into_iter();
                test_integer_literal(&args.next().unwrap(), 1);
                test_infix(&args.next().unwrap(), 2, "*", 3);
                test_infix(&args.next().unwrap(), 4, "+", 5)
            }
            _ => panic!("{} is not a call expression", exp),
        }
    }

    fn test_infix(exp: &Expression, left: i64, op: &str, right: i64) {
        match exp {
            Expression::Infix(infix) => {
                assert_eq!(
                    op.to_string(),
                    infix.operator,
                    "expected {} operator but got {}",
                    op,
                    infix.operator
                );
                test_integer_literal(&infix.left, left);
                test_integer_literal(&infix.right, right);
            }
            exp => panic!("expected prefix expression but got {}", exp),
        }
    }

    #[test]
    fn parse_fn_parameters() {
        let tests = vec![
            ("fn (){};", vec![]),
            ("fn (x){};", vec!["x"]),
            ("fn (x,y,z){};", vec!["x", "y", "z"]),
        ];

        for (input, result_vector) in tests {
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

            let fn_literal = match &stmt.expression {
                Expression::Fn(exp) => (*exp).deref(),
                _ => panic!("not a function literal"),
            };

            let parameters = &fn_literal.parameters;

            if parameters.len() != result_vector.len() {
                panic!(format!(
                    "parameters length is {} and not {}.",
                    parameters.len(),
                    result_vector.len()
                ))
            }

            for (i, param) in parameters.iter().enumerate() {
                assert_eq!(param.value, result_vector.get(i).unwrap().to_string());
            }
        }
    }

    #[test]
    fn parse_function_literal() {
        let input = "fn(x,y) { x + y; }";
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

        let fn_literal = match &stmt.expression {
            Expression::Fn(exp) => (*exp).deref(),
            _ => panic!("not a function literal"),
        };

        if fn_literal.parameters.len() != 2 {
            panic!("parameters are not 2.")
        }

        let parameter1 = match fn_literal.parameters.get(0) {
            None => {
                panic!("Parameter 1 is None")
            }
            Some(x) => x,
        };
        let parameter2 = match fn_literal.parameters.get(1) {
            None => {
                panic!("Parameter 2 is None")
            }
            Some(x) => x,
        };

        assert_eq!(parameter1.value, "x");
        assert_eq!(parameter2.value, "y");

        let stmt = match &fn_literal.body.statements.get(0).unwrap() {
            Statement::Expression(st) => (*st).deref(),
            _ => panic!("Not a expression statement;"),
        };

        let infix_statement = match &stmt.expression {
            Expression::Infix(ident) => (*ident).deref(),
            _ => panic!("not a prefix statement"),
        };

        let left_expression = match &infix_statement.left {
            Expression::Identifier(x) => x,
            _ => panic!("left is not a identifier."),
        };
        let right_expression = match &infix_statement.right {
            Expression::Identifier(x) => x,
            _ => panic!("right is not a identifier."),
        };

        assert_eq!("x", *left_expression);
        assert_eq!("+", infix_statement.operator);
        assert_eq!("y", *right_expression);
    }

    #[test]
    fn parse_if_else_expressions() {
        let input = "if (x < y) { x } else { y }";
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

        let if_statement = match &stmt.expression {
            Expression::If(exp) => (*exp).deref(),
            _ => panic!("not an if statement"),
        };

        //test infix expression
        let infix_statement = match &if_statement.condition {
            Expression::Infix(ident) => (*ident).deref(),
            _ => panic!("not a prefix statement"),
        };
        let left_expression = match &infix_statement.left {
            Expression::Identifier(x) => x,
            _ => panic!("left is not a identifier."),
        };
        let right_expression = match &infix_statement.right {
            Expression::Identifier(x) => x,
            _ => panic!("right is not a identifier."),
        };

        assert_eq!("x", *left_expression);
        assert_eq!("<", infix_statement.operator);
        assert_eq!("y", *right_expression);

        let stmt = match &if_statement.consequence.statements.get(0).unwrap() {
            Statement::Expression(st) => (*st).deref(),
            _ => panic!("Not a expression statement;"),
        };

        let consequence = match &stmt.expression {
            Expression::Identifier(ident) => ident,
            _ => panic!("not an identifier"),
        };

        assert_eq!("x", *consequence);

        let alternative = match &if_statement.alternative {
            None => {
                panic!("Alternative not present.")
            }
            Some(x) => x,
        };

        let stmt = match &alternative.statements.get(0).unwrap() {
            Statement::Expression(st) => (*st).deref(),
            _ => panic!("Not a expression statement;"),
        };

        let expression = match &stmt.expression {
            Expression::Identifier(x) => x,
            _ => panic!("right is not a identifier."),
        };

        assert_eq!("y", expression);
    }

    #[test]
    fn parse_if_expressions() {
        let input = "if (x < y) { x }";
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

        let if_statement = match &stmt.expression {
            Expression::If(exp) => (*exp).deref(),
            _ => panic!("not an if statement"),
        };

        //test infix expression
        let infix_statement = match &if_statement.condition {
            Expression::Infix(ident) => (*ident).deref(),
            _ => panic!("not a prefix statement"),
        };
        let left_expression = match &infix_statement.left {
            Expression::Identifier(x) => x,
            _ => panic!("left is not a identifier."),
        };
        let right_expression = match &infix_statement.right {
            Expression::Identifier(x) => x,
            _ => panic!("right is not a identifier."),
        };

        assert_eq!("x", *left_expression);
        assert_eq!("<", infix_statement.operator);
        assert_eq!("y", *right_expression);

        let stmt = match &if_statement.consequence.statements.get(0).unwrap() {
            Statement::Expression(st) => (*st).deref(),
            _ => panic!("Not a expression statement;"),
        };

        let ident_consequence = match &stmt.expression {
            Expression::Identifier(ident) => ident,
            _ => panic!("not an identifier"),
        };

        assert_eq!("x", *ident_consequence);
    }

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
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
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
            let infix_statement = match &stmt.expression {
                Expression::Infix(ident) => (*ident).deref(),
                _ => panic!("not a prefix statement"),
            };
            let left_expression = match &infix_statement.left {
                Expression::Integer(x) => x,
                _ => panic!("left is not a integer."),
            };
            let right_expression = match &infix_statement.right {
                Expression::Integer(x) => x,
                _ => panic!("right is not a integer."),
            };

            assert_eq!(left, *left_expression);
            assert_eq!(operator, infix_statement.operator);
            assert_eq!(right, *right_expression);
        }
    }

    #[test]
    fn parse_prefix_boolean_expressions() {
        let prefix_tests = vec![("!true", "!", true), ("!false", "!", false)];
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
        let prefix_tests = vec![("!5;", "!", 5), ("-15;", "-", 15)];
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

        let result: Vec<i64> = vec![5, 10, 993322];
        for (i, statement) in program.statements.iter().enumerate() {
            match statement {
                Statement::Return(st) => {
                    assert_eq!(st.token.literal, "return");
                    test_integer_literal(&st.return_value, result.get(i).unwrap().clone());
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
        let foobar = 15;
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
        let tests = vec![("x", 5), ("y", 10), ("foobar", 15)];
        for (i, e) in tests.iter().enumerate() {
            let statement = program.statements.get(i).unwrap();
            let let_stmt = match statement {
                Statement::Let(st) => st,
                _ => panic!("not a let statement"),
            };

            assert_eq!(test_let_statement(&let_stmt, e.0), true);
            test_integer_literal(&let_stmt.value, e.1);
        }
    }

    fn test_let_statement(let_stmt: &LetStatement, name: &str) -> bool {
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

    fn test_integer_literal(exp: &Expression, value: i64) {
        match exp {
            Expression::Integer(int) => {
                assert_eq!(value, *int, "expected {} but got {}", value, int)
            }
            _ => panic!("expected integer literal {} but got {}", value, exp),
        }
    }

    fn test_boolean_literal(exp: &Expression, value: bool) {
        match exp {
            Expression::Boolean(val) => {
                assert_eq!(value, *val, "expected {} but got {}", value, val)
            }
            _ => panic!("expected boolean literal {} but got {}", value, exp),
        }
    }

    fn test_identifier(exp: &Expression, value: &str) {
        match exp {
            Expression::Identifier(ident) => {
                assert_eq!(value, ident, "expected {} but got {}", value, ident)
            }
            _ => panic!("expected identifier expression but got {}", exp),
        }
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
