use std::fmt::{Display, Result};
use std::fmt::Formatter;
use std::vec::Vec;

use crate::parser::parser::{Parser, ParseResult};
use crate::token::{Token, TokenType};

// pub enum Node {
//     Program(Box<Program>),
//     Statement(Box<Statement>),
//     Expression(Box<Expression>),
// }

// impl Node {}
//
pub enum Statement {
    Let(Box<LetStatement>),
    Return(Box<ReturnStatement>),
    Expression(Box<ExpressionStatement>),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let s = match self {
            Statement::Let(stmt) => format!("{}", stmt),
            Statement::Return(ret) => format!("{}", ret),
            Statement::Expression(exp) => format!("{}", exp),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Expression {
    Identifier(String),
    Integer(i64),
}

impl Expression {
    pub fn string(&self) -> String {
        match self {
            Expression::Identifier(s) => s.clone(),
            Expression::Integer(i) => i.to_string(),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.string())
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.expression)
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}


impl Program {
    pub fn new() -> Program {
        Program {
            statements: Vec::new(),
        }
    }

    pub fn parse_program(&mut self, parser: &mut Parser) {
        self.statements = Vec::new();
        while parser.cur_token.type_token != TokenType::EOF {
            let statement: ParseResult<Statement> = parser.parse_statement();
            match statement {
                Err(_) => {}
                Ok(x) => {
                    self.statements.push(x);
                }
            }
            parser.next_token();
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for statement in self.statements.iter() {
            write!(f, "{}", statement)?;
        }
        Ok(())
    }
}


pub struct LetStatement {
    pub token: Token,
    pub name: IdentifierExpression,
    pub value: ExpressionStatement,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} ", self.token.literal)?;
        write!(f, "{}", self.name.value)?;
        write!(f, " = ")?;
        write!(f, "{}", self.value)?; //Todo: why value can be nil page: 50
        write!(f, ";")?;
        Ok(())
    }
}

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: ExpressionStatement,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} ", self.token.literal)?;
        write!(f, "{}", self.return_value)?; //Todo: why value can be nil page: 50
        write!(f, ";")?;
        Ok(())
    }
}


// pub struct ExpressionStatement {
//     pub token: Token,
//     pub expression: Expression,
// }
//
// impl Display for ExpressionStatement {
//     fn fmt(&self, f: &mut Formatter<'_>) -> Result {
//         write!(f, "{}", self.expression)
//     }
// }
// pub struct IntegerLiteral {
//     pub token: Token,
//     pub value: i64,
// }

pub struct IdentifierExpression {
    pub token: Token,
    pub value: String,
}

impl Display for IdentifierExpression {
    fn fmt(&self, _f: &mut Formatter<'_>) -> Result {
        Ok(()) //Todo:
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Statement::Let(Box::new(LetStatement {
                token: Token { type_token: TokenType::LET, literal: "let".to_string() },
                name: IdentifierExpression { token: Token { type_token: TokenType::IDENT, literal: "myVar".to_string() }, value: "myVar".to_string() },
                value: ExpressionStatement{expression: Expression::Identifier("anotherVar".to_string())},
            }))]
        };

       assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }
}
