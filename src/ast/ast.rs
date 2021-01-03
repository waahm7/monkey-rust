use std::fmt::Formatter;
use std::fmt::{Display, Result};
use std::vec::Vec;

use crate::parser::parser::{ParseResult, Parser};
use crate::token::{Token, TokenType};

#[derive(Debug)]
pub enum Node {
    Program(Box<Program>),
    Statement(Box<Statement>),
    Expression(Box<Expression>),
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Expression {
    Identifier(String),
    Integer(i64),
    Prefix(Box<PrefixExpression>),
    Infix(Box<InfixExpression>),
    Boolean(bool),
    If(Box<IfExpression>),
    Fn(Box<FunctionLiteral>),
    Call(Box<CallExpression>),
}

impl Expression {
    pub fn string(&self) -> String {
        match self {
            Expression::Identifier(s) => s.clone(),
            Expression::Integer(i) => i.to_string(),
            Expression::Prefix(i) => i.to_string(),
            Expression::Infix(i) => i.to_string(),
            Expression::Boolean(i) => i.to_string(),
            Expression::If(i) => i.to_string(),
            Expression::Fn(i) => i.to_string(),
            Expression::Call(i) => i.to_string(),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.string())
    }
}

#[derive(Debug)]
pub struct CallExpression {
    pub function: Expression,
    pub arguments: Vec<Expression>,
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let arg_list: Vec<String> = (&self.arguments)
            .into_iter()
            .map(|exp| exp.to_string())
            .collect();
        write!(f, "{}({})", self.function.to_string(), arg_list.join(", "))
    }
}

#[derive(Debug)]
pub struct FunctionLiteral {
    pub parameters: Vec<IdentifierExpression>,
    pub body: BlockStatement,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let param_list: Vec<String> = (&self.parameters)
            .into_iter()
            .map(|p| p.to_string())
            .collect();
        write!(f, "({}) {}", param_list.join(", "), self.body)
    }
}

#[derive(Debug)]
pub struct IfExpression {
    pub condition: Expression,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "if {} {}", self.condition, self.consequence)?;
        if let Some(alt) = &self.alternative {
            write!(f, "else {}", alt)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for statement in self.statements.iter() {
            write!(f, "{}", statement)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct InfixExpression {
    pub left: Expression,
    pub operator: String,
    pub right: Expression,
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub operator: String,
    pub right: Expression,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.expression)
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: IdentifierExpression,
    pub value: Expression,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} ", self.token.literal)?;
        write!(f, "{}", self.name.value)?;
        write!(f, " = ")?;
        write!(f, "{}", self.value)?;
        write!(f, ";")?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
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

#[derive(Debug)]
pub struct IdentifierExpression {
    pub token: Token,
    pub value: String,
}

impl Display for IdentifierExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Statement::Let(Box::new(LetStatement {
                token: Token {
                    type_token: TokenType::LET,
                    literal: "let".to_string(),
                },
                name: IdentifierExpression {
                    token: Token {
                        type_token: TokenType::IDENT,
                        literal: "myVar".to_string(),
                    },
                    value: "myVar".to_string(),
                },
                value: Expression::Identifier("anotherVar".to_string()),
            }))],
        };

        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }
}
