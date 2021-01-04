use crate::ast::ast::{Expression, Statement, Node, Program};
use crate::evaluator::object::Object;
use crate::lexer::Lexer;
use std::ops::Deref;

fn eval(node: &Node) -> Object {
    match node {
        Node::Program(i ) => { return eval_program(i.deref()); }
        Node::Statement(i) => {}
        Node::Expression(_) => {}
    }
    return Object::Null;
}

fn eval_statement(statement: &Statement) -> Object {
    match statement {
        Statement::Let(i) => {}
        Statement::Return(i) => {}
        Statement::Expression(i) => {return eval_expression(&i.deref().expression)},
    }

    Object::Null
}

fn eval_expression(exp: &Expression) -> Object {
    match exp {
        Expression::Identifier(_) => {}
        Expression::Integer(i) => {
            return Object::Integer(*i);
        }
        Expression::Prefix(_) => {}
        Expression::Infix(_) => {}
        Expression::Boolean(_) => {}
        Expression::If(_) => {}
        Expression::Fn(_) => {}
        Expression::Call(_) => {}
    }

    Object::Null
}

fn eval_program(program: &Program) -> Object {
    let mut result: Object = Object::Null;

    for statement in &program.statements {
        result = eval_statement(statement);
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::ast::Program;
    use crate::parser::parser::Parser;

    #[test]
    fn test_integer_expression() {
        let tests = vec![("5", 5), ("10", 10)];
        for (actual, expected) in tests {
            let evaluate = test_eval(actual.to_string());
            test_integer_object(evaluate, expected);
        }
    }

    fn test_eval(input: String) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let mut program = parser.parse_program();
        return eval(&Node::Program(Box::new(program)));
    }

    fn test_integer_object(obj: Object, expected: i64) {
        let integer = match obj {
            Object::Integer(i) => i,
            _ => {
                panic!(format!("{} is not an integer.", obj))
            }
        };

        assert_eq!(integer, expected);
    }
}
