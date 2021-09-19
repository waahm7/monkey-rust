use std::ops::Deref;

use crate::ast::ast::{Expression, InfixExpression, Node, PrefixExpression, Program, Statement};
use crate::evaluator::object::Object;
use crate::lexer::Lexer;

pub fn eval(node: &Node) -> Option<Object> {
    return match node {
        Node::Program(i) => Some(eval_program(i.deref())),
        Node::Statement(i) => None,
        Node::Expression(_) => None,
        _ => None,
    };
}

fn eval_statement(statement: &Statement) -> Object {
    match statement {
        Statement::Let(i) => {}
        Statement::Return(i) => {}
        Statement::Expression(i) => return eval_expression(&i.deref().expression),
    }

    Object::Null
}

fn eval_expression(exp: &Expression) -> Object {
    match exp {
        Expression::Identifier(_) => {}
        Expression::Integer(i) => return Object::Integer(*i),
        Expression::Prefix(i) => return eval_prefix_fn(i.deref()),
        Expression::Infix(i) => return eval_infix_fn(i.deref()),
        Expression::Boolean(i) => return Object::Boolean(*i),
        Expression::If(_) => {}
        Expression::Fn(_) => {}
        Expression::Call(_) => {}
    }

    Object::Null
}

fn eval_infix_fn(infix_expression: &InfixExpression) -> Object {
    let left = eval_expression(&infix_expression.left);
    let right = eval_expression(&infix_expression.right);

    return match (left, right) {
        (Object::Integer(i), Object::Integer(j)) => {
            eval_integer_infix_expression(infix_expression.operator.to_string(), i, j)
        }
        (l, r) => match infix_expression.operator.as_ref() {
            "==" => Object::Boolean(l.inspect() == r.inspect()),
            "!=" => Object::Boolean(l.inspect() != r.inspect()),
            _ => Object::Null,
        }
    }

}


fn eval_integer_infix_expression(operator: String, left: i64, right: i64) -> Object {
    return match operator.as_ref() {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => Object::Boolean(left < right),
        ">" => Object::Boolean(left > right),
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => Object::Null,
    };
}

fn eval_prefix_fn(prefix_expression: &PrefixExpression) -> Object {
    return match prefix_expression.operator.as_ref() {
        "!" => eval_bang_operator_expression(eval_expression(&prefix_expression.right)),
        "-" => eval_minus_prefix_operator_expression(eval_expression(&prefix_expression.right)),
        _ => Object::Null,
    };
}

fn eval_minus_prefix_operator_expression(object: Object) -> Object {
    return match object {
        Object::Integer(i) => Object::Integer(-i),
        _ => Object::Null,
    };
}

fn eval_bang_operator_expression(object: Object) -> Object {
    return match object {
        Object::Boolean(i) => Object::Boolean(!i),
        Object::Integer(_) => Object::Boolean(false),
        _ => Object::Null,
    };
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
    use crate::ast::ast::Program;
    use crate::parser::parser::Parser;

    use super::*;

    #[test]
    fn test_integer_expression() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
        for (actual, expected) in tests {
            let evaluate = test_eval(actual.to_string());
            test_integer_object(evaluate, expected);
        }
    }

    #[test]
    fn test_boolean_expression() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];
        for (actual, expected) in tests {
            let evaluate = test_eval(actual.to_string());
            test_boolean_object(evaluate, expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];
        for (actual, expected) in tests {
            let evaluate = test_eval(actual.to_string());
            test_boolean_object(evaluate, expected);
        }
    }

    fn test_eval(input: String) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let mut program = parser.parse_program();
        return eval(&Node::Program(Box::new(program))).unwrap();
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

    fn test_boolean_object(obj: Object, expected: bool) {
        let actual = match obj {
            Object::Boolean(i) => i,
            _ => {
                panic!(format!("{} is not an boolean.", obj))
            }
        };

        assert_eq!(actual, expected);
    }
}
