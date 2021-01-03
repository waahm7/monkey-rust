mod ast;
mod evaluator;
mod lexer;
mod parser;
mod repl;
mod token;

fn main() {
    println!("Hello, world!");
    repl::start();
}
