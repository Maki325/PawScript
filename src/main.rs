mod tokenizer;
mod parser;
mod types;
mod typechecker;

use parser::Parser;

fn main() {
  let path = "examples/example-0001.ps";

  let mut parser = Parser::new(path).expect("Should be able to read file!");
  let mut program = parser.parse();

  program.typecheck();

  println!("Program: {:?}", program);
}
