mod tokenizer;
mod parser;
mod types;

use parser::Parser;
use regex::Regex;

fn main() {
  let path = "examples/example-0001.ps";

  let mut parser = Parser::new(path).expect("Should be able to read file!");

  // let regex = Regex::new("\\\\/\\\\/.*").unwrap();
//   let regex = Regex::new(r"(?m)//.*").unwrap();
  
//   let string = "// Comment a b
// fdsfdsfds
// fdsfsdfsd";
  
//   // result will be an iterator over tuples containing the start and end indices for each match in the string
//   let result = regex.captures_iter(string);
  
//   println!("result: {:?}", result);
//   for mat in result {
//     println!("{:?}", mat);
//     println!("{:?}", mat.get(0).expect("A").as_str());
//   }

  let program = parser.parse();

  println!("Program: {:?}", program);
}
