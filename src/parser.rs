use std::fs;
use std::io;

use crate::tokenizer::Function;
use crate::tokenizer::Tokenizer;
use crate::tokenizer::Program;
use crate::tokenizer::Token;
use crate::tokenizer::Paramater;
use crate::types::Type;

pub struct Parser<'a> {
  path: String,
  content: String,

  tokenizer: Tokenizer<'a>,
  seek: Option<Token>,
}

// macro_rules! expect_token {
//   ($PATTERN:pat) => {
//     match &mut self.seek {
//       None => panic!("Expected a token!"),
//       Some($PATTERN) => self.seek.take().expect("Exists"),
//       _ => panic!("Unexpected a token!"),
//     }
//   }
// }

macro_rules! expect_token {
  ($PATTERN:pat, $self: expr) => {
    match &mut $self.seek {
      None => panic!("Expected a token!"),
      Some($PATTERN) => {
        let token = $self.seek.take().expect("Exists");
        $self.seek = $self.tokenizer.get_next_token();
        token
      },
      _ => panic!("Unexpected a token!"),
    }
  }
}

impl<'a> Parser<'a> {
  pub fn new(path: &str) -> io::Result<Parser> {
    let mut content = fs::read_to_string(path);
    match content {
      Err(error) => io::Result::Err(error),
      Ok(content) => io::Result::Ok(Parser {
        path: path.to_string(),
        content: content,
        tokenizer: Tokenizer::new(),
        seek: None,
      })
    }
  }
  
  fn expect_type(&mut self) -> Token {
    let something = expect_token!(Token::Type(_), self);

    something
    // if let Token::Type(t) = something {
    //   return t
    // }
    // panic!("Unreachable!");
  }

  pub fn parse(&'a mut self) -> Program {
    let mut program = Program::new();

    
    self.tokenizer.init(&mut self.content);
    self.seek = self.tokenizer.get_next_token();
    
    // self.expect_type();
    loop {
      let token = match &self.seek {
        Some(_) => self.seek.take().expect("Checked it exists before!"),
        None => break,
      };
      self.seek = self.tokenizer.get_next_token();

      match token {
        Token::Identifier(_) => {
          let mut paramaters: Vec<Paramater> = Vec::new();
          let mut body: Vec<Token> = Vec::new();
          expect_token!(Token::ParenthesesOpen, self);
          while false {
            paramaters.push(Paramater(Type::Bool, "".to_string()));
            body.push(Token::Assign);
          }
          expect_token!(Token::ParenthesesClose, self);
          expect_token!(Token::AssignType, self);
          
          let idfk = expect_token!(Token::Type(_), self);

          // self.expect_type();
          let return_type = Type::Bool;

          let function = Function{
            paramaters: paramaters,
            return_type: return_type,
            body: body,
          };
          // println!("something: {:?}", something);
          self.seek = self.tokenizer.get_next_token();
        }
        _ => (),
      }

      program.body.push(token);
    }

    program
  }
}

