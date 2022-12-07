use std::fs;
use std::io;

use crate::tokenizer::Function;
use crate::tokenizer::Tokenizer;
use crate::tokenizer::Program;
use crate::tokenizer::Token;
use crate::tokenizer::Paramater;
use crate::types::Type;

pub struct Parser {
  path: String,
  tokenizer: Tokenizer,
  seek: Option<Token>,
}

macro_rules! expect_token {
  ($PATTERN:pat, $self: expr) => {
    match &mut $self.seek {
      None => panic!("Expected a token!"),
      Some($PATTERN) => {
        let token = $self.seek.take().expect("Exists");
        $self.next_token();
        token
      },
      _ => panic!("Unexpected token!"),
    }
  }
}

impl Parser {
  pub fn new(path: &str) -> io::Result<Parser> {
    let mut content = fs::read_to_string(path);
    match content {
      Err(error) => io::Result::Err(error),
      Ok(content) => io::Result::Ok(Parser {
        path: path.to_string(),
        tokenizer: Tokenizer::new(content),
        seek: None,
      })
    }
  }

  fn next_token(&mut self) {
    self.seek = self.tokenizer.get_next_token();
  }

  fn expect_type(&mut self) -> Type {
    let something = expect_token!(Token::Type(_), self);
    if let Token::Type(t) = something {
      return t;
    }
    unreachable!();
  }

  pub fn parse(&mut self) -> Program {
    let mut program = Program::new();

    self.next_token();
    loop {
      let token = match &self.seek {
        Some(_) => self.seek.take().expect("Checked it exists before!"),
        None => break,
      };
      self.next_token();

      match token {
        Token::Identifier(_) => {
          let mut paramaters: Vec<Paramater> = Vec::new();
          let mut body: Vec<Token> = Vec::new();
          expect_token!(Token::ParenthesesOpen, self);
          loop {
            if let Some(Token::ParenthesesClose) = self.seek {
              self.next_token();
              break;
            }
            let name = match expect_token!(Token::Identifier(_), self) {
              Token::Identifier(string) => string,
              _ => unreachable!(),
            };
            let typee = match expect_token!(Token::Type(_), self) {
              Token::Type(typee) => typee,
              _ => unreachable!(),
            };
            paramaters.push(Paramater(name, typee));
            match self.seek {
              Some(Token::Comma) => self.next_token(),
              Some(Token::ParenthesesClose) => (),
              None => panic!("Expected a token!"),
              _ => panic!("Unexpected token!"),
            }
          }
          expect_token!(Token::AssignType, self);

          let idfk = expect_token!(Token::Type(_), self);

          self.expect_type();
          let return_type = Type::Bool;

          let function = Function{
            paramaters: paramaters,
            return_type: return_type,
            body: body,
          };
          self.seek = self.tokenizer.get_next_token();
        }
        _ => (),
      }

      program.body.push(token);
    }

    program
  }
}

