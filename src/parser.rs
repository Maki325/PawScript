use std::fs;
use std::io;

use crate::tokenizer::BinaryOperation;
use crate::tokenizer::BinaryOperationStatement;
use crate::tokenizer::Function;
use crate::tokenizer::Literal;
use crate::tokenizer::Statement;
use crate::tokenizer::Tokenizer;
use crate::tokenizer::Program;
use crate::tokenizer::Token;
use crate::tokenizer::Variable;
use crate::tokenizer::AssignVariable;
use crate::types::Type;

pub struct Parser {
  path: String,
  tokenizer: Tokenizer,
  seek: Option<Token>,
}

macro_rules! token_pattern2 {
  ($self: expr, $PATTERN:pat) => {
    Some($PATTERN) => {
      let token = $self.seek.take().expect("Exists");
      $self.next_token();
      token
    },
  };
  ($self: expr, $($PATTERN:pat), +) => {
    Some($PATTERN) => {
      let token = $self.seek.take().expect("Exists");
      $self.next_token();
      token
    },
    token_pattern!(+)
  };
}

macro_rules! token_pattern {
  ($self: expr, $PATTERN:pat) => {
    $PATTERN
  };
  ($self: expr, $PATTERN:pat, $($OTHER:pat), +) => {
    $PATTERN | token_pattern!($self, $($OTHER), +)
  };
}

macro_rules! expect_token {
  // ($self: expr, $($PATTERN:pat), +) => {
  //   match &mut $self.seek {
  //     None => panic!("Expected a token!"),
  //     token_pattern!($self, $($PATTERN), +)
  //     // Some($PATTERN) => {
  //     //   let token = $self.seek.take().expect("Exists");
  //     //   $self.next_token();
  //     //   token
  //     // },
  //     // +,
  //     _ => panic!("Unexpected token!"),
  //   }
  // }
  ($self: expr, $($PATTERN:pat), +) => {
    match &mut $self.seek {
      None => panic!("Expected a token!"),
      Some(token_pattern!($self, $($PATTERN), +)) => {
        let token = $self.seek.take().expect("Exists");
        $self.next_token();
        token
      },
      // Some($PATTERN) => {
      //   let token = $self.seek.take().expect("Exists");
      //   $self.next_token();
      //   token
      // },
      // +,
      _ => panic!("Unexpected token!, Got: ${:?}", $self.seek),
    }
  }
}

impl Parser {
  pub fn new(path: &str) -> io::Result<Parser> {
    let content = fs::read_to_string(path);
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
    // println!("next R: {:?}", self.seek);
  }

  fn expect_type(&mut self) -> Type {
    let something = expect_token!(self, Token::Type(_));
    if let Token::Type(t) = something {
      return t;
    }
    unreachable!();
  }

  pub fn parse(&mut self) -> Program {
    let mut program = Program::new();
    self.next_token();

    loop {
      match self.parse_statement() {
        Some(statement) => program.push(statement),
        None => break,
      }
    }

    program
  }

  pub fn parse_binary_operation(&mut self, left: Literal, operation: BinaryOperation) -> Statement {
    let token = expect_token!(self, Token::Literal(Literal::NumberLiteral(_)));
    let right = match token {
      Token::Literal(literal) => literal,
      _ => unreachable!(),
    };

    let token = expect_token!(self, Token::BinaryOperation(_), Token::Semicolon);
    match token {
      Token::Semicolon => {
        return Statement::BinaryOperation(BinaryOperationStatement {
          left: Box::new(Statement::Literal(left)),
          right: Box::new(Statement::Literal(right)),
          operation: operation,
        })
      },
      Token::BinaryOperation(right_operation) => {
        return Statement::BinaryOperation(BinaryOperationStatement {
          left: Box::new(Statement::Literal(left)),
          right: Box::new(self.parse_binary_operation(right, right_operation)),
          operation: operation,
        })
      },
      _ => unreachable!(),
    }
  }

  pub fn parse_number_literal(&mut self, literal: Literal) -> Statement {
    let token = expect_token!(self, Token::BinaryOperation(_), Token::Semicolon);
    return match token {
      Token::BinaryOperation(operation) => self.parse_binary_operation(literal, operation),
      Token::Semicolon => Statement::Literal(literal),
      _ => unreachable!()
    }
  }

  pub fn parse_literal(&mut self, literal: Literal) -> Statement {
    match literal {
      Literal::NumberLiteral(_) => self.parse_number_literal(literal),
      _ => unimplemented!("Not implemented!"),
    }
  }

  pub fn parse_function_declaration(&mut self, name: String) -> Statement {
    let mut paramaters: Vec<Variable> = Vec::new();
    let mut body: Vec<Statement> = Vec::new();
    
    loop {
      if let Some(Token::ParenthesesClose) = self.seek {
        self.next_token();
        break;
      }
      let name = match expect_token!(self, Token::Identifier(_)) {
        Token::Identifier(string) => string,
        _ => unreachable!(),
      };
      let typee = match expect_token!(self, Token::Type(_)) {
        Token::Type(typee) => typee,
        _ => unreachable!(),
      };
      paramaters.push(Variable{name, typee: Some(typee), mutable: false});
      match self.seek {
        Some(Token::Comma) => self.next_token(),
        Some(Token::ParenthesesClose) => (),
        None => panic!("Expected a token!"),
        _ => panic!("Unexpected token!"),
      }
    }
    expect_token!(self, Token::AssignType);

    let return_type = self.expect_type();

    expect_token!(self, Token::BracesOpen);

    loop {
      if let Some(Token::BracesClose) = self.seek {
        self.next_token();
        break;
      }
      match self.parse_statement() {
        None => panic!("Expected function body!"),
        Some(statement) => body.push(statement),
      }
    }

    let function = Function{
      name,
      paramaters,
      return_type,
      body,
    };

    return Statement::DeclareFunction(function);
  }

  pub fn parse_assign_variable(&mut self, name: String) -> Statement {
    return Statement::AssignVariable(
      AssignVariable { name, to: Box::new(self.parse_statement().expect("Expected variable assignment!")) }
    );
  }

  pub fn parse_identifier(&mut self, name: String) -> Statement {
    let token = expect_token!(self, Token::ParenthesesOpen, Token::Assign);

    match token {
      Token::ParenthesesOpen => return self.parse_function_declaration(name),
      Token::Assign => return self.parse_assign_variable(name),
      _ => unreachable!(),
    }
  }

  fn parse_return(&mut self) -> Statement {
    match self.parse_statement() {
      Some(statement) => Statement::Return(Box::new(statement)),
      None => panic!("Expected statement!"),
    }
  }

  pub fn parse_statement(&mut self) -> Option<Statement> {
    let token = match &self.seek {
      Some(_) => self.seek.take().expect("Checked it exists in match!"),
      None => return None,
    };
    self.next_token();

    match token {
      Token::Literal(literal) => return Some(self.parse_literal(literal)),
      Token::Comment(comment) => return Some(Statement::Comment(comment)),
      Token::Identifier(name) => return Some(self.parse_identifier(name)),
      Token::Return => return Some(self.parse_return()),
      Token::Mut | Token::Const => {
        let mutable = match token {
          Token::Mut => true,
          Token::Const => false,
          _ => unreachable!()
        };
        let identifier = expect_token!(self, Token::Identifier(_));
        let name = match identifier {
          Token::Identifier(name) => name,
          _ => unreachable!(),
        };
        let mut typee: Option<Type> = None;

        let token = expect_token!(self, Token::Assign, Token::AssignType, Token::Semicolon);
        match token {
          Token::Assign => {
            return Some(Statement::DeclareAndAssignVariable(
              Variable {name: name.clone(), typee, mutable},
              AssignVariable { name, to: Box::new(self.parse_statement().expect("Expected variable assignment!")) }
            ));
          },
          Token::AssignType => {
            typee = Some(self.expect_type());
            let token = expect_token!(self, Token::Assign, Token::Semicolon);

            match token {
              Token::Assign => {
                return Some(Statement::DeclareAndAssignVariable(
                  Variable {name: name.clone(), typee, mutable},
                  AssignVariable { name, to: Box::new(self.parse_statement().expect("Expected variable assignment!")) }
                ));
              },
              Token::Semicolon => {
                if let Token::Const = token {
                  panic!("Const variables must have value at declaration!");
                }
                return Some(Statement::DeclareVariable(
                  Variable {name: name.clone(), typee, mutable}
                ));
              },
              _ => unreachable!(),
            }
          },
          Token::Semicolon => {
            if let Token::Const = token {
              panic!("Const variables must have value at declaration!");
            }
            return Some(Statement::DeclareVariable(
              Variable {name: name.clone(), typee, mutable}
            ));
          },
          _ => unreachable!(),
        }
      },
      _ => (),
    }

    println!("Unreachable: {:?}", token);
    unreachable!()
  }
}

