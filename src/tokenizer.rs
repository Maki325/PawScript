use regex::Regex;
use lazy_static::lazy_static;
use crate::types::Type;

#[derive(Debug)]
pub enum BinaryOperation {
  Add,
  Substract,
  Equal,
  NotEqual
}

#[derive(Debug)]
pub struct Variable {
  pub name: String,
  pub typee: Option<Type>,
  pub mutable: bool,
}

#[derive(Debug)]
pub struct AssignVariable {
  pub name: String,
  pub to: Box<Statement>,
}

#[derive(Debug)]
pub struct Function {
  pub name: String,
  pub paramaters: Vec<Variable>,
  pub return_type: Type,
  pub body: Vec<Statement>,
}

#[derive(Debug)]
pub enum Literal {
  NumberLiteral(u64),
}

#[derive(Debug)]
pub enum Token {
  Space,
  Comment(String),
  Comma,
  Semicolon,
  Identifier(String),
  Literal(Literal),
  ParenthesesOpen,
  ParenthesesClose,
  BracesOpen,
  BracesClose,
  BracketsOpen,
  BracketsClose,
  Mut,
  Const,
  Type(Type),
  AssignType,
  Assign,
  BinaryOperation(BinaryOperation),
  Return,
}

#[derive(Debug)]
pub struct BinaryOperationStatement {
  pub left: Box<Statement>,
  pub right: Box<Statement>,
  pub operation: BinaryOperation
}

#[derive(Debug)]
pub enum Statement {
  DeclareFunction(Function),
  DeclareVariable(Variable),
  AssignVariable(AssignVariable),
  DeclareAndAssignVariable(Variable, AssignVariable),
  Comment(String),
  Literal(Literal),
  BinaryOperation(BinaryOperationStatement),
  Return(Box<Statement>)
}

#[derive(Debug)]
pub struct Program {
  body: Vec<Statement>,
}
impl Program {
  pub fn new() -> Program {
    Program {body: Vec::new()}
  }
  pub fn push(&mut self, statement: Statement) {
    self.body.push(statement);
  }
}

pub struct Tokenizer {
  content: String,
  row: usize,
  column: usize,
}

#[derive(Debug)]
struct Spec {
  regex: Regex,
  create: fn(String) -> Token,
}

lazy_static! {
  static ref SPECS: [Spec; 17] = [
    Spec {regex: Regex::new(r#"\s+"#).unwrap(), create: |_| Token::Space},
    Spec {regex: Regex::new(r#"//.*"#).unwrap(), create: |s| Token::Comment(s)},
    Spec {regex: Regex::new(r#"\d+"#).unwrap(), create: |s: String| Token::Literal(Literal::NumberLiteral(s.parse::<u64>().unwrap()))},
    Spec {regex: Regex::new(r#"\("#).unwrap(), create: |_| Token::ParenthesesOpen},
    Spec {regex: Regex::new(r#"\)"#).unwrap(), create: |_| Token::ParenthesesClose},
    Spec {regex: Regex::new(r#"\{"#).unwrap(), create: |_| Token::BracesOpen},
    Spec {regex: Regex::new(r#"\}"#).unwrap(), create: |_| Token::BracesClose},
    Spec {regex: Regex::new(r#"\["#).unwrap(), create: |_| Token::BracketsOpen},
    Spec {regex: Regex::new(r#"\]"#).unwrap(), create: |_| Token::BracketsClose},
    Spec {regex: Regex::new(r#"u64"#).unwrap(), create: |_| Token::Type(Type::U64)},
    Spec {regex: Regex::new(r#"bool"#).unwrap(), create: |_| Token::Type(Type::Bool)},
    Spec {regex: Regex::new(r#":"#).unwrap(), create: |_| Token::AssignType},
    Spec {regex: Regex::new(r#"="#).unwrap(), create: |_| Token::Assign},
    Spec {regex: Regex::new(r#"mut"#).unwrap(), create: |_| Token::Mut},
    Spec {regex: Regex::new(r#"const"#).unwrap(), create: |_| Token::Const},
    Spec {regex: Regex::new(r#";"#).unwrap(), create: |_| Token::Semicolon},
    Spec {regex: Regex::new(r#"return"#).unwrap(), create: |_| Token::Return},
  ];
}

impl Tokenizer {
  pub fn new(content: String) -> Tokenizer {
    Tokenizer {
      content: content,
      row: 0,
      column: 0,
    }
  }

  fn get_next_token_from_spec(&mut self, end: usize) -> Option<Token> {
    let text = match end {
      0 => &self.content[self.column..],
      _ => &self.content[self.column..end],
    };
    for spec in SPECS.as_ref() {
      for capture in spec.regex.captures_iter(text) {
        let m = capture.get(0).expect("Match should exist");
        let start = m.start();
        if start != 0 { continue; }
  
        let value = m.as_str().to_string();
        let len = value.len();
  
        self.column += len;
  
        let token = (spec.create)(value);

        if let Token::Comment(_) = token {
          self.column -= len;
          let nl = self.content[self.column..].find("\n");
          match nl {
            None => {
              let string = self.content[self.column..].to_string();
              self.column += string.len();

              return Some(Token::Comment(string));
            },
            Some(end) => {
              let string = self.content[self.column..self.column + end].to_string();
              self.column += string.len();

              return Some(Token::Comment(string));
            },
          }
        }

        return match token {
          Token::Space => self.get_next_token(),
          _ => Some(token),
        }
      }
    }
  
    None
  }

  fn get_next_identifier_token_from_spec(&mut self, end: usize) -> Option<Token> {
    let text = match end {
      0 => &self.content[self.column..],
      _ => &self.content[self.column..end],
    };
    let mut smallest_start: Option<usize> = None;
    for spec in SPECS.as_ref() {
      for capture in spec.regex.captures_iter(text) {
        let m = capture.get(0).expect("Match should exist");
        let start = m.start();
        if start == 0 { unreachable!("Shoul not happen, as we already called a function to check all the tokens from the start!"); }

        if let None = smallest_start {
          smallest_start = Some(start);
          continue;
        }
        if let Some(smallStart) = smallest_start {
          if start < smallStart {
            smallest_start = Some(start);
            continue;
          }
        }
      }
    }

    return match smallest_start {
      None => None,
      Some(start) => {
        self.column += start;

        Some(Token::Identifier(text[0..start].to_string()))
      }
    };
  }

  pub fn get_next_token(&mut self) -> Option<Token> {
    if self.column >= self.content.len() - 1 {
      return None;
    }

    let end = match self.content[self.column..].find(" ") {
      Some(0) => 0,
      Some(end) => self.column + end,
      None => 0,
    };

    let mut token = self.get_next_token_from_spec(end);
    if let Some(_) = token {
      return token;
    }
    token = self.get_next_identifier_token_from_spec(end);
    if let Some(_) = token {
      return token;
    }

    let text = match end {
      0 => &self.content[self.column..],
      _ => &self.content[self.column..end],
    };
    self.column += text.len();
    return Some(Token::Identifier(text.to_string()));
  }
}
