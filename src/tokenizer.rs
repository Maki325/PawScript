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
pub struct Paramater (pub String, pub Type);

#[derive(Debug)]
pub struct Function {
  pub paramaters: Vec<Paramater>,
  pub return_type: Type,
  pub body: Vec<Token>,
}

#[derive(Debug)]
pub enum Token {
  Space,
  Comment,
  Comma,
  Identifier(String),
  StringLiteral(String),
  NumberLiteral(u64),
  ParenthesesOpen,
  ParenthesesClose,
  Type(Type),
  AssignType,
  Assign,
  BinaryOperation(BinaryOperation),
  Function(Function),
}

#[derive(Debug)]
pub struct Program {
  pub body: Vec<Token>,
}
impl Program {
  pub fn new() -> Program {
    Program {body: Vec::new()}
  }
}

enum Holder {
  Holder(String)
}
pub struct Tokenizer<'a> {
  content: String,
  row: usize,
  column: usize,
}

struct Spec {
  regex: Regex,
  create: fn(String) -> Token,
}

lazy_static! {
  static ref SPECS: [Spec; 10] = [
    Spec {regex: Regex::new(r#"\s+"#).unwrap(), create: |_| Token::Space},
    Spec {regex: Regex::new(r#"//.*"#).unwrap(), create: |_| Token::Comment},
    Spec {regex: Regex::new(r#"\d+"#).unwrap(), create: |s: String| Token::NumberLiteral(s.parse::<u64>().unwrap())},
    Spec {regex: Regex::new(r#""[^"]""#).unwrap(), create: |s: String| Token::StringLiteral(s)},
    Spec {regex: Regex::new(r#"\("#).unwrap(), create: |_| Token::ParenthesesOpen},
    Spec {regex: Regex::new(r#"\)"#).unwrap(), create: |_| Token::ParenthesesClose},
    Spec {regex: Regex::new(r#"u64"#).unwrap(), create: |_| Token::Type(Type::U64)},
    Spec {regex: Regex::new(r#"bool"#).unwrap(), create: |_| Token::Type(Type::Bool)},
    Spec {regex: Regex::new(r#":"#).unwrap(), create: |_| Token::AssignType},
    Spec {regex: Regex::new(r#"="#).unwrap(), create: |_| Token::Assign},
  ];
}

impl<'a> Tokenizer<'a> {
  pub fn new(content: String) -> Tokenizer<'a> {
    Tokenizer {
      content: content,
      row: 0,
      column: 0,
    }
  }

  fn get_next_token_from_spec(&mut self, text: &str, from_start: bool) -> Option<Token> {
    for spec in SPECS.as_ref() {
      for capture in spec.regex.captures_iter(text) {
        let m = capture.get(0).expect("Match should exist");
        let start = m.start();
        if from_start && start != 0 { continue; }

        if start != 0 {
          self.column += start;
          return Some(Token::Identifier(text[0..start].to_string()));
        }

        let value = m.as_str().to_string();

        self.column += value.len();

        let token = (spec.create)(value);

        match token {
          Token::Space => return self.get_next_token(),
          _ => return Some(token),
        }
      }
    }

    None
  }

  fn get_str(self) -> &'a String {
    &self.content[0..]
  }

  pub fn get_next_token(&mut self) -> Option<Token> {
    // let content = match self.content {
    //   Holder::Holder(s) => s,
    // };
    let content = self.get_str();
    if self.column >= content.len() {
      return None;
    }

    let space = content[self.column..].find(" ");
    let text = match space {
      Some(0) => &content[self.column..],
      Some(end) => &content[self.column..self.column + end],
      None => &content[self.column..],
    };

    let mut token = self.get_next_token_from_spec(text, true);
    if let Some(_) = token {
      return token;
    }
    token = self.get_next_token_from_spec(text, false);
    if let Some(_) = token {
      return token;
    }

    self.column += text.len();
    return Some(Token::Identifier(text.to_string()));
  }
}
