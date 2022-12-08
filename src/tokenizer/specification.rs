use lazy_static::lazy_static;
use regex::Regex;
use crate::types::Type;

use super::{BinaryOperation, Literal, Token};

#[derive(Debug)]
pub struct Specification {
  pub regex: Regex,
  pub create: fn(String) -> Token,
}

lazy_static! {
  pub static ref SPECIFICATIONS: [Specification; 22] = [
    Specification {regex: Regex::new(r#"\s+"#).unwrap(), create: |_| Token::Space},
    Specification {regex: Regex::new(r#"//.*"#).unwrap(), create: |s| Token::Comment(s)},
    Specification {regex: Regex::new(r#"\d+"#).unwrap(), create: |s: String| Token::Literal(Literal::NumberLiteral(s.parse::<u64>().unwrap()))},
    Specification {regex: Regex::new(r#"\("#).unwrap(), create: |_| Token::ParenthesesOpen},
    Specification {regex: Regex::new(r#"\)"#).unwrap(), create: |_| Token::ParenthesesClose},
    Specification {regex: Regex::new(r#"\{"#).unwrap(), create: |_| Token::BracesOpen},
    Specification {regex: Regex::new(r#"\}"#).unwrap(), create: |_| Token::BracesClose},
    Specification {regex: Regex::new(r#"\["#).unwrap(), create: |_| Token::BracketsOpen},
    Specification {regex: Regex::new(r#"\]"#).unwrap(), create: |_| Token::BracketsClose},
    Specification {regex: Regex::new(r#"u64"#).unwrap(), create: |_| Token::Type(Type::U64)},
    Specification {regex: Regex::new(r#"bool"#).unwrap(), create: |_| Token::Type(Type::Bool)},
    Specification {regex: Regex::new(r#":"#).unwrap(), create: |_| Token::AssignType},
    Specification {regex: Regex::new(r#"mut"#).unwrap(), create: |_| Token::Mut},
    Specification {regex: Regex::new(r#"const"#).unwrap(), create: |_| Token::Const},
    Specification {regex: Regex::new(r#","#).unwrap(), create: |_| Token::Comma},
    Specification {regex: Regex::new(r#";"#).unwrap(), create: |_| Token::Semicolon},
    Specification {regex: Regex::new(r#"\+"#).unwrap(), create: |_| Token::BinaryOperation(BinaryOperation::Add)},
    Specification {regex: Regex::new(r#"-"#).unwrap(), create: |_| Token::BinaryOperation(BinaryOperation::Subtract)},
    Specification {regex: Regex::new(r#"=="#).unwrap(), create: |_| Token::BinaryOperation(BinaryOperation::Equal)},
    Specification {regex: Regex::new(r#"!="#).unwrap(), create: |_| Token::BinaryOperation(BinaryOperation::NotEqual)},
    Specification {regex: Regex::new(r#"return"#).unwrap(), create: |_| Token::Return},
    Specification {regex: Regex::new(r#"="#).unwrap(), create: |_| Token::Assign},
  ];
}
