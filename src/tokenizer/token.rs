use crate::types::Type;

use super::{Literal, BinaryOperation};

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
