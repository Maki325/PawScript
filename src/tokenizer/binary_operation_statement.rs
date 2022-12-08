use super::{Statement, BinaryOperation};

#[derive(Debug)]
pub struct BinaryOperationStatement {
  pub left: Box<Statement>,
  pub right: Box<Statement>,
  pub operation: BinaryOperation
}
