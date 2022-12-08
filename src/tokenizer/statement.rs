use super::{Function, Variable, AssignVariable, Literal, BinaryOperationStatement};

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
