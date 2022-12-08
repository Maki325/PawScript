use crate::types::Type;

use super::{Variable, Statement};

#[derive(Debug)]
pub struct Function {
  pub name: String,
  pub paramaters: Vec<Variable>,
  pub return_type: Type,
  pub body: Vec<Statement>,
}
