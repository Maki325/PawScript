use super::Statement;

#[derive(Debug)]
pub struct AssignVariable {
  pub name: String,
  pub to: Box<Statement>,
}
