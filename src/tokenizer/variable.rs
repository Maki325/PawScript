use crate::types::Type;

#[derive(Debug)]
pub struct Variable {
  pub name: String,
  pub typee: Option<Type>,
  pub mutable: bool,
}
