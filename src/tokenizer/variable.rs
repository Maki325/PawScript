use crate::types::Type;

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct Variable {
  pub name: String,
  pub compiled_name: Option<String>,
  pub typee: Option<Type>,
  pub mutable: bool,
}
