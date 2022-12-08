use super::Statement;

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
