use std::{collections::HashMap, process::exit, time::{SystemTime, UNIX_EPOCH}};

use crate::{tokenizer::{Program, Variable, Statement, Function, AssignVariable}, types::Type};

type Variables = HashMap<String, Variable>;

impl Program {
  fn typecheck_declare_variable_statement(variable: &mut Variable, variables: &mut Variables, assign_after: bool) {
    if variables.contains_key(&variable.name) {
      println!("Variable \"{}\" re-declaration!", variable.name);
      exit(-1);
    }
    if variable.mutable == false && !assign_after {
      println!("Const variable \"{}\" needs to have a value!", variable.name);
      exit(-1);
    }

    let time = match SystemTime::now().duration_since(UNIX_EPOCH) {
      Ok(duration) => duration,
      Err(error) => panic!("{}", error),
    };

    let mut compiled_name = String::from("var_");
    compiled_name.push_str(time.as_micros().to_string().as_str());

    variable.compiled_name = Some(compiled_name);

    variables.insert(variable.name.clone(), variable.clone());
  }

  fn typecheck_assign_variable_statement<'a>(assignVariable: &mut AssignVariable, variables: &'a mut Variables, function_return_type: &Option<&Type>) {
    Program::typecheck_statement(&mut assignVariable.to,  variables, function_return_type);

    let get_mut_type = |variables: &'a mut Variables| -> &mut Option<Type> {
      match variables.get_mut(&assignVariable.name) {
        None => {
          println!("Variable \"{}\" never declared!", assignVariable.name);
          exit(-1);
        },
        Some(var) => return &mut var.typee,
      };
    };

    match &mut *assignVariable.to.as_mut() {
      Statement::Literal(literal) => {
        match &literal {
          crate::tokenizer::Literal::NumberLiteral(_) => {
            let var = get_mut_type(variables);
            match &var {
              None => *var = Some(Type::U64),
              Some(typee) => {
                if *typee != Type::U64 {
                  println!("Not the same type!");
                  exit(-1);
                }
              }
            }
          }
        }
      },
      Statement::Variable(name) => {
        let typee = match variables.get(name) {
          None => {
            println!("Variable \"{}\" doesn't exist!", name);
            exit(-1);
          },
          Some(var) => {
            match var.typee.clone() {
              None => {
                println!("Variable \"{}\" doesn't have a type!", name);
                exit(-1);
              },
              Some(typee) => typee,
            }
          },
        };
        let variable_type = get_mut_type(variables);

        match variable_type {
          None => *variable_type = Some(typee),
          Some(var_typee) => {
            if *var_typee != typee {
              println!("Not the same type!");
              exit(-1);
            }
          }
        }
        return
      },
      Statement::BinaryOperation(operation) => {
        let operation_typee = match &operation.typee {
          None => unreachable!(),
          Some(typee) => typee.clone(),
        };
        let variable_type = get_mut_type(variables);
        
        match &variable_type {
          None => *variable_type = Some(operation_typee),
          Some(typee) => {
            if *typee != operation_typee {
              println!("Not the same type!");
              exit(-1);
            }
          }
        }
      }
      _ => {
        println!("Can't assign {:?} to variable!", &*assignVariable.to);
        exit(-1);
      },
    }
  }

  fn get_statement_type(statement: &Statement, variables: &Variables) -> Type {
    match statement {
      Statement::Variable(name) => {
        match variables.get(name) {
          None => {
            println!("Variable \"{}\" doesn't exist!", name);
            exit(-1);
          },
          Some(variable) => {
            match &variable.typee {
              None => {
                println!("Variable \"{}\" doesn't have a type!", name);
                exit(-1);
              },
              Some(typee) => return typee.clone(),
            }
          },
        }
      }
      Statement::BinaryOperation(operaition) => {
        match &operaition.typee {
          None => {
            println!("Operation \"{:?}\" doesn't have a type!", operaition.operation);
            exit(-1);
          },
          Some(typee) => return typee.clone(),
        }
      },
      Statement::Literal(literal) => {
        return match literal {
          crate::tokenizer::Literal::NumberLiteral(_) => Type::U64,
        }
      }
      Statement::Comment(_) |
      Statement::DeclareFunction(_) |
      Statement::DeclareVariable(_) |
      Statement::AssignVariable(_) |
      Statement::DeclareAndAssignVariable(..) |
      Statement::Return(_) => unreachable!(),
    }
  }

  fn typecheck_statement(statement: &mut Statement, variables: &mut Variables, function_return_type: &Option<&Type>) {
    
    match statement {
      Statement::DeclareFunction(function) =>
        Program::typecheck_function(function),
      Statement::Variable(variable) => {
        if !variables.contains_key(variable) {
          println!("Variable \"{}\" doesn't exist!", variable);
        }
      }
      Statement::DeclareVariable(variable) => 
        Program::typecheck_declare_variable_statement(variable, variables, false),
      Statement::AssignVariable(assignVariable) => 
        Program::typecheck_assign_variable_statement(assignVariable, variables, function_return_type),
      Statement::DeclareAndAssignVariable(variable, assignVariable) => {
        Program::typecheck_declare_variable_statement(variable, variables, true);
        Program::typecheck_assign_variable_statement(assignVariable, variables, function_return_type);
      },
      Statement::BinaryOperation(operation) => {
        Program::typecheck_statement(&mut *operation.left, variables, function_return_type);
        Program::typecheck_statement(&mut *operation.right, variables, function_return_type);

        let left = Program::get_statement_type(&*operation.left, variables);
        let right = Program::get_statement_type(&*operation.right, variables);
        if left != right {
          println!("Operation types don't match!");
          exit(-1);
        }
        operation.typee = Some(left);
      },
      Statement::Return(value) => {
        Program::typecheck_statement(&mut *value, variables, function_return_type);
        let return_type = Program::get_statement_type(&mut *value, variables);

        match function_return_type {
          None => {
            println!("Can't have a return when not in a function!");
            exit(-1);
          },
          Some(function_return_type) => {
            if return_type != **function_return_type {
              println!("Return doesn't return the correct type!");
              exit(-1);
            }
          }
        }
      },
      Statement::Comment(_) | Statement::Literal(_) => (),
    }
  }

  fn typecheck_function(function: &mut Function) {
    let mut variables: Variables = HashMap::new();

    for statement in &mut function.body {
      println!("B statement: {:?}", statement);
      Program::typecheck_statement(statement, &mut variables, &Some(&function.return_type));
      println!("A statement: {:?}", statement);
    }
  }

  pub fn typecheck(&mut self) {
    for statement in &mut self.body {
      match statement {
        Statement::DeclareFunction(function) => Program::typecheck_function(function),
        _ => unimplemented!("{:?}", statement)
      }
    }
  }
}