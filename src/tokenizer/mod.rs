macro_rules! export {
    ($a:ident) => {
      mod $a;
      pub use $a::*;
    };
}

mod specification;
export!(tokenizer);
export!(binary_operation);
export!(variable);
export!(token);
export!(statement);
export!(assign_variable);
export!(binary_operation_statement);
export!(function);
export!(literal);
export!(program);
