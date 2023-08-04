use std::{
  io::{stdin, stdout, Write},
  rc::Rc,
};

use anyhow::{bail, Result};
use inkwell::context::Context;

use crate::library::{
  backend::Interpreter, cli::Args, parser::parse_repl,
  qualify::TypedGlobalDecls,
};

pub struct REPL {
  args: Args,
  context: Context,
  global_decls: Rc<TypedGlobalDecls>,
}

impl REPL {
  pub fn create(args: Args) -> Self {
    Self {
      args,
      context: Context::create(),
      global_decls: Rc::new(TypedGlobalDecls::create()),
    }
  }

  pub fn run_loop(&self) -> ! {
    loop {
      let Ok(input) = Self::read_input()
        .map_err(|err| println!("Input error: {}", err))
        else { continue };

      let Ok(expr) = parse_repl(&input)
        .map_err(|err| println!("Parse error: {}", err))
        else { continue };

      let mut interpreter = Interpreter::create(
        self.global_decls.clone(),
        &self.context,
        self.args.o,
      );

      let Ok(result) = interpreter.eval(expr)
        .map_err(|err| println!("Eval error: {}", err))
        else { continue };

      println!("{}", result)
    }
  }

  fn print_and_flush(text: &str) {
    print!("{}", text);
    stdout()
      .flush()
      .unwrap_or_else(|err| panic!("flush error: {}", err));
  }

  fn read_input() -> Result<String> {
    Self::print_and_flush(">>>");

    let mut input = String::new();

    while !input.contains(";;") {
      if let Err(err) = stdin().read_line(&mut input) {
        bail!(err)
      }

      Self::print_and_flush("...");
    }

    let input = input.split(";;").next().unwrap().to_string();

    Ok(input)
  }
}
