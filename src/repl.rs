use std::{
  error::Error,
  io::{
    stdin,
    stdout,
    Write,
  },
};

use inkwell::context::Context;

use crate::library::{
  ast::untyped::Repl,
  backend::REPL,
  cli::Args,
  parser::{
    parse_decl,
    parse_expr,
  },
  qualify::TypedGlobalDecls,
};

fn parse_repl_line(input: String) -> Result<Repl, Box<dyn Error>> {
  if let Ok(decl) = parse_decl(&input) {
    return Ok(Repl::Decl(decl));
  }

  if let Ok(expr) = parse_expr(&input) {
    return Ok(Repl::Expr(expr));
  }

  return Err("Parse error".into());
}

fn print_and_flush(text: &str) {
  print!("{}", text);
  stdout()
    .flush()
    .unwrap_or_else(|err| panic!("flush error: {}", err));
}

pub fn repl(args: Args) -> ! {
  let context = Context::create();

  let global_decls = TypedGlobalDecls::create();

  'repl: loop {
    print_and_flush(">>>");
    let mut input = String::new();
    while !input.contains(";;") {
      if let Err(err) = stdin().read_line(&mut input) {
        println!("Invalid input: {}", err);
        continue 'repl;
      }
      print_and_flush("...");
    }

    let input = input
      .split(";;")
      .next()
      .unwrap()
      .to_string();

    let Ok(expr) = parse_repl_line(input)
            .map_err(|err| println!("Parse error: {}", err))
        else { continue };

    let mut repl = REPL::create(&global_decls, &context, args.o);

    let Ok(result) = repl.eval(expr)
            .map_err(|err| println!("Eval error: {}", err))
        else { continue };

    println!("{}", result)
  }
}
