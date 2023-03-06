extern crate core;

use std::error::Error;

use clap::Parser;

use crate::{compile::Compile, library::cli::Args, repl::REPL};

mod compile;
mod library;
mod repl;

fn main() -> Result<(), Box<dyn Error>> {
  let args: Args = Args::parse();

  if args.repl {
    REPL::create(args).run_loop()
  } else {
    Compile::create(args).compile()
  }
}
