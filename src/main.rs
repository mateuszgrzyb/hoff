#![allow(clippy::upper_case_acronyms)]
extern crate core;

use clap::Parser;

use crate::{compile::Compile, library::cli::Args, repl::REPL};
use anyhow::Result;

mod compile;
mod library;
mod repl;

fn main() -> Result<()> {
  let args: Args = Args::parse();

  if args.repl {
    REPL::create(args).run_loop()
  } else {
    Compile::create(args).compile()
  }
}
