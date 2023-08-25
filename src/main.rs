#![allow(clippy::upper_case_acronyms)]
extern crate core;

use clap::Parser;
use link::Linker;
use rayon::ThreadPoolBuilder;

use crate::{compile::Compile, library::cli::Args, repl::REPL};
use anyhow::{bail, Result};

mod compile;
mod library;
mod link;
mod repl;
mod utils;

fn main() -> Result<()> {
  let args: Args = Args::parse();

  if args.threads > 0 {
    ThreadPoolBuilder::new()
      .num_threads(args.threads)
      .build_global()?
  }

  match (args.repl, args.link) {
    (true, true) => {
      bail!("Both repl mode and link is set to true, aborting...")
    }
    (true, false) => REPL::create(args).run_loop(),
    (false, true) => Linker::create(args).link(),
    (false, false) => Compile::create(args).compile(),
  }
}
