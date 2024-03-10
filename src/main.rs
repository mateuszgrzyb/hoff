#![feature(impl_trait_in_assoc_type)]

use clap::Parser;
use rayon::ThreadPoolBuilder;

use crate::{compile::Compile, library::cli::Args, repl::Repl};

mod compile;
mod library;
mod repl;

fn main() {
  let args: Args = Args::parse();

  if args.threads > 0 {
    ThreadPoolBuilder::new()
      .num_threads(args.threads)
      .build_global()
      .unwrap()
  }

  if args.repl {
    Repl::create(args).run_loop()
  } else {
    let result = Compile::create(args).compile();

    match result {
      Ok(_) => {}
      Err(e) => {
        println!("ERROR: {:?}", e)
      }
    }
  }
}
