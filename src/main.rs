extern crate core;

use crate::compile::Compile;
use crate::library::cli::Args;
use crate::repl::repl;
use clap::Parser;
use std::error::Error;

mod compile;
mod library;
mod repl;

fn main() -> Result<(), Box<dyn Error>> {
    let args: Args = Args::parse();

    if args.repl {
        repl(args)
    } else {
        Compile::create(args).compile()
    }
}
