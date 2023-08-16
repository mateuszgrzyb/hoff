use clap::{arg, value_parser, Parser, ValueEnum};

#[derive(ValueEnum, Clone, Debug)]
#[value(rename_all = "lower")]
pub enum RunMode {
  JIT,
  Compile,
}

#[derive(ValueEnum, Clone, Debug)]
#[value(rename_all = "lower")]
pub enum DumpMode {
  None,
  Ast,
  QualifiedAst,
  TypedAst,
  LlvmIr,
}

#[derive(ValueEnum, Clone, Debug)]
#[value(rename_all = "lower")]
pub enum DumpTarget {
  File,
  StdOut,
}

/// Hoff compiler and interpreter.
#[derive(Parser, Clone, Debug)]
#[command(
  author,
  version,
  about,
  long_about = None,
  arg_required_else_help = true,
  verbatim_doc_comment,
)]
pub struct Args {
  #[arg(verbatim_doc_comment)]
  /// Paths to hoff files or directories containing hoff files.
  pub paths: Vec<String>,

  /// File extension used when paths passed as arguments are paths to directories.
  #[arg(short = 'x', long, default_value_t = String::from("hff"), verbatim_doc_comment)]
  pub ext: String,

  /// Choose dump mode.
  #[arg(short = 'd', long, value_enum, default_value_t = DumpMode::None, verbatim_doc_comment)]
  pub dump_mode: DumpMode,

  /// Print LLVM IR to stdout.
  #[arg(short = 't', long, value_enum, default_value_t = DumpTarget::StdOut, verbatim_doc_comment)]
  pub dump_target: DumpTarget,

  /// Set optimization level:
  /// 0 = None,
  /// 1 = Less,
  /// 2 = Default,
  /// 3 = Aggressive.
  #[arg(
    id = "Opt Level",
    short = 'O',
    default_value_t = 2,
    value_parser = value_parser!(u8).range(0..3),
    verbatim_doc_comment,
  )]
  pub o: u8,

  /// Output file name (without file extension).
  #[arg(
    short = 'o',
    long,
    default_value_t = String::from("main"),
    verbatim_doc_comment,
  )]
  pub output: String,

  /// Disable LLVM IR verification.
  /// Useful for dumping LLVM IR.
  #[arg(short = 'n', long, default_value_t = false, verbatim_doc_comment)]
  pub no_verify_llvm: bool,

  /// Run in REPL (Read-Eval-Print Loop) mode.
  #[arg(short = 'r', long, default_value_t = false, verbatim_doc_comment)]
  pub repl: bool,

  /// Choose program execution mode.
  #[arg(
    short = 'm',
    long,
    value_enum,
    default_value_t = RunMode::Compile,
    verbatim_doc_comment,
  )]
  pub mode: RunMode,

  /// Number of threads used in compilation.
  /// If set to zero, all threads are used.
  #[arg(long, default_value_t = 0, verbatim_doc_comment)]
  pub threads: usize,

  /// Sort declarations.
  /// Disabled by default, enable for deterministic dumps
  #[arg(long, default_value_t = false, verbatim_doc_comment)]
  pub sort_decls: bool,
}
