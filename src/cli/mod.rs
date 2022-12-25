use clap::{arg, Parser, ValueEnum};

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

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    pub files: Vec<String>,

    /// Choose dump mode
    #[arg(short = 'd', long, value_enum, default_value_t = DumpMode::None)]
    pub dump_mode: DumpMode,

    /// Print LLVM IR to stdout
    #[arg(short = 't', long, value_enum, default_value_t = DumpTarget::StdOut)]
    pub dump_target: DumpTarget,

    /// Set optimization level:
    /// 0 = None,
    /// 1 = Less,
    /// 2 = Default,
    /// 3 = Aggressive
    #[arg(id = "Opt Level", short = 'O', default_value_t = 2)]
    pub o: u32,

    /// Disable LLVM IR verification.
    /// Useful for dumping LLVM IR
    #[arg(short = 'n', long, default_value_t = false)]
    pub no_verify_llvm: bool,

    /// Run in REPL (Read-Eval-Print Loop) mode
    #[arg(short = 'r', long, default_value_t = false)]
    pub repl: bool,

    /// Choose program execution mode
    #[arg(short = 'm', long, value_enum, default_value_t = RunMode::JIT)]
    pub mode: RunMode,
}
