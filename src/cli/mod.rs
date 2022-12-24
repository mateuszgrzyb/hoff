use clap::{arg, Parser, ValueEnum};

#[derive(ValueEnum, Clone, Debug)]
#[value(rename_all = "lower")]
pub enum EmitLLVMTarget {
    IrFile,
    StdOut,
}

#[derive(ValueEnum, Clone, Debug)]
#[value(rename_all = "lower")]
pub enum RunMode {
    /// asdf
    JIT,
    /// asdf
    Compile,
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    pub files: Vec<String>,
    /// Print AST to stdout
    #[arg(short = 'a', long, default_value_t = false)]
    pub dump_ast: bool,

    /// Print typed AST to stdout
    #[arg(short = 't', long, default_value_t = false)]
    pub dump_typed_ast: bool,

    /// Emit LLVM IR to defined target
    #[arg(short = 'e', long, default_value_t = false)]
    pub emit_llvm: bool,

    /// Print LLVM IR to stdout
    #[arg(short = 'f', long, value_enum, default_value_t = EmitLLVMTarget::IrFile)]
    pub emit_llvm_target: EmitLLVMTarget,

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
