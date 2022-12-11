use clap::{Parser, ValueEnum};

#[derive(ValueEnum, Clone, Debug)]
pub enum EmitLLVMTarget {
    IrFile,
    StdOut,
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    pub files: Vec<String>,
    /// Print AST to stdout
    #[arg(long, default_value_t = false)]
    pub dump_ast: bool,

    /// Print typed AST to stdout
    #[arg(long, default_value_t = false)]
    pub dump_typed_ast: bool,

    /// Emit LLVM IR to defined target
    #[arg(long, default_value_t = false)]
    pub emit_llvm: bool,

    /// Print LLVM IR to stdout
    #[arg(long, value_enum, default_value_t = EmitLLVMTarget::IrFile)]
    pub emit_llvm_target: EmitLLVMTarget,

    /// Set optimization level
    /// 0 = None
    /// 1 = Less
    /// 2 = Default
    /// 3 = Aggressive
    #[arg(short, default_value_t = 2)]
    pub O: u32,

    #[arg(long, default_value_t = false)]
    pub no_verify_llvm: bool,

    #[arg(long, default_value_t = false)]
    pub repl: bool,
}
