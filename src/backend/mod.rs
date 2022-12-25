mod compiler;
mod interpreter;
mod repl;

pub use compiler::Compiler;
pub use interpreter::Interpreter;
pub use repl::REPL;

pub trait Backend {
    fn run(&self) -> Result<(), String>;
}
