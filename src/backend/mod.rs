mod compiler;
mod interpreter;

pub use compiler::Compiler;
pub use interpreter::Interpreter;

pub trait Backend {
    fn run(&self) -> Result<(), String>;
}
