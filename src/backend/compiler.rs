use super::Backend;

pub struct Compiler {}

impl Backend for Compiler {
    fn run(&self) -> Result<(), String> {
        todo!()
    }
}

impl Compiler {
    pub fn create() -> Self {
        Self {}
    }
}
