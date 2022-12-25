use inkwell::types::{FloatType, IntType, PointerType};

pub struct Types<'ctx> {
    pub int: IntType<'ctx>,
    pub float: FloatType<'ctx>,
    pub bool: IntType<'ctx>,
    pub string: PointerType<'ctx>,
}
