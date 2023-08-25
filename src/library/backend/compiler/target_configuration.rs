use std::{process::Command, rc::Rc};

use inkwell::targets::TargetTriple;

pub struct TargetConfiguration {
  pub target_triple: TargetTriple,
  pub link_command: fn(&[String], &str) -> Command,
}

impl TargetConfiguration {
  pub fn create(
    triple: &str,
    link: fn(&[String], &str) -> Command,
  ) -> (&str, Rc<Self>) {
    (
      triple,
      Rc::new(TargetConfiguration {
        target_triple: TargetTriple::create(triple),
        link_command: link,
      }),
    )
  }
}
