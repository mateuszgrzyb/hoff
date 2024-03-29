use std::collections::HashMap;

pub struct Namespace {
  nested_name: Vec<String>,
  qualified_names: HashMap<String, String>,
}

impl Namespace {
  pub fn new() -> Self {
    Self {
      nested_name: Vec::new(),
      qualified_names: HashMap::new(),
    }
  }

  pub fn create_qualified_name(&mut self, name: String) -> String {
    self.nested_name.push(name.clone());
    let qualified_name = self.nested_name.join("$$$");
    self.qualified_names.insert(name, qualified_name.clone());
    qualified_name
  }

  pub fn get_qualified_name(&self, name: String) -> String {
    self
      .qualified_names
      .get(name.as_str())
      .unwrap_or(&name)
      .clone()
  }

  pub fn drop_qualified_name(&mut self) {
    self.nested_name.pop();
  }
}
