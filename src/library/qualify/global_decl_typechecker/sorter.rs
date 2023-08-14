use anyhow::{bail, Result};
use std::collections::HashMap;

use crate::library::{ast::untyped, qualify::Nameable};

#[derive(Clone)]
pub enum Mark {
  None,
  Temp,
  Perm,
}

#[derive(Clone)]
pub struct MarkedNode<T> {
  pub mark: Mark,
  pub elem: T,
}

pub trait Sortable<S> {
  fn get_inner(
    &self,
    mark: HashMap<String, MarkedNode<S>>,
  ) -> Vec<MarkedNode<S>>;
}

impl Sortable<untyped::Struct> for untyped::Struct {
  fn get_inner(
    &self,
    marks: HashMap<String, MarkedNode<untyped::Struct>>,
  ) -> Vec<MarkedNode<untyped::Struct>> {
    let mut inner_structs = Vec::new();

    for (_, arg_type) in self.args.clone() {
      let untyped::Type::Simple(struct_name) = arg_type else {
        continue
      };

      let Some(node) = marks.get(struct_name.as_str()) else {
        continue
      };

      inner_structs.push(node.clone())
    }

    inner_structs
  }
}

impl Sortable<untyped::ValDecl> for untyped::ValDecl {
  fn get_inner(
    &self,
    marks: HashMap<String, MarkedNode<untyped::ValDecl>>,
  ) -> Vec<MarkedNode<untyped::ValDecl>> {
    let mut inner_vals = Vec::new();

    for val_name in self.inner_vals.clone() {
      let Some(node) = marks.get(val_name.as_str()) else {
        continue
      };

      inner_vals.push(node.clone())
    }

    inner_vals
  }
}

pub struct Sorter<T: Nameable + Clone + Sortable<T>> {
  marks: HashMap<String, MarkedNode<T>>,
  sorted: Vec<T>,
}

impl<T: Nameable + Clone + Sortable<T>> Sorter<T> {
  pub fn create(es: Vec<T>) -> Self {
    let marks = es
      .into_iter()
      .map(|e| {
        (
          e.get_name(),
          MarkedNode {
            elem: e,
            mark: Mark::None,
          },
        )
      })
      .collect();
    Self {
      marks,
      sorted: Vec::new(),
    }
  }

  pub fn sort(&mut self) -> Result<Vec<T>> {
    loop {
      let marks = self.marks.clone();

      let node = marks
        .values()
        .find(|MarkedNode { mark, .. }| matches!(mark, Mark::None));

      let Some(node) = node else {
        return Ok(self.sorted.clone())
      };

      self.visit(node)?
    }
  }

  fn visit(&mut self, node: &MarkedNode<T>) -> Result<()> {
    match node.mark {
      Mark::Perm => return Ok(()),
      Mark::Temp => bail!("cyclic reference found"),
      Mark::None => {
        let elem = node.elem.clone();

        self.marks.insert(
          elem.get_name(),
          MarkedNode {
            elem: elem.clone(),
            mark: Mark::Temp,
          },
        );

        let marks = self.marks.clone();

        for node in elem.get_inner(marks) {
          self.visit(&node)?
        }

        self.marks.insert(
          elem.get_name(),
          MarkedNode {
            elem: elem.clone(),
            mark: Mark::Perm,
          },
        );

        self.sorted.push(elem)
      }
    };

    Ok(())
  }
}

#[cfg(test)]
mod test {
  use rstest::*;

  use super::*;
  use crate::library::ast::*;

  #[rstest]
  fn test_struct_sort() {
    // given
    let t1 = untyped::Struct {
      name: "T1".to_string(),
      args: Vec::from([
        ("a".to_string(), Type::Simple("Int".to_string())),
        ("b".to_string(), Type::Simple("T2".to_string())),
        ("c".to_string(), Type::Simple("Bool".to_string())),
      ]),
    };
    let t2 = untyped::Struct {
      name: "T2".to_string(),
      args: Vec::from([("a".to_string(), Type::Simple("String".to_string()))]),
    };
    let t3 = untyped::Struct {
      name: "T3".to_string(),
      args: Vec::from([
        ("a".to_string(), Type::Simple("Int".to_string())),
        ("b".to_string(), Type::Simple("T2".to_string())),
        ("c".to_string(), Type::Simple("String".to_string())),
      ]),
    };
    let structs = Vec::from([t1.clone(), t2.clone(), t3.clone()]);
    let mut gs = Sorter::create(structs);

    // when
    let sorted = gs.sort().unwrap();

    // then
    let t1_pos = sorted.iter().position(|s| s.name == t1.name).unwrap();
    let t2_pos = sorted.iter().position(|s| s.name == t2.name).unwrap();
    let t3_pos = sorted.iter().position(|s| s.name == t3.name).unwrap();

    assert!(t2_pos < t1_pos);
    assert!(t2_pos < t3_pos);
  }
}
