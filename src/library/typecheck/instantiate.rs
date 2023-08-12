use crate::library::ast::typed::{Fun, FunSig, Impl, SimpleType, Type};

pub trait Instantiate {
  fn instantiate(&self) -> Self;
}

impl Impl {
  fn instantiate_type(&self, t: Type) -> Type {
    match t {
      Type::Simple(SimpleType::This) => Type::Simple(self.t.clone()),
      _ => t,
    }
  }

  fn instantiate_method(&self, f: Fun) -> Fun {
    let name = f.sig.name;
    let args = f
      .sig
      .args
      .into_iter()
      .map(|(v, t)| (v, self.instantiate_type(t)))
      .collect();
    let rt = self.instantiate_type(f.sig.rt);
    let body = f.body;

    Fun {
      sig: FunSig { name, args, rt },
      body,
    }
  }
}

impl Instantiate for Impl {
  fn instantiate(&self) -> Self {
    let class_name = self.class_name.clone();
    let t = self.t.clone();
    let impls = self
      .impls
      .clone()
      .into_iter()
      .map(|i| self.instantiate_method(i))
      .collect();
    Self {
      class_name,
      t,
      impls,
    }
  }
}
