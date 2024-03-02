use macros::define_map;

use crate::library::ast::typed::{
  Fun, FunSig, Impl, ImplDecl, SimpleType, Type,
};

trait InstantiateSubject {
  fn get_type(&self) -> SimpleType;

  fn instantiate_type(&self, t: Type) -> Type {
    match t {
      Type::Simple(SimpleType::This) => Type::Simple(self.get_type()),
      _ => t,
    }
  }

  fn instantiate_funsig(&self, fs: FunSig) -> FunSig {
    let name = fs.name;
    let args = fs
      .args
      .into_iter()
      .map(|mut arg| {
        arg.type_ = self.instantiate_type(arg.type_);
        arg
      })
      .collect();
    let rt = self.instantiate_type(fs.rt);
    FunSig { name, args, rt }
  }

  fn instantiate_fun(&self, f: Fun) -> Fun {
    let sig = self.instantiate_funsig(f.sig);
    let body = f.body;

    Fun { sig, body }
  }

  define_map!(instantiate_funsig, FunSig);

  define_map!(instantiate_fun, Fun);
}

pub trait Instantiate {
  fn instantiate(&self) -> Self;
}

impl InstantiateSubject for Impl {
  fn get_type(&self) -> SimpleType {
    self.t.clone()
  }
}

impl Instantiate for Impl {
  fn instantiate(&self) -> Self {
    let class_name = self.class_name.clone();
    let t = self.t.clone();
    let impls = self.instantiate_funs(self.impls.clone());

    Self {
      class_name,
      t,
      impls,
    }
  }
}

impl InstantiateSubject for ImplDecl {
  fn get_type(&self) -> SimpleType {
    self.t.clone()
  }
}

impl Instantiate for ImplDecl {
  fn instantiate(&self) -> Self {
    let class_name = self.class_name.clone();
    let t = self.t.clone();
    let impls = self.instantiate_funsigs(self.impls.clone());

    Self {
      class_name,
      t,
      impls,
    }
  }
}
