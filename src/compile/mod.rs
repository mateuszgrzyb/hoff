mod utils;

use std::rc::Rc;

use anyhow::anyhow;
use anyhow::Result;
use inkwell::{context::Context, module::Module};

use crate::library::{
  ast::{qualified, typed, untyped},
  backend::{Backend, Compiler, JITExecutor},
  cli::{Args, DumpMode, RunMode},
  codegen::CodeGen,
  parser::parse,
  qualify::{GlobalDeclCollector, GlobalDeclTypechecker, ImportQualifier},
  typecheck::TypeChecker,
};

pub struct Compile {
  args: Args,
  contexts: Vec<Context>,
}

impl Compile {
  pub fn create(args: Args) -> Self {
    let no_of_files = args.files.len();
    Self {
      args,
      contexts: utils::initialize_contexts(no_of_files),
    }
  }

  pub fn compile(&self) -> Result<()> {
    let files = self.read_files();

    let modules = self.parse_files(files);

    if let DumpMode::Ast = self.args.dump_mode {
      return utils::dump(&self.args, modules);
    };

    let qualified_modules = self.qualify_modules(modules);

    if let DumpMode::QualifiedAst = self.args.dump_mode {
      return utils::dump(&self.args, qualified_modules);
    };

    let typed_modules = self.typecheck_modules(qualified_modules);

    if let DumpMode::TypedAst = self.args.dump_mode {
      return utils::dump(&self.args, typed_modules);
    };

    let codegens = self.compile_modules(typed_modules);

    let codegens = self.verify_modules(codegens);

    if let DumpMode::LlvmIr = self.args.dump_mode {
      return utils::dump_llvm(&self.args, codegens);
    }

    let m = self.link_modules(codegens)?;
    let o = self.args.o;

    match self.args.mode {
      RunMode::JIT => JITExecutor::create(m, o).run()?,
      RunMode::Compile => Compiler::create(m, o).run()?,
    };

    Ok(())
  }

  fn read_files(
    &self,
  ) -> impl Iterator<Item = Result<(String, String)>> + Clone {
    self.args.files.clone().into_iter().map(|name| {
      std::fs::read_to_string(name.clone())
        .map(|file| (name, file))
        .map_err(|err| err.into())
    })
  }

  fn parse_files<FS>(
    &self,
    files: FS,
  ) -> impl Iterator<Item = Result<untyped::Mod>> + Clone
  where
    FS: Iterator<Item = Result<(String, String)>> + Clone,
  {
    #[allow(clippy::let_unit_value)]
    files.into_iter().map(|f| {
      let (name, file) = f?;
      let decls = parse(&file)?;
      let imports = ();
      Ok(untyped::Mod {
        name,
        decls,
        imports,
      })
    })
  }

  fn qualify_modules<MS>(
    &self,
    ms: MS,
  ) -> Box<dyn Iterator<Item = Result<qualified::Mod>>>
  where
    MS: Iterator<Item = Result<untyped::Mod>> + Clone + 'static,
  {
    let mut global_decl_collector = GlobalDeclCollector::create();
    let mut global_decl_typechecker = GlobalDeclTypechecker::create();

    let untyped_global_decls =
      global_decl_collector.collect(ms.clone().filter_map(|m| m.ok()));
    let typed_global_decls =
      match global_decl_typechecker.check(untyped_global_decls) {
        Ok(t) => Rc::new(t),
        Err(e) => return Box::new(std::iter::once(Err(e))),
      };

    let mut qualifier = ImportQualifier::create(typed_global_decls);

    Box::new(ms.map(move |module| qualifier.qualify(module?)))
  }

  fn typecheck_modules<QMS>(
    &self,
    qms: QMS,
  ) -> impl Iterator<Item = Result<typed::Mod>>
  where
    QMS: Iterator<Item = Result<qualified::Mod>>,
  {
    qms.map(|module| {
      let mut typechecker = TypeChecker::create();
      typechecker.check(module?)
    })
  }

  fn compile_modules<TMS>(
    &self,
    tms: TMS,
  ) -> impl Iterator<Item = Result<CodeGen>>
  where
    TMS: Iterator<Item = Result<typed::Mod>>,
  {
    tms.enumerate().map(|(i, module)| {
      let context = &self.contexts[i];
      let mut codegen = CodeGen::create(context, true);
      codegen.compile_module(module?);
      Ok(codegen)
    })
  }

  fn verify_modules<'ctx, 'a, CGS>(
    &'a self,
    cgs: CGS,
  ) -> impl Iterator<Item = Result<CodeGen<'ctx>>> + 'a
  where
    CGS: Iterator<Item = Result<CodeGen<'ctx>>> + 'a,
  {
    cgs.map(move |c| {
      let c = c?;

      if !self.args.no_verify_llvm {
        c.module.verify().map_err(|e| anyhow!(e.to_string()))?;
      };

      Ok(c)
    })
  }

  fn link_modules<'ctx, CGS>(&self, cgs: CGS) -> Result<Module<'ctx>>
  where
    CGS: Iterator<Item = Result<CodeGen<'ctx>>>,
  {
    cgs
      .map(|codegen| Ok(codegen?.module))
      .reduce(|m1: Result<Module>, m2| {
        let m1 = m1?;
        let m2 = m2?;
        m1.link_in_module(m2).unwrap();
        Ok(m1)
      })
      .ok_or_else(|| anyhow!("No files were compiled"))?
  }
}
