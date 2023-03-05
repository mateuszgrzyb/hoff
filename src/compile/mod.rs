mod utils;

use std::error::Error;

use inkwell::{
  context::Context,
  module::Module,
};

use crate::library::{
  ast::{
    qualified,
    typed,
    untyped,
  },
  backend::{
    Backend,
    Compiler,
    JITExecutor,
  },
  cli::{
    Args,
    DumpMode,
    RunMode,
  },
  codegen::CodeGen,
  parser::parse,
  qualify::{
    GlobalDeclCollector,
    GlobalDeclTypechecker,
    ImportQualifier,
  },
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

  pub fn compile(&self) -> Result<(), Box<dyn Error>> {
    let files = self.read_files()?;

    let modules = self.parse_files(files)?.into_iter();

    if let DumpMode::Ast = self.args.dump_mode {
      return utils::dump(&self.args, modules);
    };

    let qualified_modules =
      self.qualify_modules(modules).map(Vec::into_iter)?;

    if let DumpMode::QualifiedAst = self.args.dump_mode {
      return utils::dump(&self.args, qualified_modules);
    };

    let typed_modules = self
      .typecheck_modules(qualified_modules)
      .map(Vec::into_iter)?;

    if let DumpMode::TypedAst = self.args.dump_mode {
      return utils::dump(&self.args, typed_modules);
    };

    let codegens = self.compile_modules(typed_modules);

    if !self.args.no_verify_llvm {
      self.verify_modules(&codegens)?
    }

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

  fn read_files(&self) -> Result<Vec<(String, String)>, Box<dyn Error>> {
    self
      .args
      .files
      .clone()
      .into_iter()
      .map(|name| {
        std::fs::read_to_string(name.clone())
          .map(|file| (name, file))
          .map_err(|err| err.into())
      })
      .collect()
  }

  fn parse_files(
    &self,
    files: Vec<(String, String)>,
  ) -> Result<Vec<untyped::Mod>, Box<dyn Error>> {
    files
      .into_iter()
      .map(move |(name, file)| {
        let decls = parse(&file)?;
        let imports = ();
        Ok(untyped::Mod {
          name,
          decls,
          imports,
        })
      })
      .collect()
  }

  fn qualify_modules<MS>(
    &self,
    ms: MS,
  ) -> Result<Vec<qualified::Mod>, Box<dyn Error>>
  where
    MS: Iterator<Item = untyped::Mod>,
  {
    let ms = ms.collect::<Vec<_>>();

    let mut global_decl_collector = GlobalDeclCollector::create();
    let mut global_decl_typechecker = GlobalDeclTypechecker::create();

    let untyped_global_decls = global_decl_collector.collect(&ms);
    let typed_global_decls =
      global_decl_typechecker.check(untyped_global_decls)?;

    let mut qualifier = ImportQualifier::create(&typed_global_decls);

    ms.clone()
      .into_iter()
      .map(|module| qualifier.qualify(module))
      .collect()
  }

  fn typecheck_modules<QMS>(
    &self,
    qms: QMS,
  ) -> Result<Vec<typed::Mod>, Box<dyn Error>>
  where
    QMS: Iterator<Item = qualified::Mod>,
  {
    qms
      .map(|module| {
        let mut typechecker = TypeChecker::create();
        typechecker.check(module)
      })
      .collect()
  }

  fn compile_modules<TMS>(&self, tms: TMS) -> Vec<CodeGen>
  where
    TMS: Iterator<Item = typed::Mod>,
  {
    tms
      .enumerate()
      .map(|(i, module)| {
        let context = &self.contexts[i];
        let mut codegen = CodeGen::create(context, true);
        codegen.compile_module(module);
        codegen
      })
      .collect()
  }

  fn verify_modules(&self, cgs: &Vec<CodeGen>) -> Result<(), Box<dyn Error>> {
    for c in cgs.iter() {
      c.module.verify()?
    }

    Ok(())
  }

  fn link_modules<'ctx>(
    &self,
    cgs: Vec<CodeGen<'ctx>>,
  ) -> Result<Module<'ctx>, Box<dyn Error>> {
    cgs
      .into_iter()
      .map(|codegen| codegen.module)
      .reduce(|m1, m2| {
        m1.link_in_module(m2).unwrap();
        m1
      })
      .ok_or_else(|| "No files were compiled".into())
  }
}
