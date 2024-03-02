mod utils;

use std::sync::Arc;

use anyhow::{anyhow, Result};
use inkwell::{context::Context, module::Module};
use rayon::prelude::*;

use crate::library::{
  ast::{qualified, typed, untyped},
  backend::{Backend, Compiler, JITExecutor},
  cli::{Args, DumpMode, RunMode},
  codegen::{CodeGen, ProcessASTNode},
  parser::parse,
  qualify::{GlobalDeclCollector, GlobalDeclTypechecker, ImportQualifier},
  typecheck::TypeChecker,
};

use self::utils::InputFile;

pub struct Compile {
  args: Args,
  context: Context,
}

impl Compile {
  pub fn create(args: Args) -> Self {
    let context = Context::create();
    Self { args, context }
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

  fn read_files(&self) -> impl ParallelIterator<Item = Result<InputFile>> + Clone {
    InputFile::read_files(self.args.paths.clone())
  }

  fn parse_files<FS>(&self, files: FS) -> impl ParallelIterator<Item = Result<untyped::Mod>> + Clone
  where
    FS: ParallelIterator<Item = Result<InputFile>> + Clone,
  {
    #[allow(clippy::let_unit_value)]
    files.map(|f| {
      let InputFile { name, contents } = f?;
      let defs = parse(&contents)?;
      let imports = ();
      Ok(untyped::Mod {
        name,
        defs,
        imports,
      })
    })
  }

  fn qualify_modules<MS>(&self, ms: MS) -> impl ParallelIterator<Item = Result<qualified::Mod>>
  where
    MS: ParallelIterator<Item = Result<untyped::Mod>> + Clone + 'static,
  {
    let global_decl_collector = GlobalDeclCollector::create();
    let mut global_decl_typechecker = GlobalDeclTypechecker::create();

    let untyped_global_decls = global_decl_collector.collect(ms.clone().filter_map(|m| m.ok()));

    let typed_global_decls = Arc::new(
      untyped_global_decls
        .and_then(|ds| global_decl_typechecker.check(ds))
        .map(Arc::new),
    );

    ms.map(move |module| {
      let typed_global_decls = typed_global_decls.clone();

      let qualifier = match typed_global_decls.as_ref() {
        Ok(tgds) => Ok(ImportQualifier::create(tgds.clone())),
        Err(e) => Err(anyhow!(e.to_string())),
      };

      qualifier?.qualify(module?)
    })
  }

  fn typecheck_modules<QMS>(&self, qms: QMS) -> impl ParallelIterator<Item = Result<typed::Mod>>
  where
    QMS: ParallelIterator<Item = Result<qualified::Mod>>,
  {
    qms.map(|module| TypeChecker::create(module?).check())
  }

  fn compile_modules<TMS>(&self, tms: TMS) -> impl Iterator<Item = Result<CodeGen>>
  where
    TMS: ParallelIterator<Item = Result<typed::Mod>>,
  {
    let evaluated_tms = tms.collect::<Vec<_>>();

    evaluated_tms.into_iter().map(|module| {
      let mut codegen = CodeGen::create(&self.context, true, self.args.sort_decls);

      module?.process(&mut codegen);
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
        m1.link_in_module(m2).map_err(|e| anyhow!(e.to_string()))?;
        Ok(m1)
      })
      .ok_or_else(|| anyhow!("No files were compiled"))?
  }
}
