mod utils;

use std::sync::Arc;

use derive_more::From;
use inkwell::{context::Context, module::Module};
use peg::{error::ParseError, str::LineCol};
use rayon::prelude::*;

use crate::library::{
  ast::{qualified, typed, untyped},
  backend::{Backend, Compiler, CompilerError, JitExecutor, JitExecutorError},
  cli::{Args, DumpMode, RunMode},
  codegen::{Codegen, ProcessCodegenNode},
  parser::parse,
  qualify::{
    GdcError, GdtError, GlobalDeclCollector, GlobalDeclTypechecker,
    ImportQualifier, ImportQualifierError, ProcessImportQualifierNode,
  },
  typecheck::{TypecheckError, Typechecker},
};

use self::utils::{InputFile, InputFileError};

#[derive(Debug, From, Clone)]
pub enum CompileError {
  InputFile(InputFileError),
  Parse(ParseError<LineCol>),
  Gdc(GdcError),
  ImportQualifier(ImportQualifierError),
  Gdt(GdtError),
  Typecheck(TypecheckError),
  #[from(ignore)]
  ModuleVerification(String),
  #[from(ignore)]
  ModuleLinkage(String),
  NoFiles,
  JitExecutor(JitExecutorError),
  Compiler(CompilerError),
}

pub struct Compile {
  args: Args,
  context: Context,
}

impl Compile {
  pub fn create(args: Args) -> Self {
    let context = Context::create();
    Self { args, context }
  }

  pub fn compile(&self) -> Result<(), CompileError> {
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
      RunMode::Jit => JitExecutor::create(m, o).run()?,
      RunMode::Compile => Compiler::create(m, o).run()?,
    };

    Ok(())
  }

  fn read_files(
    &self,
  ) -> impl ParallelIterator<Item = Result<InputFile, CompileError>> + Clone
  {
    InputFile::read_files(self.args.paths.clone()).map(|f| Ok(f?))
  }

  fn parse_files<FS>(
    &self,
    files: FS,
  ) -> impl ParallelIterator<Item = Result<untyped::Mod, CompileError>> + Clone
  where
    FS: ParallelIterator<Item = Result<InputFile, CompileError>> + Clone,
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

  fn qualify_modules<MS>(
    &self,
    ms: MS,
  ) -> impl ParallelIterator<Item = Result<qualified::Mod, CompileError>>
  where
    MS: ParallelIterator<Item = Result<untyped::Mod, CompileError>>
      + Clone
      + 'static,
  {
    let global_decl_collector = GlobalDeclCollector::create();
    let mut global_decl_typechecker = GlobalDeclTypechecker::create();

    let untyped_global_decls =
      global_decl_collector.collect(ms.clone().filter_map(|m| m.ok()));

    let typed_global_decls = Arc::new(
      untyped_global_decls
        .map_err(CompileError::Gdc)
        .and_then(|ds| {
          let r = global_decl_typechecker.check(ds)?;
          Ok(r)
        })
        .map(Arc::new),
    );

    ms.map(move |module| {
      let typed_global_decls = typed_global_decls.clone();

      let qualifier = match typed_global_decls.as_ref() {
        Ok(tgds) => Ok(ImportQualifier::create(tgds.clone())),
        Err(e) => Err(e),
      };

      let module = module?;
      let qualifier = qualifier.map_err(|e| e.clone())?;

      let result = module.process(&qualifier)?;
      Ok(result)
    })
  }

  fn typecheck_modules<QMS>(
    &self,
    qms: QMS,
  ) -> impl ParallelIterator<Item = Result<typed::Mod, CompileError>>
  where
    QMS: ParallelIterator<Item = Result<qualified::Mod, CompileError>>,
  {
    qms.map(|module| {
      let module = module?;
      let result = Typechecker::create(module).check()?;
      Ok(result)
    })
  }

  fn compile_modules<TMS>(
    &self,
    tms: TMS,
  ) -> impl Iterator<Item = Result<Codegen, CompileError>>
  where
    TMS: ParallelIterator<Item = Result<typed::Mod, CompileError>>,
  {
    let evaluated_tms = tms.collect::<Vec<_>>();

    evaluated_tms.into_iter().map(|module| {
      let mut codegen =
        Codegen::create(&self.context, true, self.args.sort_decls);

      module?.process(&mut codegen);
      Ok(codegen)
    })
  }

  fn verify_modules<'ctx, 'a, CGS>(
    &'a self,
    cgs: CGS,
  ) -> impl Iterator<Item = Result<Codegen<'ctx>, CompileError>> + 'a
  where
    CGS: Iterator<Item = Result<Codegen<'ctx>, CompileError>> + 'a,
  {
    cgs.map(move |c| {
      let c = c?;

      if !self.args.no_verify_llvm {
        c.module
          .verify()
          .map_err(|e| CompileError::ModuleVerification(e.to_string()))?;
      };

      Ok(c)
    })
  }

  fn link_modules<'ctx, CGS>(
    &self,
    cgs: CGS,
  ) -> Result<Module<'ctx>, CompileError>
  where
    CGS: Iterator<Item = Result<Codegen<'ctx>, CompileError>>,
  {
    cgs
      .map(|codegen| Ok(codegen?.module))
      .reduce(|m1, m2| {
        let m1 = m1?;
        let m2 = m2?;
        m1.link_in_module(m2)
          .map_err(|e| CompileError::ModuleLinkage(e.to_string()))?;
        Ok(m1)
      })
      .ok_or_else(|| CompileError::NoFiles)?
  }
}
