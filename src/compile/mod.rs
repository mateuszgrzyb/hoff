mod utils;

use crate::library::ast::{qualified, typed, untyped};
use crate::library::backend::{Backend, Compiler, Interpreter};
use crate::library::cli::{Args, DumpMode, RunMode};
use crate::library::codegen::CodeGen;
use crate::library::import_qualifier::{
    ImportPreQualifier, ImportQualifier, TypeCheckPreQualified,
};
use crate::library::parser::grammar::ModParser;
use crate::library::typecheck::TypeChecker;
use inkwell::context::Context;
use inkwell::module::Module;
use std::error::Error;

pub struct Compile {
    args: Args,
    contexts: Vec<Context>,
    parser: ModParser,
}

impl Compile {
    pub fn create(args: Args) -> Self {
        let no_of_files = args.files.len();
        Self {
            args,
            contexts: utils::initialize_contexts(no_of_files),
            parser: ModParser::new(),
        }
    }

    pub fn compile(&self) -> Result<(), Box<dyn Error>> {
        let files = self.read_files()?;

        let modules = self.parse_files(files).into_iter();

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

        let backend: Box<dyn Backend> = match self.args.mode {
            RunMode::JIT => Box::new(Interpreter::create(m, self.args.o)),
            RunMode::Compile => Box::new(Compiler::create(m, self.args.o)),
        };

        backend.run()?;

        Ok(())
    }

    fn read_files(&self) -> Result<Vec<(String, String)>, std::io::Error> {
        self.args
            .files
            .clone()
            .into_iter()
            .map(|name| {
                std::fs::read_to_string(name.clone()).map(|file| (name, file))
            })
            .collect()
    }

    fn parse_files(&self, files: Vec<(String, String)>) -> Vec<untyped::Mod> {
        files
            .into_iter()
            .map(move |(name, file)| {
                let decls = self.parser.parse(&file).unwrap();
                untyped::Mod {
                    name,
                    decls,
                    imports: (),
                }
            })
            .collect()
    }

    fn qualify_modules<MS>(
        &self,
        ms: MS,
    ) -> Result<Vec<qualified::Mod>, String>
    where
        MS: Iterator<Item = untyped::Mod>,
    {
        let mut ipq = ImportPreQualifier::create();
        let mut tcpq = TypeCheckPreQualified::create();

        let ms = ms.collect::<Vec<_>>();

        let (fds, ss) = ipq.pre_qualify(&ms);
        let (fds, ss) = tcpq.run(fds, ss).map_err(|err| err.to_string())?;

        let mut qualifier = ImportQualifier::create(&fds, &ss);

        ms.clone()
            .into_iter()
            .map(|module| qualifier.qualify(module))
            .collect()
    }

    fn typecheck_modules<QMS>(
        &self,
        qms: QMS,
    ) -> Result<Vec<typed::Mod>, String>
    where
        QMS: Iterator<Item = qualified::Mod>,
    {
        qms.map(|module| {
            let mut typechecker = TypeChecker::create();
            typechecker.check(module)
        })
        .collect()
    }

    fn compile_modules<TMS>(&self, tms: TMS) -> Vec<CodeGen>
    where
        TMS: Iterator<Item = typed::Mod>,
    {
        tms.enumerate()
            .map(|(i, module)| {
                let context = &self.contexts[i];
                let mut codegen = CodeGen::create(context, true);
                codegen.compile_module(module);
                codegen
            })
            .collect()
    }

    fn verify_modules(
        &self,
        cgs: &Vec<CodeGen>,
    ) -> Result<(), Box<dyn Error>> {
        for c in cgs.iter() {
            c.module.verify()?
        }

        Ok(())
    }

    fn link_modules<'ctx>(
        &self,
        cgs: Vec<CodeGen<'ctx>>,
    ) -> Result<Module<'ctx>, String> {
        cgs.into_iter()
            .map(|codegen| codegen.module)
            .reduce(|m1, m2| {
                m1.link_in_module(m2).unwrap();
                m1
            })
            .ok_or_else(|| format!("No files were compiled"))
    }
}
