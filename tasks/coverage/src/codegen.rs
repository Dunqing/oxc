use std::path::{Path, PathBuf};

use oxc_allocator::Allocator;
use oxc_codegen::{Codegen, CodegenOptions};
use oxc_parser::Parser;
use oxc_span::SourceType;

use crate::{
    babel::BabelCase,
    suite::{Case, TestResult},
    test262::{Test262Case, TestFlag},
    typescript::TypeScriptCase,
};

pub struct CodegenTest262Case {
    base: Test262Case,
}

impl Case for CodegenTest262Case {
    fn new(path: PathBuf, code: String) -> Self {
        Self { base: Test262Case::new(path, code) }
    }

    fn code(&self) -> &str {
        self.base.code()
    }

    fn path(&self) -> &Path {
        self.base.path()
    }

    fn test_result(&self) -> &TestResult {
        self.base.test_result()
    }

    fn skip_test_case(&self) -> bool {
        self.base.should_fail()
    }

    fn run(&mut self) {
        let source_text = self.base.code();
        let is_module = self.base.meta().flags.contains(&TestFlag::Module);
        let source_type = SourceType::default().with_module(is_module);
        let result = get_result(source_text, source_type, CodegenOptions);
        self.base.set_result(result);
    }
}

pub struct CodegenBabelCase {
    base: BabelCase,
}

impl Case for CodegenBabelCase {
    fn new(path: PathBuf, code: String) -> Self {
        Self { base: BabelCase::new(path, code) }
    }

    fn code(&self) -> &str {
        self.base.code()
    }

    fn path(&self) -> &Path {
        self.base.path()
    }

    fn test_result(&self) -> &TestResult {
        self.base.test_result()
    }

    fn skip_test_case(&self) -> bool {
        self.base.skip_test_case() || self.base.should_fail()
    }

    fn run(&mut self) {
        let source_text = self.base.code();
        let source_type = self.base.source_type();
        let result = get_result(source_text, source_type, CodegenOptions);
        self.base.set_result(result);
    }
}

pub struct CodegenTypeScriptCase {
    base: TypeScriptCase,
}

impl Case for CodegenTypeScriptCase {
    fn new(path: PathBuf, code: String) -> Self {
        Self { base: TypeScriptCase::new(path, code) }
    }

    fn code(&self) -> &str {
        self.base.code()
    }

    fn path(&self) -> &Path {
        self.base.path()
    }

    fn test_result(&self) -> &TestResult {
        self.base.test_result()
    }

    fn skip_test_case(&self) -> bool {
        self.base.skip_test_case() || self.base.should_fail()
    }

    fn run(&mut self) {
        let source_text = self.base.code();
        let source_type = self.base.source_type();
        let result = get_result(source_text, source_type, CodegenOptions);
        self.base.set_result(result);
    }
}

fn get_result(source_text: &str, source_type: SourceType, options: CodegenOptions) -> TestResult {
    if !get_normal_result(source_text, source_type, options)
        || !get_minify_result(source_text, source_type, options)
    {
        TestResult::ParseError(String::new(), false)
    } else {
        TestResult::Passed
    }
}

fn get_normal_result(source_text: &str, source_type: SourceType, options: CodegenOptions) -> bool {
    let allocator = Allocator::default();
    let program1 = Parser::new(&allocator, source_text, source_type).parse().program;
    let source_text1 = Codegen::<false>::new(source_text.len(), options).build(&program1);
    let program2 = Parser::new(&allocator, &source_text1, source_type).parse().program;
    let source_text2 = Codegen::<false>::new(source_text1.len(), options).build(&program2);
    source_text1 == source_text2
}

fn get_minify_result(source_text: &str, source_type: SourceType, options: CodegenOptions) -> bool {
    let allocator = Allocator::default();
    let program1 = Parser::new(&allocator, source_text, source_type).parse().program;
    let source_text1 = Codegen::<true>::new(source_text.len(), options).build(&program1);
    let program2 = Parser::new(&allocator, &source_text1, source_type).parse().program;
    let source_text2 = Codegen::<true>::new(source_text1.len(), options).build(&program2);
    source_text1 == source_text2
}
