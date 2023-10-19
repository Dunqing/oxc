use indexmap::IndexMap;
use serde::de::DeserializeOwned;
use serde_json::Value;
use std::{
    cell::RefCell,
    fs::{self, File},
    io::Write,
    path::{Path, PathBuf},
    process::Command,
    rc::Rc,
};
use walkdir::WalkDir;

use oxc_allocator::Allocator;
use oxc_codegen::{Codegen, CodegenOptions};
use oxc_parser::Parser;
use oxc_semantic::SemanticBuilder;
use oxc_span::{SourceType, VALID_EXTENSIONS};
use oxc_tasks_common::{normalize_path, project_root, BabelOptions};
use oxc_transformer::{
    NullishCoalescingOperatorOptions, ReactJsxOptions, TransformOptions, TransformTarget,
    Transformer,
};

#[test]
#[cfg(any(coverage, coverage_nightly))]
fn test() {
    TestRunner::new(TestRunnerOptions::default()).run();
}

#[derive(Default)]
pub struct TestRunnerOptions {
    pub filter: Option<String>,
    pub exec: bool,
}

/// The test runner which walks the babel repository and searches for transformation tests.
pub struct TestRunner {
    options: TestRunnerOptions,
}

fn root() -> PathBuf {
    project_root().join("tasks/coverage/babel/packages")
}

const CASES: &[&str] = &[
    // ES2024
    "babel-plugin-transform-unicode-sets-regex",
    // ES2022
    "babel-plugin-transform-class-properties",
    "babel-plugin-transform-class-static-block",
    "babel-plugin-transform-private-methods",
    "babel-plugin-transform-private-property-in-object",
    // [Syntax] "babel-plugin-transform-syntax-top-level-await",
    // ES2021
    "babel-plugin-transform-logical-assignment-operators",
    "babel-plugin-transform-numeric-separator",
    // ES2020
    "babel-plugin-transform-export-namespace-from",
    "babel-plugin-transform-dynamic-import",
    "babel-plugin-transform-nullish-coalescing-operator",
    "babel-plugin-transform-optional-chaining",
    // [Syntax] "babel-plugin-transform-syntax-bigint",
    // [Syntax] "babel-plugin-transform-syntax-dynamic-import",
    // [Syntax] "babel-plugin-transform-syntax-import-meta",
    // ES2019
    "babel-plugin-transform-optional-catch-binding",
    "babel-plugin-transform-json-strings",
    // ES2018
    "babel-plugin-transform-async-generator-functions",
    "babel-plugin-transform-object-rest-spread",
    // [Regex] "babel-plugin-transform-unicode-property-regex",
    "babel-plugin-transform-dotall-regex",
    // [Regex] "babel-plugin-transform-named-capturing-groups-regex",
    // ES2017
    "babel-plugin-transform-async-to-generator",
    // ES2016
    "babel-plugin-transform-exponentiation-operator",
    // ES2015
    "babel-plugin-transform-shorthand-properties",
    "babel-plugin-transform-sticky-regex",
    "babel-plugin-transform-unicode-regex",
    // TypeScript
    "babel-plugin-transform-typescript",
    // React
    "babel-plugin-transform-react-jsx",
];

impl TestRunner {
    pub fn new(options: TestRunnerOptions) -> Self {
        Self { options }
    }

    /// # Panics
    pub fn run(self) {
        let root = root();
        let (transform_paths, exec_files) = Self::glob_files(&root);
        self.generate_snapshot(
            SnapshotOption {
                paths: transform_paths,
                root: root.clone(),
                snap_file_name: project_root().join("tasks/transform_conformance/babel.snap.md"),
            },
            |test_case| test_case.test(self.options.filter.as_deref()),
        );

        self.generate_snapshot(
            SnapshotOption {
                paths: exec_files,
                root,
                snap_file_name: project_root()
                    .join("tasks/transform_conformance/exec_runner.snap.md"),
            },
            TestCase::exec,
        );
    }

    fn glob_files(
        root: &Path,
    ) -> (IndexMap<String, Vec<TestCase>>, IndexMap<String, Vec<TestCase>>) {
        // use `IndexMap` to keep the order of the test cases same with the insert order.
        let mut transform_files = IndexMap::<String, Vec<TestCase>>::new();
        let mut exec_files = IndexMap::<String, Vec<TestCase>>::new();

        for case in CASES {
            let root = root.join(case).join("test/fixtures");
            let (mut transform_paths, mut exec_paths): (Vec<TestCase>, Vec<TestCase>) =
                WalkDir::new(root)
                    .into_iter()
                    .filter_map(Result::ok)
                    .filter_map(|e| {
                        let kind = TestCaseKind::from_path(e.path())?;
                        let test_case = TestCase::new(e.path().to_path_buf(), kind);
                        if test_case.skip_test_case() {
                            return None;
                        }
                        Some(test_case)
                    })
                    .partition(|p| p.kind == TestCaseKind::Transform);

            transform_paths.sort_unstable_by_key(|p| p.path.clone());
            exec_paths.sort_unstable_by_key(|p| p.path.clone());

            transform_files.insert((*case).to_string(), transform_paths);
            exec_files.insert((*case).to_string(), exec_paths);
        }

        (transform_files, exec_files)
    }

    fn generate_snapshot<F>(&self, option: SnapshotOption, predict: F)
    where
        F: Fn(&TestCase) -> bool,
    {
        let SnapshotOption { paths, root, snap_file_name } = option;
        let mut snapshot = String::new();
        let mut total = 0;
        let mut all_passed = vec![];
        let mut all_passed_count = 0;

        for (case, test_cases) in paths {
            let root = root.join(&case).join("test/fixtures");
            let num_of_tests = test_cases.len();
            total += num_of_tests;

            // Run the test
            let (passed, failed): (Vec<TestCase>, Vec<TestCase>) =
                test_cases.into_iter().partition(|test_case| predict(test_case));
            all_passed_count += passed.len();

            // Snapshot
            if failed.is_empty() {
                all_passed.push(case);
            } else {
                snapshot.push_str("# ");
                snapshot.push_str(&case);
                snapshot.push_str(&format!(" ({}/{})\n", passed.len(), num_of_tests));
                for test_case in failed {
                    snapshot.push_str("* ");
                    snapshot.push_str(&normalize_path(test_case.path.strip_prefix(&root).unwrap()));
                    snapshot.push('\n');
                }
                snapshot.push('\n');
            }
        }

        if self.options.filter.is_none() {
            let all_passed =
                all_passed.into_iter().map(|s| format!("* {s}")).collect::<Vec<_>>().join("\n");
            let snapshot = format!(
                "Passed: {all_passed_count}/{total}\n\n# All Passed:\n{all_passed}\n\n\n{snapshot}"
            );
            // let path = project_root().join("tasks/transform_conformance/babel.snap.md");
            let mut file = File::create(snap_file_name).unwrap();
            file.write_all(snapshot.as_bytes()).unwrap();
        }
    }
}

struct TestCase {
    path: PathBuf,
    options: BabelOptions,
    kind: TestCaseKind,
}

impl TestCase {
    fn new<P: Into<PathBuf>>(path: P, kind: TestCaseKind) -> Self {
        let path = path.into();
        let options = BabelOptions::from_path(path.parent().unwrap());
        Self { path, options, kind }
    }

    fn transform_options(&self) -> TransformOptions {
        fn get_options<T: Default + DeserializeOwned>(value: Option<Value>) -> T {
            value.and_then(|v| serde_json::from_value::<T>(v).ok()).unwrap_or_default()
        }

        TransformOptions {
            target: TransformTarget::ESNext,
            react_jsx: Some(ReactJsxOptions::default()),
            assumptions: self.options.assumptions,
            class_static_block: self.options.get_plugin("transform-class-static-block").is_some(),
            logical_assignment_operators: self
                .options
                .get_plugin("transform-logical-assignment-operators")
                .is_some(),
            nullish_coalescing_operator: self
                .options
                .get_plugin("transform-nullish-coalescing-operator")
                .map(get_options::<NullishCoalescingOperatorOptions>),
            optional_catch_binding: self
                .options
                .get_plugin("transform-optional-catch-binding")
                .is_some(),
            exponentiation_operator: self
                .options
                .get_plugin("transform-exponentiation-operator")
                .is_some(),
            shorthand_properties: self
                .options
                .get_plugin("transform-shorthand-properties")
                .is_some(),
            sticky_regex: self.options.get_plugin("transform-sticky-regex").is_some(),
        }
    }

    fn skip_test_case(&self) -> bool {
        // Legacy decorators is not supported by the parser
        if self
            .options
            .get_plugin("syntax-decorators")
            .flatten()
            .as_ref()
            .and_then(|o| o.as_object())
            .and_then(|o| o.get("version"))
            .is_some_and(|s| s == "legacy")
        {
            return true;
        }
        false
    }

    /// Test conformance by comparing the parsed babel code and transformed code.
    fn test(&self, filter: Option<&str>) -> bool {
        let filtered = filter.is_some_and(|f| self.path.to_string_lossy().as_ref().contains(f));

        let output_path = self.path.parent().unwrap().read_dir().unwrap().find_map(|entry| {
            let path = entry.ok()?.path();
            let file_stem = path.file_stem()?;
            (file_stem == "output").then_some(path)
        });

        let allocator = Allocator::default();
        let input = fs::read_to_string(&self.path).unwrap();
        let source_type = SourceType::from_path(&self.path).unwrap();

        if filtered {
            println!("input_path: {:?}", &self.path);
            println!("output_path: {output_path:?}");
        }

        // Transform input.js
        let program = Parser::new(&allocator, &input, source_type).parse().program;
        let semantic = SemanticBuilder::new(&input, source_type).build(&program).semantic;
        let (symbols, scopes) = semantic.into_symbol_table_and_scope_tree();
        let symbols = Rc::new(RefCell::new(symbols));
        let scopes = Rc::new(RefCell::new(scopes));
        let program = allocator.alloc(program);
        Transformer::new(&allocator, source_type, &symbols, &scopes, self.transform_options())
            .build(program);
        let transformed_code = Codegen::<false>::new(input.len(), CodegenOptions).build(program);

        // Get output.js by using our codeg so code comparison can match.
        let output = output_path.and_then(|path| fs::read_to_string(path).ok()).map_or_else(
            || {
                // The transformation should be equal to input.js If output.js does not exist.
                let program = Parser::new(&allocator, &input, source_type).parse().program;
                Codegen::<false>::new(input.len(), CodegenOptions).build(&program)
            },
            |output| {
                // Get expected code by parsing the source text, so we can get the same code generated result.
                let program = Parser::new(&allocator, &output, source_type).parse().program;
                Codegen::<false>::new(output.len(), CodegenOptions).build(&program)
            },
        );

        let passed = transformed_code == output;
        if filtered {
            println!("Input:\n");
            println!("{input}\n");
            println!("Output:\n");
            println!("{output}\n");
            println!("Transformed:\n");
            println!("{transformed_code}\n");
            println!("Passed: {passed}");
        }
        passed
    }

    fn exec(&self) -> bool {
        let result = self.transform(&self.path);
        let target_path = self.write_to_test_files(&result);
        Self::run_test(target_path.file_name().unwrap().to_string_lossy().as_ref())
    }

    fn run_test(filename: &str) -> bool {
        let output = Command::new("bun")
            .args(["test", filename])
            .output()
            .expect("Try install bun: https://bun.sh/docs/installation");

        let content = if output.stderr.is_empty() { &output.stdout } else { &output.stderr };
        let content = String::from_utf8_lossy(content);

        content.contains("1 pass")
    }

    fn write_to_test_files(&self, content: &str) -> PathBuf {
        let target_root = project_root().join("tasks/transform_conformance/");
        let allocator = Allocator::default();

        let new_file_name: String =
            normalize_path(self.path.strip_prefix(&project_root()).unwrap())
                .split('/')
                .collect::<Vec<&str>>()
                .join("-");

        let mut target_path = target_root.join("fixtures").join(new_file_name);
        target_path.set_extension("test.js");
        let content = format!(
            r#"
                import {{expect, test}} from 'bun:test';
                test("exec", () => {{
                    {content}
                }})
            "#
        );
        fs::write(&target_path, content).unwrap();
        let source_text = fs::read_to_string(&target_path).unwrap();
        let source_type = SourceType::from_path(&target_path).unwrap();
        let transformed_program =
            Parser::new(&allocator, &source_text, source_type).parse().program;
        let result =
            Codegen::<false>::new(source_text.len(), CodegenOptions).build(&transformed_program);

        fs::write(&target_path, result).unwrap();

        target_path
    }

    fn transform(&self, path: &Path) -> String {
        let allocator = Allocator::default();
        let source_text = fs::read_to_string(path).unwrap();
        let source_type = SourceType::from_path(path).unwrap();
        let transformed_program =
            Parser::new(&allocator, &source_text, source_type).parse().program;

        let semantic =
            SemanticBuilder::new(&source_text, source_type).build(&transformed_program).semantic;
        let (symbols, scopes) = semantic.into_symbol_table_and_scope_tree();
        let symbols = Rc::new(RefCell::new(symbols));
        let scopes = Rc::new(RefCell::new(scopes));
        let transformed_program = allocator.alloc(transformed_program);

        Transformer::new(&allocator, source_type, &symbols, &scopes, self.transform_options())
            .build(transformed_program);
        Codegen::<false>::new(source_text.len(), CodegenOptions).build(transformed_program)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum TestCaseKind {
    Transform,
    Exec,
}

impl TestCaseKind {
    fn from_path(path: &Path) -> Option<Self> {
        // in `exec` directory
        if path.parent().is_some_and(|path| path.file_name().is_some_and(|n| n == "exec"))
            && path.extension().is_some_and(|ext| VALID_EXTENSIONS.contains(&ext.to_str().unwrap()))
        {
            return Some(Self::Exec);
        }
        // named `exec.[ext]`
        if path.file_stem().is_some_and(|name| name == "exec")
            && path.extension().is_some_and(|ext| VALID_EXTENSIONS.contains(&ext.to_str().unwrap()))
        {
            return Some(Self::Exec);
        }

        // named `input.[ext]``
        if path.file_stem().is_some_and(|name| name == "input")
            && path.extension().is_some_and(|ext| VALID_EXTENSIONS.contains(&ext.to_str().unwrap()))
        {
            return Some(Self::Transform);
        }

        None
    }
}

struct SnapshotOption {
    paths: IndexMap<String, Vec<TestCase>>,
    root: PathBuf,
    snap_file_name: PathBuf,
}
