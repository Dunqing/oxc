use oxc_diagnostics::OxcDiagnostic;
use oxc_macros::declare_oxc_lint;
use oxc_span::Span;

use crate::{context::LintContext, rule::Rule};

fn no_named_default_diagnostic(span: Span) -> OxcDiagnostic {
    OxcDiagnostic::warn("Replace default import with named import.")
        .with_help("Forbid named default exports.")
        .with_label(span)
}

#[derive(Debug, Default, Clone)]
pub struct NoNamedDefault;

declare_oxc_lint!(
    /// ### What it does
    ///
    /// Reports use of a default export as a locally named import.
    ///
    /// ### Why is this bad?
    ///
    /// Rationale: the syntax exists to import default exports expressively, let's use it.
    ///
    /// ### Examples
    ///
    /// Examples of **incorrect** code for this rule:
    /// ```js
    /// // message: Using exported name 'bar' as identifier for default export.
    /// import { default as foo } from './foo.js';
    /// import { default as foo, bar } from './foo.js';
    /// ```
    ///
    /// Examples of **correct** code for this rule:
    /// ```js
    /// import foo from './foo.js';
    /// import foo, { bar } from './foo.js';
    /// ```
    NoNamedDefault,
    import,
    style,
);

impl Rule for NoNamedDefault {
    fn run_once(&self, ctx: &LintContext) {
        let Some(module) = ctx.current_module() else {
            return;
        };

        for import in module.named_imports.values() {
            // A named import of "default" that is NOT a default import syntax
            // i.e., `import { default as foo }` not `import foo`
            if import.imported_name == "default" && !import.is_default_import && !import.is_type {
                ctx.diagnostic(no_named_default_diagnostic(import.span));
            }
        }
    }
}

#[test]
fn test() {
    use crate::tester::Tester;

    let pass = vec![
        r#"import bar from "./bar";"#,
        r#"import bar, { foo } from "./bar";"#,
        r#"import { type default as Foo } from "./bar";"#,
    ];

    let unicorn_pass = vec![
        r#"import named from "foo";"#,
        r#"import "foo";"#,
        r#"import * as named from "foo";"#,
        r#"export {foo as default} from "foo";"#,
        r#"export * as default from "foo";"#,
    ];

    let fail = vec![
        r#"import { default as bar } from "./bar";"#,
        r#"import { foo, default as bar } from "./bar";"#,
        r#"import { "default" as bar } from "./bar";"#,
    ];

    let unicorn_fail = vec![
        r#"import {default as named} from "foo";"#,
        r#"import {default as named,} from "foo";"#,
        r#"import {default as named, bar} from "foo";"#,
        r#"import {default as named, bar,} from "foo";"#,
        r#"import defaultExport, {default as named} from "foo";"#,
        r#"import defaultExport, {default as named,} from "foo";"#,
        r#"import defaultExport, {default as named, bar} from "foo";"#,
        r#"import defaultExport, {default as named, bar,} from "foo";"#,
        r#"import{default as named}from"foo";"#,
        r#"import {default as named}from"foo";"#,
        r#"import{default as named} from"foo";"#,
        r#"import{default as named,}from"foo";"#,
        r#"import/*comment*/{default as named}from"foo";"#,
        r#"import /*comment*/{default as named}from"foo";"#,
        r#"import{default as named}/*comment*/from"foo";"#,
        r#"import defaultExport,{default as named}from "foo";"#,
        r#"import defaultExport, {default as named} from "foo" with {type: "json"};"#,
        r#"import defaultExport, {default as named} from "foo" with {type: "json"}"#,
        r#"import {default as named1, default as named2,} from "foo";"#,
    ];

    Tester::new(
        NoNamedDefault::NAME,
        NoNamedDefault::PLUGIN,
        pass.into_iter().chain(unicorn_pass).collect(),
        fail.into_iter().chain(unicorn_fail).collect(),
    )
    .with_import_plugin(true)
    .change_rule_path("index.ts")
    .test_and_snapshot();
}
