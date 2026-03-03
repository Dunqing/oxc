use oxc_diagnostics::OxcDiagnostic;
use oxc_macros::declare_oxc_lint;
use oxc_span::Span;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::{
    context::LintContext,
    rule::{DefaultRuleConfig, Rule},
};

fn prefer_default_export_diagnostic(span: Span, target: Target) -> OxcDiagnostic {
    let msg = if target == Target::Single {
        "Prefer default export on a file with single export."
    } else {
        "Prefer default export to be present on every file that has export."
    };
    OxcDiagnostic::warn(msg).with_help("Prefer a default export").with_label(span)
}

#[derive(Debug, Default, PartialEq, Clone, Copy, Deserialize, Serialize, JsonSchema)]
#[serde(rename_all = "kebab-case")]
enum Target {
    #[default]
    Single,
    Any,
}

#[derive(Debug, Default, Clone, Deserialize, Serialize, JsonSchema)]
#[serde(rename_all = "camelCase", default, deny_unknown_fields)]
pub struct PreferDefaultExport {
    /// Configuration option to specify the target type for preferring default exports.
    /// - `"single"`: Prefer default export when there is only one export in the module.
    /// - `"any"`: Prefer default export in any module that has exports.
    target: Target,
}

declare_oxc_lint!(
    /// ### What it does
    ///
    /// In exporting files, this rule checks if there is default export or not.
    ///
    /// ### Why is this bad?
    ///
    /// This rule exists to standardize module exports by preferring default exports
    /// when a module only has one export, enhancing readability, maintainability.
    ///
    /// ### Examples
    ///
    /// Examples of **incorrect** code for the `{ target: "single" }` option:
    /// ```js
    /// export const foo = 'foo';
    /// ```
    ///
    /// Examples of **correct** code for the `{ target: "single" }` option:
    /// ```js
    /// export const foo = 'foo';
    /// const bar = 'bar';
    /// export default bar;
    /// ```
    ///
    /// Examples of **incorrect** code for the `{ target: "any" }` option:
    /// ```js
    /// export const foo = 'foo';
    /// export const baz = 'baz';
    /// ```
    ///
    /// Examples of **correct** code for the `{ target: "any" }` option:
    /// ```js
    /// export default function bar() {};
    /// ```
    PreferDefaultExport,
    import,
    style,
    config = PreferDefaultExport,
);

impl Rule for PreferDefaultExport {
    fn from_configuration(value: Value) -> Result<Self, serde_json::error::Error> {
        serde_json::from_value::<DefaultRuleConfig<Self>>(value).map(DefaultRuleConfig::into_inner)
    }

    fn run_once(&self, ctx: &LintContext<'_>) {
        let Some(module) = ctx.current_module() else {
            return;
        };

        // If module has a default export, that's fine
        if module.named_exports.contains_key("default") {
            return;
        }

        // If there are star exports, skip
        if !module.star_export_entries.is_empty() {
            return;
        }

        let indirect_entries = &module.indirect_export_entries;

        // Collect local exports, filtering out type-only
        let local_exports: Vec<_> = module.named_exports.values().filter(|e| !e.is_type).collect();

        // Collect indirect exports, filtering out type-only
        let indirect_exports: Vec<_> = indirect_entries.iter().filter(|e| !e.is_type).collect();

        // If any export is type-only, skip (consistent with original behavior)
        if module.named_exports.values().any(|e| e.is_type)
            || indirect_entries.iter().any(|e| e.is_type)
        {
            return;
        }

        let total_exports = indirect_exports.len() + local_exports.len();

        if self.target == Target::Single {
            if total_exports == 1 {
                for entry in &indirect_exports {
                    ctx.diagnostic(prefer_default_export_diagnostic(entry.span, self.target));
                }
                for entry in &local_exports {
                    ctx.diagnostic(prefer_default_export_diagnostic(entry.span, self.target));
                }
            }
        } else {
            // Find the last export statement by span start position
            let last_span = indirect_exports
                .iter()
                .map(|e| e.span)
                .chain(local_exports.iter().map(|e| e.span))
                .max_by_key(|span| span.start);

            if let Some(span) = last_span {
                ctx.diagnostic(prefer_default_export_diagnostic(span, self.target));
            }
        }
    }
}

#[test]
fn test() {
    use crate::tester::Tester;
    use serde_json::json;

    let pass = vec![
        ("export default a", None),
        ("export const { foo, bar } = item;", None),
        ("export const { foo, bar: baz } = item;", None),
        ("export const { foo: { bar, baz } } = item;", None),
        ("export const [a, b] = item;", None),
        (
            "
                let item;
                export const foo = item;
                export { item };
            ",
            None,
        ),
        (
            "
                let foo;
                export { foo as default }
            ",
            None,
        ),
        ("export const [CounterProvider,, withCounter] = func()", None),
        ("export * from './foo'", None),
        ("export default function bar() {};", None),
        (
            "
                export const foo = 'foo';
                export const bar = 'bar';
            ",
            None,
        ),
        (
            "
                export const foo = 'foo';
                export function bar() {};
            ",
            None,
        ),
        (
            "
                export const foo = 'foo';
                export default bar;
            ",
            None,
        ),
        (
            "
                let foo, bar;
                export { foo, bar }
            ",
            None,
        ),
        (
            "
                export default a;
                export * from './foo'
                export const a = 3;
                export { MemoryValue } from './Memory'
            ",
            None,
        ),
        ("export type foo = string", None),
        ("import * as foo from './foo'", None),
        ("export type UserId = number;", None),
        ("export { a, b } from 'foo.js'", None),
        ("let foo; export { foo as 'default' };", None),
        ("export default function bar() {};", Some(json!([{"target": "any"}]))),
        (
            "
                export const foo = 'foo';
                export const bar = 'bar';
                export default 42;
            ",
            Some(json!([{"target": "any"}])),
        ),
        ("export default a = 2;", Some(json!([{"target": "any"}]))),
        (
            "
                export const a = 2;
                export default function foo() {};
            ",
            Some(json!([{"target": "any"}])),
        ),
        (
            "
                export const a = 5;
                export function bar(){};
                let foo;
                export { foo as default }
            ",
            Some(json!([{"target": "any"}])),
        ),
        ("export * from './foo';", Some(json!([{"target": "any"}]))),
        ("import * as foo from './foo';", Some(json!([{"target": "any"}]))),
        ("const a = 5;", Some(json!([{"target": "any"}]))),
        (
            "export const a = 4; let foo; export { foo as 'default' };",
            Some(json!([{"target": "any"}])),
        ),
        (
            "
                export type foo = string;
                export type bar = number;
            ",
            None,
        ),
        (
            "
                export const a = 2;
                export { a } from './c';
                export type bar = number;
            ",
            Some(json!([{"target": "any"}])),
        ),
    ];

    let fail = vec![
        ("export const a = 3", None),
        ("export { MemoryValue } from './Memory'", None),
        ("export const a = 3", Some(json!([{"target": "any"}]))),
        ("export function bar() {}", None),
        ("export const foo = 'foo';", None),
        ("const foo = 'foo'; export { foo };", None),
        ("export const { foo } = { foo: 'bar' };", None),
        ("export const { foo: { bar } } = { foo: { bar: 'baz' } };", None),
        ("export const [a] = ['foo']", None),
        (
            "
                export const foo = 'foo'
                export const bar = 'bar';
            ",
            Some(json!([{"target": "any"}])),
        ),
        (
            "
                export const foo = 'foo';
                export function bar() {};
            ",
            Some(json!([{"target": "any"}])),
        ),
        (
            "
                let foo, bar;
                export { foo, bar }
            ",
            Some(json!([{"target": "any"}])),
        ),
        (
            "
                let item;
                export const foo = item;
                export { item };
            ",
            Some(json!([{"target": "any"}])),
        ),
        ("export { a, b } from 'foo.js'", Some(json!([{"target": "any"}]))),
        (
            "
                const foo = 'foo';
                export { foo };
            ",
            Some(json!([{"target": "any"}])),
        ),
        ("export const { foo } = { foo: 'bar' };", Some(json!([{"target": "any"}]))),
        (
            "export const { foo: { bar } } = { foo: { bar: 'baz' } };",
            Some(json!([{"target": "any"}])),
        ),
        (
            "
                export const b = 3;
                export const r = 3;
                export { last } from './Memory'
            ",
            Some(json!([{"target": "any"}])),
        ),
    ];

    Tester::new(PreferDefaultExport::NAME, PreferDefaultExport::PLUGIN, pass, fail)
        .with_import_plugin(true)
        .change_rule_path("index.ts")
        .test_and_snapshot();
}
