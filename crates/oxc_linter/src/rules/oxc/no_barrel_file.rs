use crate::{
    context::LintContext,
    rule::{DefaultRuleConfig, Rule},
};
use oxc_diagnostics::{LabeledSpan, OxcDiagnostic};
use oxc_macros::declare_oxc_lint;
use oxc_span::Span;
use schemars::JsonSchema;
use serde::Deserialize;

fn no_barrel_file(total: usize, threshold: usize, labels: Vec<LabeledSpan>) -> OxcDiagnostic {
    OxcDiagnostic::warn(format!(
        "Barrel file detected, {total} modules are loaded which exceeds the threshold of {threshold}.",
    ))
    .with_help(format!("Consider importing directly from the specific modules instead of using `export *` or `import *`.\nLoading {total} modules may be slow for runtimes and bundlers."))
    .with_note("See: https://marvinh.dev/blog/speeding-up-javascript-ecosystem-part-7")
    .with_labels(labels)
}

#[derive(Debug, Clone, JsonSchema, Deserialize)]
#[serde(rename_all = "camelCase", default, deny_unknown_fields)]
pub struct NoBarrelFile {
    /// The maximum number of modules that can be re-exported via `export *`
    /// before the rule is triggered.
    threshold: usize,
}

impl Default for NoBarrelFile {
    fn default() -> Self {
        Self { threshold: 100 }
    }
}

declare_oxc_lint!(
    /// ### What it does
    ///
    /// Disallow the use of barrel files where the file contains `export *` statements,
    /// and the total number of modules exceed a threshold.
    ///
    /// The default threshold is 100.
    ///
    /// ### Why is this bad?
    ///
    /// Barrel files that re-export many modules can significantly slow down
    /// applications and bundlers. When a barrel file exports a large number of
    /// modules, importing from it forces the runtime or bundler to process all
    /// the exported modules, even if only a few are actually used. This leads
    /// to slower startup times and larger bundle sizes.
    ///
    /// References:
    ///
    /// * <https://github.com/thepassle/eslint-plugin-barrel-files>
    /// * <https://marvinh.dev/blog/speeding-up-javascript-ecosystem-part-7>
    ///
    /// ### Examples
    ///
    /// Invalid:
    ///
    /// ```javascript
    /// export * from 'foo'; // where `foo` loads a subtree of 100 modules
    /// import * as ns from 'foo'; // where `foo` loads a subtree of 100 modules
    /// ```
    ///
    /// Valid:
    ///
    /// ```javascript
    /// export { foo } from 'foo';
    /// ```
    NoBarrelFile,
    oxc,
    restriction,
    config = NoBarrelFile,
);

impl Rule for NoBarrelFile {
    fn from_configuration(value: serde_json::Value) -> Result<Self, serde_json::error::Error> {
        serde_json::from_value::<DefaultRuleConfig<Self>>(value).map(DefaultRuleConfig::into_inner)
    }

    fn run_once(&self, ctx: &LintContext<'_>) {
        let Some(module) = ctx.current_module() else {
            return;
        };
        self.run_with_module_graph(ctx, module);
    }
}

impl NoBarrelFile {
    fn run_with_module_graph(
        &self,
        ctx: &LintContext<'_>,
        module: &oxc_module_graph::NormalModule,
    ) {
        if !module.has_module_syntax {
            return;
        }

        // Build a lookup from specifier to specifier_span from import_records,
        // so we can label the specifier string instead of the whole statement.
        let specifier_spans: rustc_hash::FxHashMap<&str, Span> = module
            .import_records
            .iter()
            .map(|r| (r.specifier.as_str(), r.specifier_span))
            .collect();

        // Collect star export entries that re-export all (`export * from '...'`)
        let star_requests: Vec<_> = module
            .star_export_entries
            .iter()
            .map(|entry| {
                let span = specifier_spans
                    .get(entry.module_request.as_str())
                    .copied()
                    .unwrap_or(entry.span);
                (span, entry.resolved_module)
            })
            .collect();

        // Collect indirect export entries that re-export all (`export * as ns from '...'`)
        let indirect_all_requests: Vec<_> = module
            .indirect_export_entries
            .iter()
            .filter(|entry| entry.imported_name == "*")
            .map(|entry| {
                let span = specifier_spans
                    .get(entry.module_request.as_str())
                    .copied()
                    .unwrap_or(entry.span);
                (span, entry.resolved_module)
            })
            .collect();

        let mut labels = vec![];
        let mut total: usize = 0;

        for (span, resolved_module) in star_requests.iter().chain(indirect_all_requests.iter()) {
            // the own module is counted as well
            total += 1;

            if let Some(idx) = resolved_module
                && let Some(remote_module) = ctx.resolve_module(*idx)
            {
                let count = count_modules_recursive(ctx, remote_module);
                if count > 0 {
                    total += count;
                    labels.push(
                        span.label(format!("{count} module{}", if count > 1 { "s" } else { "" })),
                    );
                }
            }
        }

        let threshold = self.threshold;
        if total > threshold {
            if labels.is_empty() {
                labels.push(Span::new(0, 0).label("File defined here."));
            }

            ctx.diagnostic(no_barrel_file(total, threshold, labels));
        }
    }
}

/// Recursively count the number of modules loaded by a given module via the
/// new module graph API.
fn count_modules_recursive(
    ctx: &LintContext<'_>,
    module: &oxc_module_graph::NormalModule,
) -> usize {
    let mut visited = rustc_hash::FxHashSet::default();
    let mut stack = vec![module.idx];
    let mut count: usize = 0;

    while let Some(idx) = stack.pop() {
        if !visited.insert(idx) {
            continue;
        }
        if let Some(m) = ctx.resolve_module(idx) {
            for record in &m.import_records {
                if let Some(resolved) = record.resolved_module {
                    count += 1;
                    stack.push(resolved);
                }
            }
        }
    }
    count
}

#[test]
fn test() {
    use crate::tester::Tester;

    let pass = vec![
        (r#"export type * from "foo";"#, None),
        (r#"export type { foo } from "foo";"#, None),
        (r#"export type * from "foo"; export type { bar } from "bar";"#, None),
        (r#"import { foo, bar, baz } from "../import/export-star/models";"#, None),
        (
            r#"import boo from "foo";
                    const test = 0;"#,
            Some(serde_json::json!([{"threshold": 0}])),
        ),
        (r"export const test = 0;", Some(serde_json::json!([{"threshold": 0}]))),
        (
            r"export const test = 0;
            export const other = 1;",
            Some(serde_json::json!([{"threshold": 0}])),
        ),
    ];

    let settings = Some(serde_json::json!([{"threshold": 1}]));

    let fail = vec![(
        r#"
export * from "./deep/a.js";
export * from "./deep/b.js";
export * from "./deep/c.js";
export * from "./deep/d.js";
        "#,
        settings,
    )];

    Tester::new(NoBarrelFile::NAME, NoBarrelFile::PLUGIN, pass, fail)
        .change_rule_path("index.ts")
        .with_import_plugin(true)
        .test_and_snapshot();
}
