use std::borrow::Cow;

use oxc_ast::ast::ImportDeclarationSpecifier;
use oxc_diagnostics::{LabeledSpan, OxcDiagnostic};
use oxc_macros::declare_oxc_lint;
use oxc_span::Span;
use rustc_hash::FxHashMap;
use schemars::JsonSchema;
use serde::Deserialize;

use crate::{
    context::LintContext,
    rule::{DefaultRuleConfig, Rule},
};

// <https://github.com/import-js/eslint-plugin-import/blob/v2.29.1/docs/rules/no-duplicates.md>
#[derive(Debug, Default, Clone, JsonSchema, Deserialize)]
#[serde(rename_all = "camelCase", default, deny_unknown_fields)]
pub struct NoDuplicates {
    /// When set to `true`, prefer inline type imports instead of separate type import
    /// statements for TypeScript code.
    ///
    /// Examples of **correct** code with this option set to `true`:
    /// ```typescript
    /// import { Foo, type Bar } from './module';
    /// ```
    #[serde(alias = "prefer-inline")]
    prefer_inline: bool,

    /// When set to `true`, the rule will consider the query string part of the import path
    /// when determining if imports are duplicates. This is useful when using loaders like
    /// webpack that use query strings to configure how a module should be loaded.
    ///
    /// Examples of **correct** code with this option set to `true`:
    /// ```javascript
    /// import x from './bar?optionX';
    /// import y from './bar?optionY';
    /// ```
    consider_query_string: bool,
}

declare_oxc_lint!(
    /// ### What it does
    ///
    /// Reports if a resolved path is imported more than once in the same module.
    /// This helps avoid unnecessary duplicate imports and keeps the code clean.
    ///
    /// ### Why is this bad?
    ///
    /// Importing the same module multiple times can lead to redundancy and
    /// unnecessary complexity. It also affects maintainability, as it might
    /// confuse developers and result in inconsistent usage of imports across the code.
    ///
    /// ### Examples
    ///
    /// Examples of **incorrect** code for this rule:
    /// ```javascript
    /// import { foo } from './module';
    /// import { bar } from './module';
    ///
    /// import a from './module';
    /// import { b } from './module';
    /// ```
    ///
    /// Examples of **correct** code for this rule:
    /// ```typescript
    /// import { foo, bar } from './module';
    ///
    /// import * as a from 'foo'; // separate statements for namespace imports
    /// import { b } from 'foo';
    ///
    /// import { c } from 'foo';      // separate type imports, unless
    /// import type { d } from 'foo'; // `prefer-inline` is true
    /// ```
    NoDuplicates,
    import,
    style,
    // This rule intentionally does not have a fixer, as `eslint/no-duplicate-imports` does the
    // same thing as this rule and is also pending a fixer.
    none,
    config = NoDuplicates,
);

/// Classification of an import statement for duplicate detection.
/// 0 = value (or type named when prefer_inline),
/// 1 = type named (when !prefer_inline),
/// 2 = namespace (type or value),
/// 3 = type default
#[derive(Clone, Copy)]
enum ImportKind {
    Value = 0,
    TypeNamed = 1,
    Namespace = 2,
    TypeDefault = 3,
}

impl Rule for NoDuplicates {
    fn from_configuration(value: serde_json::Value) -> Result<Self, serde_json::error::Error> {
        serde_json::from_value::<DefaultRuleConfig<Self>>(value).map(DefaultRuleConfig::into_inner)
    }

    fn run_once(&self, ctx: &LintContext<'_>) {
        let module = ctx.current_module();

        // Build a specifier → resolved_path map from the module graph.
        let resolved_paths: FxHashMap<&str, String> = module
            .map(|m| {
                m.import_records
                    .iter()
                    .filter_map(|record| {
                        let resolved = record
                            .resolved_module
                            .and_then(|idx| ctx.resolve_module(idx))
                            .map(|m| m.path.to_string_lossy().to_string())?;
                        Some((record.specifier.as_str(), resolved))
                    })
                    .collect()
            })
            .unwrap_or_default();

        // Collect import info from AST: (grouping_key, source_span, import_kind)
        let mut imports_by_group: FxHashMap<String, FxHashMap<u8, Vec<Span>>> =
            FxHashMap::default();

        for node in ctx.nodes().iter() {
            let oxc_ast::AstKind::ImportDeclaration(import_decl) = node.kind() else { continue };
            // Only consider top-level imports (parent is Program)
            if !matches!(ctx.nodes().parent_kind(node.id()), oxc_ast::AstKind::Program(_)) {
                continue;
            }

            let source = &import_decl.source;
            let specifier = source.value.as_str();
            let source_span = source.span;

            // Resolve the grouping key
            let resolved_absolute_path =
                resolved_paths.get(specifier).cloned().unwrap_or_else(|| specifier.to_string());

            let grouping_key = if self.consider_query_string {
                if let Some(query_pos) = specifier.find('?') {
                    format!("{}{}", resolved_absolute_path, &specifier[query_pos..])
                } else {
                    resolved_absolute_path
                }
            } else {
                resolved_absolute_path
            };

            // Classify the import
            let kind = classify_import(import_decl, self.prefer_inline);

            let group = imports_by_group.entry(grouping_key).or_default();
            // Use flags to only add one entry per kind per import statement
            let mut flags = [true; 4];
            for k in kind {
                let key = k as u8;
                if flags[key as usize] {
                    flags[key as usize] = false;
                    group.entry(key).or_default().push(source_span);
                }
            }
        }

        // Check for duplicates in each group
        for kinds_map in imports_by_group.values() {
            for i in 0..4u8 {
                check_duplicates(ctx, kinds_map.get(&i));
            }
        }
    }
}

/// Classify an import declaration into one or more ImportKind categories.
///
/// An import like `import { a, type b } from './foo'` produces both Value and TypeNamed.
fn classify_import(
    import_decl: &oxc_ast::ast::ImportDeclaration<'_>,
    prefer_inline: bool,
) -> Vec<ImportKind> {
    use oxc_ast::ast::ImportOrExportKind;

    let is_type_import = import_decl.import_kind == ImportOrExportKind::Type;

    let Some(specifiers) = &import_decl.specifiers else {
        // Side-effect import: `import './foo'`
        return vec![ImportKind::Value];
    };

    if specifiers.is_empty() {
        // `import {} from './foo'` — treat as value
        return vec![ImportKind::Value];
    }

    let mut kinds = Vec::new();

    for specifier in specifiers {
        let kind = match specifier {
            ImportDeclarationSpecifier::ImportNamespaceSpecifier(_) => {
                // `import * as ns` or `import type * as ns`
                ImportKind::Namespace
            }
            ImportDeclarationSpecifier::ImportDefaultSpecifier(_) => {
                if is_type_import {
                    ImportKind::TypeDefault
                } else {
                    ImportKind::Value
                }
            }
            ImportDeclarationSpecifier::ImportSpecifier(spec) => {
                let is_type_spec = is_type_import || spec.import_kind == ImportOrExportKind::Type;
                if is_type_spec {
                    if prefer_inline {
                        ImportKind::Value // merged with value when prefer_inline
                    } else {
                        ImportKind::TypeNamed
                    }
                } else {
                    ImportKind::Value
                }
            }
        };
        kinds.push(kind);
    }

    kinds
}

fn no_duplicates_diagnostic<I>(
    module_name: &str,
    first_import: Span,
    other_imports: I,
) -> OxcDiagnostic
where
    I: IntoIterator<Item = Span>,
{
    const MAX_MODULE_LEN: usize = 16;

    let message = if module_name.len() > MAX_MODULE_LEN {
        Cow::Borrowed("Modules should not be imported multiple times in the same file")
    } else {
        Cow::Owned(format!("Module '{module_name}' is imported more than once in this file"))
    };
    let labels = std::iter::once(first_import.primary_label("It is first imported here"))
        .chain(other_imports.into_iter().map(LabeledSpan::underline));

    OxcDiagnostic::warn(message)
        .with_labels(labels)
        .with_help("Merge these imports into a single import statement")
}

fn check_duplicates(ctx: &LintContext, requested_modules: Option<&Vec<Span>>) {
    if let Some(requested_modules) = requested_modules
        && requested_modules.len() > 1
    {
        let mut labels = requested_modules.iter().copied();
        let first = labels.next().unwrap(); // we know there is at least one
        let module_name = ctx.source_range(first).trim_matches('\'').trim_matches('"');
        ctx.diagnostic(no_duplicates_diagnostic(module_name, first, labels));
    }
}

#[test]
fn test() {
    use serde_json::json;

    use crate::tester::Tester;

    let pass = vec![
        (r#"import "./malformed.js""#, None),
        (r"import { x } from './foo'; import { y } from './bar'", None),
        (r#"import foo from "234artaf"; import { shoop } from "234q25ad""#, None),
        (r"import { x } from './foo'; import type { y } from './foo'", None),
        // #1107: Using different query strings that trigger different webpack loaders.
        // Test camelCase option
        (
            r"import x from './bar?optionX'; import y from './bar?optionY';",
            Some(json!([{ "considerQueryString": true }])),
        ),
        (
            r"import x from './foo'; import y from './bar';",
            Some(json!([{ "considerQueryString": true }])),
        ),
        // #1538: It is impossible to import namespace and other in one line, so allow this.
        (r"import * as ns from './foo'; import { y } from './foo'", None),
        (r"import { y } from './foo'; import * as ns from './foo'", None),
        // TypeScript
        (r"import type { x } from './foo'; import y from './foo'", None),
        (r"import type x from './foo'; import type y from './bar'", None),
        (r"import type {x} from './foo'; import type {y} from './bar'", None),
        (r"import type x from './foo'; import type {y} from './foo'", None),
        (
            r"import type {} from './module';
        import {} from './module2';",
            None,
        ),
        (
            r"import type { Identifier } from 'module';

        declare module 'module2' {
        import type { Identifier } from 'module';
        }

        declare module 'module3' {
        import type { Identifier } from 'module';
        }",
            None,
        ),
        (r"import { type x } from './foo'; import y from './foo'", None),
        (r"import { type x } from './foo'; import { y } from './foo'", None),
        (r"import { type x } from './foo'; import type y from 'foo'", None),
        (r"import { x } from './foo'; export { x } from './foo'", None),
        // for cases in https://github.com/import-js/eslint-plugin-import/issues/2750
        (r"import type * as something from './foo'; import type y from './foo';", None),
        (r"import type * as something from './foo'; import type { y } from './foo';", None),
        (r"import type y from './foo'; import type * as something from './foo';", None),
        (r"import type { y } from './foo'; import type * as something from './foo';", None),
        // type + import
        (r"import type * as something from './foo'; import y from './foo';", None),
        (r"import type * as something from './foo'; import { y } from './foo';", None),
        (r"import y from './foo'; import type * as something from './foo';", None),
        (r"import { y } from './foo'; import type * as something from './foo';", None),
        (r"import { RouterModule, Routes } from '@angular/router';", None),
    ];

    let fail = vec![
        (r"import { x } from './foo'; import { y } from './foo'", None),
        (r"import {x} from './foo'; import {y} from './foo'; import { z } from './foo'", None),
        // #1107: Using different query strings without considerQueryString (default false)
        // These should be flagged as duplicates because query strings are ignored by default
        (r"import x from './bar.js?optionX'; import y from './bar?optionX';", None),
        (r"import x from './bar?optionX'; import y from './bar?optionY';", None),
        (r"import x from './bar?optionX'; import y from './bar.js?optionX';", None),
        // #1107: Using same query strings with considerQueryString: true
        // Same file + same query string = duplicate
        (
            r"import x from './bar?optionX'; import y from './bar.js?optionX';",
            Some(json!([{ "considerQueryString": true }])),
        ),
        // #86: duplicate unresolved modules should be flagged
        (r"import foo from 'non-existent'; import bar from 'non-existent';", None),
        (r"import type { x } from './foo'; import type { y } from './foo'", None),
        (r"import './foo'; import './foo'", None),
        (
            r"import { x, /* x */ } from './foo'; import {//y
        y//y2
        } from './foo'",
            None,
        ),
        (r"import {x} from './foo'; import {} from './foo'", None),
        (r"import {a} from './foo'; import { a } from './foo'", None),
        (
            r"import {a,b} from './foo'; import { b, c } from './foo'; import {b,c,d} from './foo'",
            None,
        ),
        (r"import {a} from './foo'; import { a/*,b*/ } from './foo'", None),
        (r"import {a} from './foo'; import { a } from './foo'", None),
        (
            r"import {a,b} from './foo'; import { b, c } from './foo'; import {b,c,d} from './foo'",
            None,
        ),
        (r"import {a} from './foo'; import { a/*,b*/ } from './foo'", None),
        (
            r"import {x} from './foo'; import {} from './foo'; import {/*c*/} from './foo'; import {y} from './foo'",
            None,
        ),
        (r"import { } from './foo'; import {x} from './foo'", None),
        (r"import './foo'; import {x} from './foo'", None),
        (r"import'./foo'; import {x} from './foo'", None),
        (
            r"import './foo'; import { /*x*/} from './foo'; import {//y
        } from './foo'; import {z} from './foo'",
            None,
        ),
        (r"import './foo'; import def, {x} from './foo'", None),
        (r"import './foo'; import def from './foo'", None),
        (r"import def from './foo'; import {x} from './foo'", None),
        (r"import {x} from './foo'; import def from './foo'", None),
        (r"import{x} from './foo'; import def from './foo'", None),
        (r"import {x} from './foo'; import def, {y} from './foo'", None),
        (r"import * as ns1 from './foo'; import * as ns2 from './foo'", None),
        (r"import * as ns from './foo'; import {x} from './foo'; import {y} from './foo'", None),
        (
            r"import {x} from './foo'; import * as ns from './foo'; import {y} from './foo'; import './foo'",
            None,
        ),
        (
            r"// some-tool-disable-next-line
            import {x} from './foo'
            import {//y
        y} from './foo'",
            None,
        ),
        (
            r"import {x} from './foo'
            // some-tool-disable-next-line
            import {y} from './foo'",
            None,
        ),
        (
            r"import {x} from './foo' // some-tool-disable-line
            import {y} from './foo'",
            None,
        ),
        (
            r"import {x} from './foo'
            import {y} from './foo' // some-tool-disable-line",
            None,
        ),
        (
            r"import {x} from './foo'
            /* comment */ import {y} from './foo'",
            None,
        ),
        (
            r"import {x} from './foo'
            import {y} from './foo' /* comment
            multiline */",
            None,
        ),
        (
            r"import {x} from './foo'
        import {y} from './foo'
        // some-tool-disable-next-line",
            None,
        ),
        (
            r"import {x} from './foo'
        // comment

        import {y} from './foo'",
            None,
        ),
        (
            r"import {x} from './foo'
            import/* comment */{y} from './foo'",
            None,
        ),
        (
            r"import {x} from './foo'
            import/* comment */'./foo'",
            None,
        ),
        (
            r"import {x} from './foo'
            import{y}/* comment */from './foo'",
            None,
        ),
        (
            r"import {x} from './foo'
            import{y}from/* comment */'./foo'",
            None,
        ),
        (
            r"import {x} from
            // some-tool-disable-next-line
            './foo'
            import {y} from './foo'",
            None,
        ),
        (
            r"import { Foo } from './foo';
        import { Bar } from './foo';
        export const value = {}",
            None,
        ),
        (
            r"import { Foo } from './foo';
        import Bar from './foo';
        export const value = {}",
            None,
        ),
        (
            r"import {
              DEFAULT_FILTER_KEYS,
              BULK_DISABLED,
            } from '../constants';
            import React from 'react';
            import {
              BULK_ACTIONS_ENABLED
            } from '../constants';

            const TestComponent = () => {
              return <div>
              </div>;
            }

            export default TestComponent;",
            None,
        ),
        (
            r"import {A1,} from 'foo';
            import {B1,} from 'foo';
            import {C1,} from 'foo';

            import {
            A2,
            } from 'bar';
            import {
            B2,
            } from 'bar';
            import {
            C2,
            } from 'bar';",
            None,
        ),
        // TypeScript
        (r"import type x from './foo'; import type y from './foo'", None),
        (r"import type x from './foo'; import type x from './foo'", None),
        (r"import type {x} from './foo'; import type {y} from './foo'", None),
        // prefer-inline: false (default) - inline type and type imports are in same category
        (r"import {type x} from './foo'; import type {y} from './foo'", None),
        // prefer-inline: true - inline type and type imports are in same category
        (
            r"import {type x} from 'foo'; import type {y} from 'foo'",
            Some(json!([{ "prefer-inline": true }])),
        ),
        // prefer-inline: true - swapped order (type import first, then inline type)
        (
            r"import type {x} from 'foo'; import {type y} from 'foo'",
            Some(json!([{ "prefer-inline": true }])),
        ),
        // prefer-inline: false (default)
        (r"import {type x} from 'foo'; import type {y} from 'foo'", None),
        // prefer-inline: true - both inline type imports
        (
            r"import {type x} from './foo'; import {type y} from './foo'",
            Some(json!([{ "prefer-inline": true }])),
        ),
        // prefer-inline: false (default) - both inline type imports
        (r"import {type x} from './foo'; import {type y} from './foo'", None),
        (r"import {AValue, type x, BValue} from './foo'; import {type y} from './foo'", None),
        // #2834 Detect duplicates across type and regular imports with prefer-inline: true
        // Test prefer-inline with camelCase (legacy)
        (
            r"import {AValue} from './foo'; import type {AType} from './foo'",
            Some(json!([{ "preferInline": true }])),
        ),
        // Test prefer-inline with kebab-case (primary, matches ESLint)
        (
            r"import {AValue} from './foo'; import type {AType} from './foo'",
            Some(json!([{ "prefer-inline": true }])),
        ),
    ];

    Tester::new(NoDuplicates::NAME, NoDuplicates::PLUGIN, pass, fail)
        .change_rule_path("index.ts")
        .with_import_plugin(true)
        .test_and_snapshot();
}
