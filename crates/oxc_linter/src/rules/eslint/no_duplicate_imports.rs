use rustc_hash::FxHashMap;

use oxc_ast::AstKind;
use oxc_ast::ast::{ImportDeclarationSpecifier, ImportOrExportKind};
use oxc_diagnostics::OxcDiagnostic;
use oxc_macros::declare_oxc_lint;
use oxc_span::Span;
use schemars::JsonSchema;
use serde::Deserialize;

use crate::{
    context::LintContext,
    rule::{DefaultRuleConfig, Rule},
};

fn no_duplicate_imports_diagnostic(
    module_name: &str,
    span: Span,
    previous_span: Span,
) -> OxcDiagnostic {
    OxcDiagnostic::warn(format!("'{module_name}' import is duplicated"))
        .with_help("Merge the duplicated import into a single import statement")
        .with_labels([
            span.label("This import is duplicated"),
            previous_span.label("Can be merged with this import"),
        ])
}

fn no_duplicate_exports_diagnostic(
    module_name: &str,
    span: Span,
    previous_span: Span,
) -> OxcDiagnostic {
    OxcDiagnostic::warn(format!("'{module_name}' export is duplicated"))
        .with_help("Merge the duplicated exports into a single export statement")
        .with_labels([
            span.label("This export is duplicated"),
            previous_span.label("Can be merged with this"),
        ])
}

#[derive(Debug, Default, Clone, JsonSchema, Deserialize)]
#[serde(rename_all = "camelCase", default, deny_unknown_fields)]
pub struct NoDuplicateImports {
    /// When `true` this rule will also look at exports to see if there is both a re-export of a
    /// module as in `export ... from 'module'` and also a standard import statement for the same
    /// module. This would count as a rule violation because there are in a sense two statements
    /// importing from the same module.
    ///
    /// Examples of **incorrect** code when `includeExports` is set to `true`:
    /// ```js
    /// import { merge } from 'module';
    ///
    /// export { find } from 'module'; // re-export which is an import and an export.
    /// ```
    ///
    /// Examples of **correct** code when `includeExports` is set to `true`:
    ///
    /// If re-exporting from an imported module, you should add the imports to the
    /// `import` statement, and export that directly, not use `export ... from`.
    /// ```js
    /// import { merge } from "lodash-es";
    /// export { merge as lodashMerge }
    /// ```
    ///
    /// ```js
    /// import { merge, find } from 'module';
    ///
    /// // cannot be merged with the above import
    /// export * as something from 'module';
    ///
    /// // cannot be written differently
    /// export * from 'module';
    /// ```
    include_exports: bool,
    /// When `true`, imports with only type specifiers (inline types or type imports) are
    /// considered separate from imports with value specifiers, so they can be imported from the
    /// same module on separate import statements.
    ///
    /// Examples of **correct** code when `allowSeparateTypeImports` is set to `true`:
    /// ```js
    /// import { foo } from "module";
    /// import type { Bar } from "module";
    /// ```
    ///
    /// ```js
    /// import { type Foo } from "module";
    /// import type { Bar } from "module";
    /// ```
    allow_separate_type_imports: bool,
}

declare_oxc_lint!(
    /// ### What it does
    ///
    /// Disallow duplicate module imports.
    ///
    /// ### Why is this bad?
    ///
    /// Using a single import statement per module will make the code clearer because you can see
    /// everything being imported from that module on one line.
    ///
    /// ### Examples
    ///
    /// Examples of **incorrect** code for this rule:
    ///
    /// In the following example the module import on line 1 is repeated on line 3. These can be
    /// combined to make the list of imports more succinct.
    /// ```js
    /// import { merge } from 'module';
    /// import something from 'another-module';
    /// import { find } from 'module';
    /// ```
    ///
    /// Examples of **correct** code for this rule:
    /// ```js
    /// import { merge, find } from 'module';
    /// import something from 'another-module';
    /// ```
    NoDuplicateImports,
    eslint,
    style,
    pending,
    config = NoDuplicateImports,
);

#[derive(Debug, Clone, PartialEq)]
enum ImportType {
    Named,
    Default,
    Namespace,
    SideEffect,
    AllButDefault,
}

#[derive(Debug, Clone, PartialEq)]
enum ModuleType {
    Import,
    Export,
}

impl Rule for NoDuplicateImports {
    fn from_configuration(value: serde_json::Value) -> Result<Self, serde_json::error::Error> {
        serde_json::from_value::<DefaultRuleConfig<Self>>(value).map(DefaultRuleConfig::into_inner)
    }

    fn run_once(&self, ctx: &LintContext) {
        // Use AST walking to get per-statement information, since the module graph
        // deduplicates import records per specifier and loses statement boundaries.
        let mut import_map: FxHashMap<&str, Vec<(ImportType, Span, ModuleType, bool)>> =
            FxHashMap::default();
        let mut side_effect_import_map: FxHashMap<&str, Vec<Span>> = FxHashMap::default();
        let mut has_named_imports = false;

        // Phase 1: Process all import declarations
        for node in ctx.nodes().iter() {
            if !matches!(ctx.nodes().parent_kind(node.id()), AstKind::Program(_)) {
                continue;
            }
            let AstKind::ImportDeclaration(import_decl) = node.kind() else {
                continue;
            };

            let source = import_decl.source.value.as_str();
            let source_span = import_decl.source.span;
            let is_type_import = import_decl.import_kind == ImportOrExportKind::Type;

            let Some(specifiers) = &import_decl.specifiers else {
                // Side-effect import: `import 'module'`
                side_effect_import_map.entry(source).or_default().push(source_span);
                continue;
            };

            if specifiers.is_empty() {
                // Empty import: `import {} from 'module'`
                side_effect_import_map.entry(source).or_default().push(source_span);
                continue;
            }

            has_named_imports = true;

            // Get the first specifier type to check for duplicates (matches old behavior)
            let first_type = classify_specifier(&specifiers[0]);
            let is_first_specifier_type =
                is_type_import || is_specifier_inline_type(&specifiers[0]);

            // Check for duplicates against existing imports
            if let Some(existing) = import_map.get(source)
                && can_merge_imports(
                    &first_type,
                    existing,
                    self.allow_separate_type_imports,
                    is_type_import,
                    is_first_specifier_type,
                )
            {
                ctx.diagnostic(no_duplicate_imports_diagnostic(
                    source,
                    source_span,
                    existing.first().unwrap().1,
                ));
                continue;
            }

            // Add all specifier types from this statement to the map
            for specifier in specifiers {
                let import_type = classify_specifier(specifier);
                import_map.entry(source).or_default().push((
                    import_type,
                    source_span,
                    ModuleType::Import,
                    is_type_import,
                ));
            }
        }

        // Phase 2: Report duplicate side-effect imports (only when no named imports exist)
        if !has_named_imports {
            for (source, spans) in &side_effect_import_map {
                let mut iter = spans.iter();
                if let Some(first_span) = iter.next() {
                    for span in iter {
                        ctx.diagnostic(no_duplicate_imports_diagnostic(
                            source,
                            *span,
                            *first_span,
                        ));
                    }
                }
            }
        }

        if !self.include_exports {
            return;
        }

        // Phase 3a: Process star exports (ExportAllDeclaration) first
        for node in ctx.nodes().iter() {
            if !matches!(ctx.nodes().parent_kind(node.id()), AstKind::Program(_)) {
                continue;
            }
            let AstKind::ExportAllDeclaration(export_decl) = node.kind() else {
                continue;
            };

            let source = export_decl.source.value.as_str();
            let span = export_decl.span;

            if export_decl.exported.is_none() {
                // `export * from 'x'` → AllButDefault
                if let Some(existing) = import_map.get(source)
                    && existing
                        .iter()
                        .any(|(t, _, _, _)| matches!(t, ImportType::AllButDefault))
                {
                    ctx.diagnostic(no_duplicate_exports_diagnostic(
                        source,
                        span,
                        existing.first().unwrap().1,
                    ));
                    continue;
                }
                if let Some(existing) = side_effect_import_map.get(source) {
                    ctx.diagnostic(no_duplicate_exports_diagnostic(
                        source,
                        span,
                        *existing.first().unwrap(),
                    ));
                    continue;
                }
                import_map.entry(source).or_default().push((
                    ImportType::AllButDefault,
                    span,
                    ModuleType::Export,
                    false,
                ));
            } else {
                // `export * as ns from 'x'` → namespace re-export
                if let Some(existing) = import_map.get(source)
                    && existing
                        .iter()
                        .any(|(t, _, _, _)| matches!(t, ImportType::Default | ImportType::Namespace))
                {
                    ctx.diagnostic(no_duplicate_exports_diagnostic(
                        source,
                        span,
                        existing.first().unwrap().1,
                    ));
                }
                // Don't add to import_map (matches old behavior)
            }
        }

        // Phase 3b: Process named re-exports (ExportNamedDeclaration with source)
        for node in ctx.nodes().iter() {
            if !matches!(ctx.nodes().parent_kind(node.id()), AstKind::Program(_)) {
                continue;
            }
            let AstKind::ExportNamedDeclaration(export_decl) = node.kind() else {
                continue;
            };
            let Some(source_lit) = &export_decl.source else {
                continue;
            };

            let source = source_lit.value.as_str();
            let source_span = source_lit.span;
            let is_statement_type_export =
                export_decl.export_kind == ImportOrExportKind::Type;

            // Process each specifier individually (matches old per-entry behavior)
            for specifier in &export_decl.specifiers {
                let specifier_span = specifier.span;
                let is_type = is_statement_type_export
                    || specifier.export_kind == ImportOrExportKind::Type;

                // Check against side-effect imports
                if let Some(existing) = side_effect_import_map.get(source) {
                    ctx.diagnostic(no_duplicate_exports_diagnostic(
                        source,
                        specifier_span,
                        *existing.first().unwrap(),
                    ));
                    continue;
                }

                if let Some(existing) = import_map.get(source) {
                    // Special case: type export with type imports (default + named)
                    if is_type {
                        let all_existing_are_type =
                            existing.iter().all(|(_, _, _, is_stmt_type)| *is_stmt_type);
                        if all_existing_are_type {
                            let has_default = existing
                                .iter()
                                .any(|(t, _, _, _)| matches!(t, ImportType::Default));
                            // Export is named, cannot merge with default type import
                            if has_default {
                                import_map.entry(source).or_default().push((
                                    ImportType::Named,
                                    specifier_span,
                                    ModuleType::Export,
                                    is_type,
                                ));
                                continue;
                            }
                        }
                    }

                    // Handle allowSeparateTypeImports for exports
                    if self.allow_separate_type_imports {
                        let imports_only: Vec<_> = existing
                            .iter()
                            .filter(|(_, _, module_type, _)| *module_type == ModuleType::Import)
                            .collect();

                        if !imports_only.is_empty() {
                            if is_statement_type_export && is_type {
                                // Statement-level type export: check if there are value imports
                                let has_value_imports = imports_only
                                    .iter()
                                    .any(|(_, _, _, is_stmt_type)| !is_stmt_type);
                                if has_value_imports {
                                    import_map.entry(source).or_default().push((
                                        ImportType::Named,
                                        specifier_span,
                                        ModuleType::Export,
                                        is_statement_type_export,
                                    ));
                                    continue;
                                }
                            } else if !is_statement_type_export {
                                // Value/inline type export: check if all imports are statement-level type
                                let all_imports_are_statement_type = imports_only
                                    .iter()
                                    .all(|(_, _, _, is_stmt_type)| *is_stmt_type);
                                if all_imports_are_statement_type {
                                    import_map.entry(source).or_default().push((
                                        ImportType::Named,
                                        specifier_span,
                                        ModuleType::Export,
                                        is_statement_type_export,
                                    ));
                                    continue;
                                }
                            }
                        }
                    }

                    // Main duplicate check
                    if existing.iter().any(|(t, existing_span, module_type, _)| {
                        // Named import/export from different statement
                        if matches!(t, ImportType::Named)
                            && source_span != *existing_span
                        {
                            return true;
                        }
                        (matches!(
                            t,
                            ImportType::Named | ImportType::SideEffect | ImportType::Default
                        ) && *module_type == ModuleType::Export)
                            || (matches!(t, ImportType::Default)
                                && *module_type == ModuleType::Import)
                    }) {
                        ctx.diagnostic(no_duplicate_exports_diagnostic(
                            source,
                            specifier_span,
                            existing.first().unwrap().1,
                        ));
                        continue;
                    }
                }

                import_map.entry(source).or_default().push((
                    ImportType::Named,
                    specifier_span,
                    ModuleType::Export,
                    is_statement_type_export,
                ));
            }
        }
    }
}

fn classify_specifier(specifier: &ImportDeclarationSpecifier) -> ImportType {
    match specifier {
        ImportDeclarationSpecifier::ImportNamespaceSpecifier(_) => ImportType::Namespace,
        ImportDeclarationSpecifier::ImportDefaultSpecifier(_) => ImportType::Default,
        ImportDeclarationSpecifier::ImportSpecifier(_) => ImportType::Named,
    }
}

fn is_specifier_inline_type(specifier: &ImportDeclarationSpecifier) -> bool {
    matches!(
        specifier,
        ImportDeclarationSpecifier::ImportSpecifier(spec) if spec.import_kind == ImportOrExportKind::Type
    )
}

fn can_merge_imports(
    current_type: &ImportType,
    existing: &[(ImportType, Span, ModuleType, bool)],
    allow_separate_type_imports: bool,
    is_current_statement_type_import: bool,
    is_current_specifier_type: bool,
) -> bool {
    if *current_type == ImportType::AllButDefault {
        return false;
    }

    if *current_type == ImportType::SideEffect {
        return !existing.is_empty();
    }

    // Special case: both are type imports (statement-level)
    // Check if one is default and one is named (cannot be merged per ESLint)
    let all_existing_are_type = existing.iter().all(|(_, _, _, is_stmt_type)| *is_stmt_type);
    if is_current_statement_type_import && is_current_specifier_type && all_existing_are_type {
        let current_is_default = matches!(current_type, ImportType::Default);
        let current_is_named = matches!(current_type, ImportType::Named);
        let has_default = existing.iter().any(|(t, _, _, _)| matches!(t, ImportType::Default));
        let has_named = existing.iter().any(|(t, _, _, _)| matches!(t, ImportType::Named));

        // Cannot merge if one is default and the other is named
        if (current_is_default && has_named) || (current_is_named && has_default) {
            return false;
        }
    }

    // Handle allowSeparateTypeImports option
    if allow_separate_type_imports {
        let has_value_statement_imports =
            existing.iter().any(|(_, _, _, is_stmt_type)| !is_stmt_type);
        let has_type_statement_imports =
            existing.iter().any(|(_, _, _, is_stmt_type)| *is_stmt_type);

        if is_current_statement_type_import {
            // Don't merge with value/mixed statements (allow separation)
            // But DO merge with other statement-level type imports (report as duplicate)
            if has_value_statement_imports && !has_type_statement_imports {
                return false;
            }
        } else {
            // Current is a value or mixed import (not statement-level type)
            // Don't merge with statement-level type imports (allow separation)
            if has_type_statement_imports {
                return false;
            }
        }
    }

    let namespace = existing.iter().find(|(t, _, _, _)| matches!(t, ImportType::Namespace));
    let named = existing.iter().find(|(t, _, _, _)| matches!(t, ImportType::Named));
    let default = existing.iter().find(|(t, _, _, _)| matches!(t, ImportType::Default));

    let has_namespace = namespace.is_some();
    let has_named = named.is_some();
    let has_default = default.is_some();

    match current_type {
        ImportType::Named => {
            has_named
                || (has_default
                    && !namespace.is_some_and(|(_, namespace_span, _, _)| {
                        default.unwrap().1 == *namespace_span
                    }))
        }
        ImportType::Namespace => {
            if has_named
                && has_default
                && let Some((_, named_span, _, _)) = named
                && let Some((_, default_span, _, _)) = default
                && named_span == default_span
            {
                return false;
            }

            has_namespace || has_default
        }
        ImportType::Default => has_default || has_namespace || has_named,
        _ => unreachable!("other ImportType values should be already checked"),
    }
}

#[test]
fn test() {
    use crate::tester::Tester;

    let pass = vec![
        (
            r#"import os from "os";
            import fs from "fs";"#,
            None,
        ),
        (r#"import { merge } from "lodash-es";"#, None),
        (r#"import _, { merge } from "lodash-es";"#, None),
        (r#"import * as Foobar from "async";"#, None),
        (r#"import "foo""#, None),
        (
            r#"import os from "os";
            export { something } from "os";"#,
            None,
        ),
        (
            r#"import * as bar from "os";
            import { baz } from "os";"#,
            None,
        ),
        (
            r#"import foo, * as bar from "os";
            import { baz } from "os";"#,
            None,
        ),
        (
            r#"import foo, { bar } from "os";
            import * as baz from "os";"#,
            None,
        ),
        (
            r#"import os from "os";
            export { hello } from "hello";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import os from "os";
            export * from "hello";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import os from "os";
            export { hello as hi } from "hello";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import os from "os";
            export default function(){};"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import { merge } from "lodash-es";
            export { merge as lodashMerge }"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"export { something } from "os";
            export * as os from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import { something } from "os";
            export * as os from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import * as os from "os";
            export { something } from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import os from "os";
            export * from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"export { something } from "os";
            export * from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import type { Os } from "os";
            import type { Fs } from "fs";"#,
            None,
        ),
        (
            r#"import { type Os } from "os";
            import type { Fs } from "fs";"#,
            None,
        ),
        (r#"import type { Merge } from "lodash-es";"#, None),
        (r#"import _, { type Merge } from "lodash-es";"#, None),
        (r#"import type * as Foobar from "async";"#, None),
        (
            r#"import type Os from "os";
            export type { Something } from "os";"#,
            None,
        ),
        (
            r#"import type Os from "os";
            export { type Something } from "os";"#,
            None,
        ),
        (
            r#"import type * as Bar from "os";
            import { type Baz } from "os";"#,
            None,
        ),
        (
            r#"import foo, * as bar from "os";
            import { type Baz } from "os";"#,
            None,
        ),
        (
            r#"import foo, { type bar } from "os";
            import type * as Baz from "os";"#,
            None,
        ),
        (
            r#"import type { Merge } from "lodash-es";
            import type _ from "lodash-es";"#,
            None,
        ),
        (
            r#"import type Os from "os";
            export { type Hello } from "hello";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import type Os from "os";
            export type * from "hello";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import type Os from "os";
            export { type Hello as Hi } from "hello";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import type Os from "os";
            export default function(){};"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import { type Merge } from "lodash-es";
            export { Merge as lodashMerge }"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"export type { Something } from "os";
            export * as os from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import { type Something } from "os";
            export * as os from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import type * as Os from "os";
            export { something } from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import type Os from "os";
            export * from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import type Os from "os";
            export type { Something } from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"export type { Something } from "os";
            export * from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import { foo, type Bar } from "module";"#,
            Some(serde_json::json!([{ "allowSeparateTypeImports": true }])),
        ),
        (
            r#"import { foo } from "module";
            import type { Bar } from "module";"#,
            Some(serde_json::json!([{ "allowSeparateTypeImports": true }])),
        ),
        (
            r#"import { type Foo } from "module";
            import type { Bar } from "module";"#,
            Some(serde_json::json!([{ "allowSeparateTypeImports": true }])),
        ),
        (
            r#"import { foo, type Bar } from "module";
            export { type Baz } from "module2";"#,
            Some(serde_json::json!([{ "allowSeparateTypeImports": true, "includeExports": true }])),
        ),
        (
            r#"import type { Foo } from "module";
            export { bar, type Baz } from "module";"#,
            Some(serde_json::json!([{ "allowSeparateTypeImports": true, "includeExports": true }])),
        ),
        (
            r#"import { type Foo } from "module";
            export type { Bar } from "module";"#,
            Some(serde_json::json!([{ "allowSeparateTypeImports": true, "includeExports": true }])),
        ),
        (
            r#"import type * as Foo from "module";
            export { type Bar } from "module";"#,
            Some(serde_json::json!([{ "allowSeparateTypeImports": true, "includeExports": true }])),
        ),
        (
            r#"import { type Foo } from "module";
            export type * as Bar from "module";"#,
            Some(serde_json::json!([{ "allowSeparateTypeImports": true, "includeExports": true }])),
        ),
    ];

    let fail = vec![
        (
            r#"import "fs";
            import "fs""#,
            None,
        ),
        (
            r#"import { merge } from "lodash-es";
            import { find } from "lodash-es";"#,
            None,
        ),
        (
            r#"import { merge } from "lodash-es";
            import _ from "lodash-es";"#,
            None,
        ),
        (
            r#"import os from "os";
            import { something } from "os";
            import * as foobar from "os";"#,
            None,
        ),
        (
            r#"import * as modns from "lodash-es";
            import { merge } from "lodash-es";
            import { baz } from "lodash-es";"#,
            None,
        ),
        (
            r#"export { os } from "os";
            export { something } from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import os from "os";
            export { os as foobar } from "os";
            export { something } from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import os from "os";
            export { something } from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import os from "os";
            export * as os from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"export * as os from "os";
            import os from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import * as modns from "mod";
            export * as  modns from "mod";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"export * from "os";
            export * from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import "os";
            export * from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import "fs";
            import "fs""#,
            None,
        ),
        (
            r#"import { type Merge } from "lodash-es";
            import { type Find } from "lodash-es";"#,
            None,
        ),
        (
            r#"import { type Merge } from "lodash-es";
            import type { Find } from "lodash-es";"#,
            None,
        ),
        (
            r#"import type { Merge } from "lodash-es";
            import type { Find } from "lodash-es";"#,
            None,
        ),
        (
            r#"import type Os from "os";
            import type { Something } from "os";
            import type * as Foobar from "os";"#,
            None,
        ),
        (
            r#"import type * as Modns from "lodash-es";
            import type { Merge } from "lodash-es";
            import type { Baz } from "lodash-es";"#,
            None,
        ),
        (
            r#"import { type Foo } from "module";
            export type { Bar } from "module";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"export { os } from "os";
            export type { Something } from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"export type { Os } from "os";
            export type { Something } from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import type { Os } from "os";
            export type { Os as Foobar } from "os";
            export type { Something } from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import type { Os } from "os";
            export type { Something } from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import type Os from "os";
            export type * as Os from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import type * as Modns from "mod";
            export type * as Modns from "mod";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"export type * from "os";
            export type * from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            r#"import "os";
            export type { Os } from "os";"#,
            Some(serde_json::json!([{ "includeExports": true }])),
        ),
        (
            "import { someValue } from 'module';
            import { anotherValue } from 'module';",
            Some(serde_json::json!([{ "allowSeparateTypeImports": true }])),
        ),
        (
            r#"import type { Merge } from "lodash-es";
            import type { Find } from "lodash-es";"#,
            Some(serde_json::json!([{ "allowSeparateTypeImports": true }])),
        ),
        (
            "import { someValue, type Foo } from 'module';
            import type { SomeType } from 'module';
            import type { AnotherType } from 'module';",
            Some(serde_json::json!([{ "allowSeparateTypeImports": true }])),
        ),
        (
            "import { type Foo } from 'module';
            import { type Bar } from 'module';",
            Some(serde_json::json!([{ "allowSeparateTypeImports": true }])),
        ),
        (
            r#"export type { Foo } from "module";
            export type { Bar } from "module";"#,
            Some(serde_json::json!([{ "allowSeparateTypeImports": true, "includeExports": true }])),
        ),
        (
            r#"import { type Foo } from "module";
            export { type Bar } from "module";
            export { type Baz } from "module";"#,
            Some(serde_json::json!([{ "allowSeparateTypeImports": true, "includeExports": true }])),
        ),
        (
            r#"import { type Foo } from "module";
            export { type Bar } from "module";
            export { regular } from "module";"#,
            Some(serde_json::json!([{ "allowSeparateTypeImports": true, "includeExports": true }])),
        ),
        (
            r#"import { type Foo } from "module";
            import { regular } from "module";
            export { type Bar } from "module";
            export { regular as other } from "module";"#,
            Some(serde_json::json!([{ "allowSeparateTypeImports": true, "includeExports": true }])),
        ),
    ];

    Tester::new(NoDuplicateImports::NAME, NoDuplicateImports::PLUGIN, pass, fail)
        .test_and_snapshot();
}
