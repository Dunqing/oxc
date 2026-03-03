use oxc_diagnostics::OxcDiagnostic;
use oxc_macros::declare_oxc_lint;
use oxc_span::Span;

use crate::{context::LintContext, rule::Rule};

fn no_named_as_default_diagnostic(
    span: Span,
    module_name: &str,
    export_name: &str,
) -> OxcDiagnostic {
    OxcDiagnostic::warn(format!("Module {export_name:?} has named export {module_name:?}"))
        .with_help(format!("Using default import as {module_name:?} can be confusing. Use another name for default import to avoid confusion."))
        .with_label(span)
}

// <https://github.com/import-js/eslint-plugin-import/blob/v2.29.1/docs/rules/no-named-as-default.md>
#[derive(Debug, Default, Clone)]
pub struct NoNamedAsDefault;

declare_oxc_lint!(
    /// ### What it does
    ///
    /// Reports use of an exported name as the locally imported name of a default export.
    /// This happens when an imported default export is assigned a name that conflicts
    /// with a named export from the same module.
    ///
    /// ### Why is this bad?
    ///
    /// Using a named export's identifier for a default export can cause confusion
    /// and errors in understanding which value is being imported. It also reduces
    /// code clarity, making it harder for other developers to understand the intended
    /// imports.
    ///
    /// ### Examples
    ///
    /// Given
    /// ```javascript
    /// // foo.js
    /// export default 'foo';
    /// export const bar = 'baz';
    /// ```
    ///
    /// Examples of **incorrect** code for this rule:
    /// ```javascript
    /// // Invalid: using exported name 'bar' as the identifier for default export.
    /// import bar from './foo.js';
    /// ```
    ///
    /// Examples of **correct** code for this rule:
    /// ```javascript
    /// // Valid: correctly importing default export with a non-conflicting name.
    /// import foo from './foo.js';
    /// ```
    NoNamedAsDefault,
    import,
    suspicious
);

impl Rule for NoNamedAsDefault {
    fn run_once(&self, ctx: &LintContext<'_>) {
        let Some(module) = ctx.current_module() else {
            return;
        };

        for import in module.named_imports.values() {
            if !import.is_default_import {
                continue;
            }

            let Some(record) = module.import_records.get(import.record_idx.index()) else {
                continue;
            };
            let Some(target_idx) = record.resolved_module else {
                continue;
            };
            let Some(remote) = ctx.resolve_module(target_idx) else {
                continue;
            };

            // Get the local name of the default import from the module graph
            let Some(graph) = ctx.module_graph() else {
                return;
            };
            let import_name = graph.symbol_name(import.local_symbol);

            if !remote.named_exports.contains_key(import_name)
                && !remote.resolved_exports.contains_key(import_name)
            {
                continue;
            }

            if default_and_named_are_same_reexport(remote, import_name, ctx) {
                continue;
            }

            ctx.diagnostic(no_named_as_default_diagnostic(
                import.span,
                import_name,
                &record.specifier,
            ));
        }
    }
}

/// Check if the remote module re-exports both the default and the given named export
/// from the same source module with the same local identifier.
///
/// This is a special case where using the named export's name for the default import is allowed,
/// because they refer to the same value.
///
/// See <https://github.com/import-js/eslint-plugin-import/pull/3032>
/// and <https://github.com/oxc-project/oxc/issues/19099>
fn default_and_named_are_same_reexport(
    remote: &oxc_module_graph::NormalModule,
    name: &str,
    ctx: &LintContext<'_>,
) -> bool {
    // Find the default re-export entry.
    let Some(default_entry) =
        remote.indirect_export_entries.iter().find(|entry| entry.exported_name == "default")
    else {
        return false;
    };

    // Find the named re-export entry
    let Some(named_entry) =
        remote.indirect_export_entries.iter().find(|entry| entry.exported_name == name)
    else {
        return false;
    };

    // Both must re-export from the same module
    if default_entry.module_request != named_entry.module_request {
        return false;
    }

    // Both must import the same identifier from that module.
    // For direct re-exports (`export { foo as default } from './source'`), the imported_name
    // directly reflects the source binding name and can be compared as-is.
    //
    // For import-then-export (`import foo from './source'; export { foo as default }`),
    // the parser sets imported_name to the local name ("foo") rather than "default".
    // In that case the imported_name is unreliable, so we resolve the actual source name
    // by checking the remote module's named imports.
    let Some(graph) = ctx.module_graph() else {
        return false;
    };
    let default_source_name = remote
        .named_imports
        .values()
        .find(|imp| {
            imp.is_default_import && {
                let rec = remote.import_records.get(imp.record_idx.index());
                rec.is_some_and(|r| r.specifier == default_entry.module_request)
                    && graph.symbol_name(imp.local_symbol) == default_entry.imported_name.as_str()
            }
        })
        .map_or(default_entry.imported_name.as_str(), |_| "default");

    default_source_name == named_entry.imported_name.as_str()
}

#[test]
fn test() {
    use crate::tester::Tester;

    let pass = vec![
        r#"import "./malformed.js""#,
        r#"import bar, { foo } from "./bar";"#,
        r#"import bar, { foo } from "./empty-folder";"#,
        // unsupported syntax
        // r#"export default from "./bar";"#,
        r#"import bar, { foo } from "./export-default-string-and-named""#,
        // When both default and named exports are re-exported from the same source,
        // using the named export's name for the default import is not confusing.
        r#"import userEvent from "./re-export-default-and-named""#,
        // Also allowed.
        r#"import { userEvent } from "./re-export-default-and-named""#,
        // Import-then-export of the same named binding as both default and named.
        // Both refer to the same source binding, so this is allowed.
        r#"import userEvent from "./re-export-default-and-named-import-then-export""#,
    ];

    let fail = vec![
        r#"import foo from "./bar";"#,
        r#"import foo, { foo as bar } from "./bar";"#,
        // unsupported syntax
        // r#"export default, { foo as bar } from "./bar";"#,
        // r#"import foo from "./malformed.js""#,
        r#"import foo from "./export-default-string-and-named""#,
        r#"import foo, { foo as bar } from "./export-default-string-and-named""#,
        // When default and named exports are re-exported from different sources,
        // it should still report.
        r#"import userEvent from "./re-export-default-and-named-misleading""#,
        // When default and named exports are re-exported through local aliases
        // that map to different remote symbols, it should still report.
        r#"import userEvent from "./re-export-default-and-named-alias-misleading""#,
        // When default and named exports are re-exported from the same source
        // but refer to different bindings, it should still report.
        r#"import userEvent from "./re-export-default-and-named-different-binding""#,
    ];

    Tester::new(NoNamedAsDefault::NAME, NoNamedAsDefault::PLUGIN, pass, fail)
        .change_rule_path("index.js")
        .with_import_plugin(true)
        .test_and_snapshot();
}
