use oxc_ast::ast::*;
use oxc_ast::AstBuilder;

use std::rc::Rc;

/// Transform TypeScript
///
/// References:
/// * <https://babeljs.io/docs/babel-plugin-transform-typescript>
/// * <https://github.com/babel/babel/tree/main/packages/babel-plugin-transform-typescript>
pub struct TypeScript<'a> {
    _ast: Rc<AstBuilder<'a>>,
}

impl<'a> TypeScript<'a> {
    pub fn new(_ast: Rc<AstBuilder<'a>>) -> Self {
        Self { _ast }
    }

    #[allow(clippy::unused_self)]
    pub fn transform_formal_parameters(&self, params: &mut FormalParameters<'a>) {
        if params.items.get(0).is_some_and(|param| matches!(&param.pattern.kind, BindingPatternKind::BindingIdentifier(ident) if ident.name =="this")) {
            params.items.remove(0);
        }
    }

    pub fn transform_program(&self, program: &mut Program<'a>) {
        // let needs_explicit_esm = false;

        for stmt in program.body.iter_mut() {
            if let Statement::ModuleDeclaration(module_decl) = stmt {
                match &mut **module_decl {
                    ModuleDeclaration::ExportNamedDeclaration(decl) => {
                        decl.specifiers.retain(|specifier| specifier.export_kind.is_value());
                    }
                    ModuleDeclaration::ImportDeclaration(decl) => {
                        decl.specifiers.retain(|specifier| match specifier {
                            ImportDeclarationSpecifier::ImportSpecifier(s) => {
                                s.import_kind.is_value()
                            }
                            _ => false,
                        });
                    }
                    _ => {}
                }
            }
        }

        program.body.retain(|stmt| match stmt {
            Statement::ModuleDeclaration(module_decl) => match &**module_decl {
                ModuleDeclaration::ImportDeclaration(decl) if decl.import_kind.is_type() => false,
                ModuleDeclaration::ExportNamedDeclaration(decl)
                    if decl.export_kind.is_type() || decl.specifiers.is_empty() =>
                {
                    false
                }
                _ => true,
            },
            _ => true,
        });
    }
}
