//! Contextual name inference shared by class transforms.

use oxc_ast::ast::*;
use oxc_span::SPAN;
use oxc_str::{JSStr, Str};
use oxc_syntax::identifier::is_identifier_name;
use oxc_traverse::Ancestor;

use crate::context::TraverseCtx;

/// Infer an anonymous class's name from its immediate non-parenthesized ancestor.
///
/// Property names captured during class or object evaluation are handled by the caller.
pub fn contextual_class_name<'a>(ancestor: &Ancestor<'a, '_>) -> Option<Str<'a>> {
    match ancestor {
        Ancestor::VariableDeclaratorInit(decl) => decl.id().get_identifier_name().map(Str::from),
        Ancestor::AssignmentPatternRight(pattern) => {
            pattern.left().get_identifier_name().map(Str::from)
        }
        Ancestor::FormalParameterInitializer(param) => {
            param.pattern().get_identifier_name().map(Str::from)
        }
        Ancestor::AssignmentExpressionRight(assign)
            if assign.operator().is_assign() || assign.operator().is_logical() =>
        {
            assignment_name(assign.left())
        }
        Ancestor::AssignmentTargetWithDefaultInit(target) => assignment_name(target.binding()),
        Ancestor::AssignmentTargetPropertyIdentifierInit(target) => {
            Some(Str::from(target.binding().name))
        }
        Ancestor::ExportDefaultDeclarationDeclaration(_) => Some(Str::from("default")),
        _ => None,
    }
}

fn assignment_name<'a>(target: &AssignmentTarget<'a>) -> Option<Str<'a>> {
    match target {
        AssignmentTarget::AssignmentTargetIdentifier(ident) => Some(ident.name.into()),
        _ => None,
    }
}

/// `{ name: class {} }.name`, or `{ "na-me": class {} }["na-me"]`.
pub fn create_named_evaluation<'a>(
    class_expr: Expression<'a>,
    name: JSStr<'a>,
    ctx: &TraverseCtx<'a>,
) -> Expression<'a> {
    let create_object = |key| {
        let property = ObjectPropertyKind::new_object_property(
            SPAN,
            PropertyKind::Init,
            key,
            class_expr,
            false,
            false,
            ctx,
        );
        Expression::new_object_expression(SPAN, [property], ctx)
    };

    if let Some(ident_name) =
        name.as_str().filter(|name| *name != "__proto__" && is_identifier_name(name))
    {
        // `{ name: class {} }.name`
        let key = PropertyKey::new_static_identifier(SPAN, ident_name, ctx);
        let object = create_object(key);
        let property = IdentifierName::new(SPAN, ident_name, ctx);
        Expression::new_static_member_expression(SPAN, object, property, false, ctx)
    } else {
        // `{ "na-me": class {} }["na-me"]`.
        // `{ ["__proto__"]: class {} }` is a normal property, unlike `{ __proto__: class {} }`.
        let key = if name == "__proto__" {
            PropertyKey::new_string_literal(SPAN, name, None, ctx)
        } else {
            PropertyKey::new_static_string_literal(SPAN, name, None, ctx)
        };
        let object = create_object(key);
        let key = Expression::new_string_literal(SPAN, name, None, ctx);
        Expression::new_computed_member_expression(SPAN, object, key, false, ctx)
    }
}
