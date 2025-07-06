use crate::ast::Type;
use crate::ast::TypedASTNode;
use crate::ast::TypedEnum;
use crate::ast::TypedEnumVariant;
use crate::ast::TypedEnumVariantKind;
use crate::ast::TypedExpr;
use crate::ast::TypedExprKind;
use crate::ast::TypedFunction;
use crate::ast::TypedStruct;

use std::collections::HashSet;
use std::ops::Range;
use std::sync::Arc;

type ReportedSet = HashSet<(usize, Range<usize>)>;

pub fn check_for_type_vars(
    ast: &Vec<TypedASTNode>,
    file_path: &str,
) -> Vec<(String, Range<usize>, String)> {
    let mut reported = HashSet::new();
    let mut errors = Vec::new();

    for node in ast {
        match node {
            TypedASTNode::Expr((expr, _)) => {
                let mut node_errors = check_typed_expr(expr, &mut reported);
                errors.extend(
                    node_errors
                        .drain(..)
                        .map(|(span, reason)| (file_path.to_string(), span, reason)),
                );
            }
            TypedASTNode::Function((func, _)) => {
                let mut node_errors = check_typed_function(func, &mut reported);
                errors.extend(
                    node_errors
                        .drain(..)
                        .map(|(span, reason)| (file_path.to_string(), span, reason)),
                );
            }
            TypedASTNode::Struct((strct, _)) => {
                let mut node_errors = check_typed_struct(strct, &mut reported);
                errors.extend(
                    node_errors
                        .drain(..)
                        .map(|(span, reason)| (file_path.to_string(), span, reason)),
                );
            }
            TypedASTNode::Enum((enm, _)) => {
                let mut node_errors = check_typed_enum(enm, &mut reported);
                errors.extend(
                    node_errors
                        .drain(..)
                        .map(|(span, reason)| (file_path.to_string(), span, reason)),
                );
            }
            TypedASTNode::Error => {
                errors.push((
                    file_path.to_string(),
                    0..0,
                    "Error node found in AST".to_string(),
                ));
            }
        }
    }

    errors
}

pub fn check_type(
    ty: &Arc<Type>,
    span: Range<usize>,
    reported: &mut ReportedSet,
) -> Vec<(Range<usize>, String)> {
    let mut errors = Vec::new();

    match &**ty {
        Type::Constructor { generics, .. } => {
            for generic in generics {
                errors.extend(check_type(generic, span.clone(), reported));
            }
        }
        Type::Variable(index) => {
            if reported.insert((*index, span.clone())) {
                errors.push((span, "unresolved type variable".to_string()));
            }
        }
        Type::Trait(_) => {}
        Type::Function {
            params,
            return_type,
        } => {
            for param in params {
                errors.extend(check_type(param, span.clone(), reported));
            }
            errors.extend(check_type(return_type, span, reported));
        }
        Type::Tuple(elems) => {
            for elem in elems {
                errors.extend(check_type(elem, span.clone(), reported));
            }
        }
        Type::Union(types) => {
            for t in types {
                errors.extend(check_type(t, span.clone(), reported));
            }
        }
        Type::Never | Type::Unit => {}
    }

    errors
}

pub fn check_typed_expr(
    expr: &TypedExpr,
    reported: &mut ReportedSet,
) -> Vec<(Range<usize>, String)> {
    let mut errors = Vec::new();

    errors.extend(check_type(&expr.ty, expr.range.clone(), reported));

    match &expr.kind {
        TypedExprKind::Bool(_)
        | TypedExprKind::Int(_)
        | TypedExprKind::Float(_)
        | TypedExprKind::String(_)
        | TypedExprKind::Variable(_) => {}

        TypedExprKind::Return(inner) => {
            errors.extend(check_typed_expr(inner, reported));
        }

        TypedExprKind::Lambda { args, expression } => {
            for (_, ty, span) in args {
                errors.extend(check_type(ty, span.clone(), reported));
            }
            errors.extend(check_typed_expr(expression, reported));
        }

        TypedExprKind::Array { elements } => {
            for elem in elements {
                errors.extend(check_typed_expr(elem, reported));
            }
        }

        TypedExprKind::Index { array, index } => {
            errors.extend(check_typed_expr(array, reported));
            errors.extend(check_typed_expr(index, reported));
        }

        TypedExprKind::Call { function, args } => {
            errors.extend(check_typed_expr(function, reported));
            for arg in args {
                errors.extend(check_typed_expr(arg, reported));
            }
        }

        TypedExprKind::StructAccess { struct_val, .. } => {
            errors.extend(check_typed_expr(struct_val, reported));
        }

        TypedExprKind::MethodCall {
            struct_val, args, ..
        } => {
            errors.extend(check_typed_expr(struct_val, reported));
            for arg in args {
                errors.extend(check_typed_expr(arg, reported));
            }
        }

        TypedExprKind::BinOp {
            l_value, r_value, ..
        } => {
            errors.extend(check_typed_expr(l_value, reported));
            errors.extend(check_typed_expr(r_value, reported));
        }

        TypedExprKind::Assign {
            l_value, r_value, ..
        } => {
            errors.extend(check_typed_expr(l_value, reported));
            errors.extend(check_typed_expr(r_value, reported));
        }

        TypedExprKind::UnOp { expression, .. } => {
            errors.extend(check_typed_expr(expression, reported));
        }

        TypedExprKind::Do { expressions } => {
            for expr in expressions {
                errors.extend(check_typed_expr(expr, reported));
            }
        }

        TypedExprKind::Let { value, .. } => {
            errors.extend(check_typed_expr(value, reported));
        }

        TypedExprKind::IfElse {
            condition,
            if_branch,
            else_branch,
        } => {
            errors.extend(check_typed_expr(condition, reported));
            errors.extend(check_typed_expr(if_branch, reported));
            if let Some(else_expr) = else_branch {
                errors.extend(check_typed_expr(else_expr, reported));
            }
        }

        TypedExprKind::Tuple(elements) => {
            for elem in elements {
                errors.extend(check_typed_expr(elem, reported));
            }
        }

        TypedExprKind::EnumVariant { fields, .. } => {
            for (_, expr) in fields {
                errors.extend(check_typed_expr(expr, reported));
            }
        }

        TypedExprKind::Match { expr, arms } => {
            errors.extend(check_typed_expr(expr, reported));
            for arm in arms {
                errors.extend(check_typed_expr(&arm.body, reported));
            }
        }

        TypedExprKind::Error => {}
    }

    errors
}

pub fn check_typed_function(
    func: &TypedFunction,
    reported: &mut ReportedSet,
) -> Vec<(Range<usize>, String)> {
    let mut errors = Vec::new();

    for (_, ty, span) in &func.args {
        errors.extend(check_type(ty, span.clone(), reported));
    }

    let (ret_ty, ret_span) = &func.return_type;
    errors.extend(check_type(ret_ty, ret_span.clone(), reported));

    errors.extend(check_typed_expr(&func.body.0, reported));

    errors
}

pub fn check_typed_struct(
    strct: &TypedStruct,
    reported: &mut ReportedSet,
) -> Vec<(Range<usize>, String)> {
    let mut errors = Vec::new();

    for (_, ty, span) in &strct.fields {
        errors.extend(check_type(ty, span.clone(), reported));
    }

    errors
}

pub fn check_typed_enum(
    enm: &TypedEnum,
    reported: &mut ReportedSet,
) -> Vec<(Range<usize>, String)> {
    let mut errors = Vec::new();

    for variant in &enm.variants {
        errors.extend(check_typed_enum_variant(variant, reported));
    }

    errors
}

pub fn check_typed_enum_variant(
    variant: &TypedEnumVariant,
    reported: &mut ReportedSet,
) -> Vec<(Range<usize>, String)> {
    let mut errors = Vec::new();

    match &variant.kind {
        TypedEnumVariantKind::Unit => {}
        TypedEnumVariantKind::Tuple(tys) => {
            for ty in tys {
                errors.extend(check_type(ty, variant.range.clone(), reported));
            }
        }
        TypedEnumVariantKind::Struct(fields) => {
            for (_, ty) in fields {
                errors.extend(check_type(ty, variant.range.clone(), reported));
            }
        }
    }

    errors
}
