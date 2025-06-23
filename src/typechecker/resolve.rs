#![allow(unused_imports)]

use crate::typechecker::EnumVariantKind;
use std::collections::HashMap;
use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    ASTNode, AssignOp, BinOp, Expr, Function, Struct, Type, TypeAnnot, TypedASTNode, TypedEnum,
    TypedEnumVariant, TypedEnumVariantKind, TypedExpr, TypedExprKind, TypedFunction, TypedStruct,
};
use crate::typechecker::{EnumTy, StructTy, TypeEnv, type_annot_to_type};
use crate::{t_bool, t_float, t_int, t_list, t_string, t_unit, tvar};

impl TypeEnv<'_> {
    // Resolve types in the entire environment and AST
    pub fn resolve_all(&mut self, ast: Vec<TypedASTNode>) -> Vec<TypedASTNode> {
        // Resolve variable types
        let mut resolved_vars = HashMap::new();
        for (name, ty) in &self.variables {
            resolved_vars.insert(name.clone(), self.resolve(ty.clone()));
        }
        self.variables = resolved_vars;

        // Resolve struct types
        let mut resolved_structs = HashMap::new();
        for (name, struct_ty) in &self.structs {
            let mut resolved_fields = Vec::new();
            for (field_name, field_ty) in &struct_ty.fields {
                resolved_fields.push((field_name.clone(), self.resolve(field_ty.clone())));
            }

            resolved_structs.insert(
                name.clone(),
                Arc::new(StructTy {
                    name: struct_ty.name.clone(),
                    generics: struct_ty.generics.clone(),
                    fields: resolved_fields,
                }),
            );
        }
        self.structs = resolved_structs;

        let mut resolved_enums = HashMap::new();
        for (name, enum_ty) in &self.enums {
            let mut resolved_variants = HashMap::new();

            for (variant_name, (kind, ty)) in &enum_ty.variants {
                let resolved_ty = self.resolve(ty.clone());

                // Resolve variant field types
                let resolved_kind = match kind {
                    EnumVariantKind::Unit => EnumVariantKind::Unit,
                    EnumVariantKind::Tuple(fields) => {
                        EnumVariantKind::Tuple(fields.iter().map(|t| t.clone()).collect())
                    }
                    EnumVariantKind::Struct(fields) => EnumVariantKind::Struct(
                        fields
                            .iter()
                            .map(|(n, t, r)| (n.clone(), t.clone(), r.clone()))
                            .collect(),
                    ),
                };

                resolved_variants.insert(variant_name.clone(), (resolved_kind, resolved_ty));
            }

            resolved_enums.insert(
                name.clone(),
                Arc::new(EnumTy {
                    name: enum_ty.name.clone(),
                    generics: enum_ty.generics.clone(),
                    variants: resolved_variants,
                }),
            );
        }
        self.enums = resolved_enums;

        // Resolve AST nodes
        ast.into_iter()
            .map(|node| self.resolve_node(node))
            .collect()
    }

    fn resolve_node(&mut self, node: TypedASTNode) -> TypedASTNode {
        match node {
            TypedASTNode::Expr((expr, span)) => TypedASTNode::Expr((self.resolve_expr(expr), span)),
            TypedASTNode::Function((func, span)) => {
                TypedASTNode::Function((self.resolve_function(func), span))
            }
            TypedASTNode::Struct((strukt, span)) => {
                TypedASTNode::Struct((self.resolve_struct(strukt), span))
            }
            TypedASTNode::Enum((enum_, span)) => {
                TypedASTNode::Enum((self.resolve_enum(enum_), span))
            }
            TypedASTNode::Error => TypedASTNode::Error,
        }
    }

    fn resolve_enum(&mut self, enum_: TypedEnum) -> TypedEnum {
        // Resolve generic parameters
        let generics = enum_.generics; // Just names and spans, no types to resolve

        // Resolve each variant
        let variants = enum_
            .variants
            .into_iter()
            .map(|variant| self.resolve_enum_variant(variant))
            .collect();

        TypedEnum {
            name: enum_.name,
            generics,
            variants,
        }
    }

    fn resolve_enum_variant(&mut self, variant: TypedEnumVariant) -> TypedEnumVariant {
        let kind = match variant.kind {
            TypedEnumVariantKind::Unit => TypedEnumVariantKind::Unit,
            TypedEnumVariantKind::Tuple(fields) => {
                let resolved_fields = fields.into_iter().map(|ty| self.resolve(ty)).collect();
                TypedEnumVariantKind::Tuple(resolved_fields)
            }
            TypedEnumVariantKind::Struct(fields) => {
                let resolved_fields = fields
                    .into_iter()
                    .map(|(name, ty)| (name, self.resolve(ty)))
                    .collect();
                TypedEnumVariantKind::Struct(resolved_fields)
            }
        };

        TypedEnumVariant {
            name: variant.name,
            kind,
            range: variant.range,
        }
    }

    fn resolve_expr(&mut self, expr: TypedExpr) -> TypedExpr {
        // Resolve the type first
        let resolved_ty = self.resolve(expr.ty.clone());

        // Then resolve child expressions
        let kind = match expr.kind {
            TypedExprKind::Call { function, args } => {
                let resolved_function = self.resolve_expr(*function);
                let resolved_args = args.into_iter().map(|arg| self.resolve_expr(arg)).collect();
                TypedExprKind::Call {
                    function: Box::new(resolved_function),
                    args: resolved_args,
                }
            }
            TypedExprKind::BinOp {
                operator,
                l_value,
                r_value,
            } => {
                let resolved_l = self.resolve_expr(*l_value);
                let resolved_r = self.resolve_expr(*r_value);
                TypedExprKind::BinOp {
                    operator,
                    l_value: Box::new(resolved_l),
                    r_value: Box::new(resolved_r),
                }
            }
            TypedExprKind::Let { var, value } => {
                let resolved_value = self.resolve_expr(*value);
                TypedExprKind::Let {
                    var,
                    value: Box::new(resolved_value),
                }
            }
            // Add cases for other expression types as needed...
            _ => expr.kind,
        };

        TypedExpr {
            kind,
            ty: resolved_ty,
            range: expr.range,
        }
    }

    fn resolve_function(&mut self, func: TypedFunction) -> TypedFunction {
        TypedFunction {
            name: func.name,
            args: func
                .args
                .into_iter()
                .map(|(name, ty, range)| (name, self.resolve(ty), range))
                .collect(),
            body: Box::new((self.resolve_expr(func.body.0), func.body.1)),
            return_type: (self.resolve(func.return_type.0), func.return_type.1),
        }
    }

    fn resolve_struct(&mut self, strukt: TypedStruct) -> TypedStruct {
        TypedStruct {
            name: strukt.name,
            generics: strukt.generics,
            fields: strukt
                .fields
                .into_iter()
                .map(|(name, ty, range)| (name, self.resolve(ty), range))
                .collect(),
        }
    }
}
