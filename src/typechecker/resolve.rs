#![allow(unused_imports)]

use crate::ast::TypedMatchArm;
use crate::ast::TypedPattern;
use crate::typechecker::EnumVariantKind;
use crate::typechecker::EnumVariantKindTy;
use crate::typechecker::EnumVariantTy;
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

            for (variant_name, variant_ty) in &enum_ty.variants {
                let resolved_ty = self.resolve(variant_ty.ty.clone());

                // Resolve variant field types
                let resolved_kind = match &variant_ty.kind {
                    EnumVariantKindTy::Unit => EnumVariantKindTy::Unit,
                    EnumVariantKindTy::Tuple(fields) => EnumVariantKindTy::Tuple(
                        fields.iter().map(|t| self.resolve(t.clone())).collect(),
                    ),
                    EnumVariantKindTy::Struct(fields) => EnumVariantKindTy::Struct(
                        fields
                            .iter()
                            .map(|(n, t)| (n.clone(), self.resolve(t.clone())))
                            .collect(),
                    ),
                };

                resolved_variants.insert(
                    variant_name.clone(),
                    EnumVariantTy {
                        kind: resolved_kind,
                        ty: resolved_ty,
                    },
                );
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
        let generics = enum_.generics;

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

    pub fn resolve_expr(&mut self, expr: TypedExpr) -> TypedExpr {
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
            TypedExprKind::Match { expr, arms } => {
                let resolved_expr = self.resolve_expr(*expr);
                let resolved_arms = arms
                    .into_iter()
                    .map(|arm| self.resolve_match_arm(arm))
                    .collect();

                TypedExprKind::Match {
                    expr: Box::new(resolved_expr),
                    arms: resolved_arms,
                }
            }
            TypedExprKind::Return(inner) => {
                let resolved_inner = self.resolve_expr(*inner);
                TypedExprKind::Return(Box::new(resolved_inner))
            }
            TypedExprKind::Lambda { args, expression } => {
                let resolved_args = args
                    .into_iter()
                    .map(|(name, ty, span)| (name, self.resolve(ty), span))
                    .collect();
                
                let resolved_body = self.resolve_expr(*expression);
                
                TypedExprKind::Lambda {
                    args: resolved_args,
                    expression: Box::new(resolved_body),
                }
            }
            TypedExprKind::Bool(b) => TypedExprKind::Bool(b),
            TypedExprKind::Int(i) => TypedExprKind::Int(i),
            TypedExprKind::Float(f) => TypedExprKind::Float(f),
            TypedExprKind::String(s) => TypedExprKind::String(s),
            
            TypedExprKind::Variable(name) => TypedExprKind::Variable(name),
            
            TypedExprKind::Array { elements } => {
                let resolved_elements = elements.into_iter()
                    .map(|e| self.resolve_expr(e))
                    .collect();
                TypedExprKind::Array { elements: resolved_elements }
            },
            
            TypedExprKind::Index { array, index } => {
                let resolved_array = self.resolve_expr(*array);
                let resolved_index = self.resolve_expr(*index);
                TypedExprKind::Index {
                    array: Box::new(resolved_array),
                    index: Box::new(resolved_index),
                }
            },
            
            TypedExprKind::StructAccess { struct_val, field_name } => {
                let resolved_struct = self.resolve_expr(*struct_val);
                TypedExprKind::StructAccess {
                    struct_val: Box::new(resolved_struct),
                    field_name,
                }
            },
            
            TypedExprKind::MethodCall { struct_val, method_name, args } => {
                let resolved_struct = self.resolve_expr(*struct_val);
                let resolved_args = args.into_iter()
                    .map(|a| self.resolve_expr(a))
                    .collect();
                TypedExprKind::MethodCall {
                    struct_val: Box::new(resolved_struct),
                    method_name,
                    args: resolved_args,
                }
            },
            
            TypedExprKind::Assign { l_value, r_value, assign_op } => {
                let resolved_l = self.resolve_expr(*l_value);
                let resolved_r = self.resolve_expr(*r_value);
                TypedExprKind::Assign {
                    assign_op,
                    l_value: Box::new(resolved_l),
                    r_value: Box::new(resolved_r),
                }
            },
            
            TypedExprKind::UnOp { unop, expression } => {
                let resolved_expr = self.resolve_expr(*expression);
                TypedExprKind::UnOp {
                    unop,
                    expression: Box::new(resolved_expr),
                }
            },
            
            TypedExprKind::Do { expressions } => {
                let resolved_exprs = expressions.into_iter()
                    .map(|e| self.resolve_expr(e))
                    .collect();
                TypedExprKind::Do { expressions: resolved_exprs }
            },
            
            TypedExprKind::IfElse { condition, if_branch, else_branch } => {
                let resolved_cond = self.resolve_expr(*condition);
                let resolved_if = self.resolve_expr(*if_branch);
                let resolved_else = else_branch.map(|b| Box::new(self.resolve_expr(*b)));
                TypedExprKind::IfElse {
                    condition: Box::new(resolved_cond),
                    if_branch: Box::new(resolved_if),
                    else_branch: resolved_else,
                }
            },
            
            TypedExprKind::Tuple(elements) => {
                let resolved_elements = elements.into_iter()
                    .map(|e| self.resolve_expr(e))
                    .collect();
                TypedExprKind::Tuple(resolved_elements)
            },
            
            TypedExprKind::EnumVariant { enum_name, variant_name, fields } => {
                let resolved_fields = fields.into_iter()
                    .map(|(name, expr)| (name, self.resolve_expr(expr)))
                    .collect();
                TypedExprKind::EnumVariant {
                    enum_name,
                    variant_name,
                    fields: resolved_fields,
                }
            },
            
            TypedExprKind::Error => TypedExprKind::Error,
        };

        TypedExpr {
            kind,
            ty: resolved_ty,
            range: expr.range,
        }
    }

    fn resolve_match_arm(&mut self, arm: TypedMatchArm) -> TypedMatchArm {
        TypedMatchArm {
            pattern: self.resolve_pattern(arm.pattern),
            body: Box::new(self.resolve_expr(*arm.body)),
        }
    }

    fn resolve_pattern(&mut self, pattern: TypedPattern) -> TypedPattern {
        match pattern {
            TypedPattern::Variable(name, ty) => TypedPattern::Variable(name, self.resolve(ty)),
            TypedPattern::EnumVariant {
                enum_name,
                variant_name,
                subpatterns,
            } => TypedPattern::EnumVariant {
                enum_name,
                variant_name,
                subpatterns: subpatterns
                    .into_iter()
                    .map(|p| self.resolve_pattern(p))
                    .collect(),
            },
            TypedPattern::Union(subpatterns) => TypedPattern::Union(
                subpatterns
                    .into_iter()
                    .map(|p| self.resolve_pattern(p))
                    .collect(),
            ),
            TypedPattern::Tuple(subpatterns) => TypedPattern::Tuple(
                subpatterns
                    .into_iter()
                    .map(|p| self.resolve_pattern(p))
                    .collect(),
            ),
            TypedPattern::Literal(expr) => TypedPattern::Literal(self.resolve_expr(expr)),
            _ => pattern,
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
