#![allow(unused_imports)]

use std::collections::HashMap;
use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    ASTNode, AssignOp, BinOp, Expr, Function, Struct, Type, TypeAnnot, TypedASTNode, TypedExpr,
    TypedExprKind, TypedFunction, TypedStruct,
};
use crate::typechecker::{TypeEnv, type_annot_to_type, StructTy};
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
        
        // Resolve AST nodes
        ast.into_iter().map(|node| self.resolve_node(node)).collect()
    }
    
    fn resolve_node(&mut self, node: TypedASTNode) -> TypedASTNode {
        match node {
            TypedASTNode::Expr((expr, span)) => {
                TypedASTNode::Expr((self.resolve_expr(expr), span))
            }
            TypedASTNode::Function((func, span)) => {
                TypedASTNode::Function((self.resolve_function(func), span))
            }
            TypedASTNode::Struct((strukt, span)) => {
                TypedASTNode::Struct((self.resolve_struct(strukt), span))
            }
            TypedASTNode::Error => TypedASTNode::Error,
        }
    }
    
    fn resolve_expr(&mut self, expr: TypedExpr) -> TypedExpr {
        // Resolve the type first
        let resolved_ty = self.resolve(expr.ty.clone());
        
        // Then resolve child expressions
        let kind = match expr.kind {
            TypedExprKind::Call { function, args } => {
                let resolved_function = self.resolve_expr(*function);
                let resolved_args = args.into_iter()
                    .map(|arg| self.resolve_expr(arg))
                    .collect();
                TypedExprKind::Call {
                    function: Box::new(resolved_function),
                    args: resolved_args,
                }
            }
            TypedExprKind::BinOp { operator, l_value, r_value } => {
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
            args: func.args.into_iter()
                .map(|(name, ty, range)| (name, self.resolve(ty), range))
                .collect(),
            body: Box::new((
                self.resolve_expr(func.body.0),
                func.body.1,
            )),
            return_type: (self.resolve(func.return_type.0), func.return_type.1),
        }
    }
    
    fn resolve_struct(&mut self, strukt: TypedStruct) -> TypedStruct {
        TypedStruct {
            name: strukt.name,
            generics: strukt.generics,
            fields: strukt.fields.into_iter()
                .map(|(name, ty, range)| (name, self.resolve(ty), range))
                .collect(),
        }
    }
}