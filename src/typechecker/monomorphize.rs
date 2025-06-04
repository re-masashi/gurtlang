#![allow(unused_imports)]

use std::collections::HashMap;
use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    ASTNode, AssignOp, BinOp, Expr, Function, Struct, Type, TypeAnnot, TypedASTNode, TypedExpr,
    TypedExprKind, TypedFunction, TypedStruct,
};
use crate::typechecker::{StructTy, TypeEnv, type_annot_to_type};
use crate::{t_bool, t_float, t_int, t_list, t_string, t_unit, tvar};

impl TypeEnv<'_> {
    pub fn collect_monomorphizations(&self, ast: &[TypedASTNode]) -> HashMap<String, Vec<Vec<Arc<Type>>>> {
        let mut mono_map: HashMap<String, Vec<Vec<Arc<Type>>>> = HashMap::new();
        
        for node in ast {
            self.collect_node_monomorphizations(node, &mut mono_map);
        }
        
        mono_map
    }
    
    fn collect_node_monomorphizations(
        &self,
        node: &TypedASTNode,
        mono_map: &mut HashMap<String, Vec<Vec<Arc<Type>>>>
    ) {
        match node {
            TypedASTNode::Expr((expr, _)) => {
                self.collect_expr_monomorphizations(expr, mono_map);
            }
            TypedASTNode::Function((func, _)) => {
                self.collect_function_monomorphizations(func, mono_map);
            }
            TypedASTNode::Struct((strukt, _)) => {
                self.collect_struct_monomorphizations(strukt, mono_map);
            }
            _ => {}
        }
    }
    
    fn collect_expr_monomorphizations(
        &self,
        expr: &TypedExpr,
        mono_map: &mut HashMap<String, Vec<Vec<Arc<Type>>>>
    ) {
        match &expr.kind {
            TypedExprKind::Call { function, args } => {
                if let TypedExprKind::Variable(func_name) = &function.kind {
                    let arg_types: Vec<Arc<Type>> = args.iter()
                        .map(|arg| arg.ty.clone())
                        .collect();
                    
                    mono_map.entry(func_name.clone())
                        .or_default()
                        .push(arg_types);
                }
            }
            TypedExprKind::StructAccess { struct_val, .. } => {
                if let TypedExprKind::Variable(struct_name) = &struct_val.kind {
                    // Get the struct definition
                    if let Some(struct_ty) = self.structs.get(struct_name) {
                        // Collect generic parameter types
                        let generic_types: Vec<Arc<Type>> = struct_ty.generics.iter()
                            .map(|name| {
                                self.variables.get(name)
                                    .cloned()
                                    .unwrap_or_else(|| tvar!(0))
                            })
                            .collect();
                        
                        mono_map.entry(struct_name.clone())
                            .or_default()
                            .push(generic_types);
                    }
                }
            }
            _ => {}
        }
        
        // Recursively process child expressions
        match &expr.kind {
            TypedExprKind::Call { function, args } => {
                self.collect_expr_monomorphizations(function, mono_map);
                for arg in args {
                    self.collect_expr_monomorphizations(arg, mono_map);
                }
            }
            TypedExprKind::BinOp { l_value, r_value, .. } => {
                self.collect_expr_monomorphizations(l_value, mono_map);
                self.collect_expr_monomorphizations(r_value, mono_map);
            }
            TypedExprKind::Let { value, .. } => {
                self.collect_expr_monomorphizations(value, mono_map);
            }
            TypedExprKind::IfElse { condition, if_branch, else_branch, .. } => {
                self.collect_expr_monomorphizations(condition, mono_map);
                self.collect_expr_monomorphizations(if_branch, mono_map);
                if let Some(branch) = else_branch {
                    self.collect_expr_monomorphizations(branch, mono_map);
                }
            }
            TypedExprKind::Do {expressions}=>{
                for expr in expressions {
                    self.collect_expr_monomorphizations(expr, mono_map);
                }
            }
            TypedExprKind::Array { elements } => {
                for elem in elements {
                    self.collect_expr_monomorphizations(elem, mono_map);
                }
            }
            TypedExprKind::Tuple(elements) => {
                for elem in elements {
                    self.collect_expr_monomorphizations(elem, mono_map);
                }
            }
            // Add other recursive cases as needed...
            _ => {}
        }
    }
    
    fn collect_function_monomorphizations(
        &self,
        func: &TypedFunction,
        mono_map: &mut HashMap<String, Vec<Vec<Arc<Type>>>>
    ) {
        // For functions, collect based on argument types
        let arg_types: Vec<Arc<Type>> = func.args.iter()
            .map(|(_, ty, _)| ty.clone())
            .collect();
        
        if !arg_types.is_empty() {
            mono_map.entry(func.name.clone())
                .or_default()
                .push(arg_types);
        }
        
        // Process function body
        self.collect_expr_monomorphizations(&func.body.0, mono_map);
    }
    
    fn collect_struct_monomorphizations(
        &self,
        strukt: &TypedStruct,
        mono_map: &mut HashMap<String, Vec<Vec<Arc<Type>>>>
    ) {
        // Collect struct's generic parameters
        let generic_types: Vec<Arc<Type>> = strukt.generics.iter()
            .map(|(name, _)| {
                self.variables.get(name)
                    .cloned()
                    .unwrap_or_else(|| tvar!(0))
            })
            .collect();
        
        if !generic_types.is_empty() {
            mono_map.entry(strukt.name.clone())
                .or_default()
                .push(generic_types);
        }
    }
}