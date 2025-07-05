#![allow(unused_imports)]

use std::collections::HashMap;
use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    ASTNode, AssignOp, BinOp, Expr, Function, Struct, Type, TypeAnnot, TypedASTNode, TypedExpr,
    TypedExprKind, TypedFunction, TypedStruct,
};
use crate::typechecker::{StructTy, TypeEnv, type_annot_to_type, type_string};
use crate::{t_bool, t_float, t_int, t_list, t_string, t_unit, tvar};

impl TypeEnv<'_> {
    fn substitute_type(&self, ty: &Arc<Type>, type_map: &HashMap<usize, Arc<Type>>) -> Arc<Type> {
        match &**ty {
            Type::Variable(id) => {
                type_map.get(id).cloned().unwrap_or_else(|| {
                    // If not in map, try to resolve it
                    let resolved = self.resolve(ty.clone());
                    if let Type::Variable(id) = &*resolved {
                        type_map.get(id).cloned().unwrap_or(resolved)
                    } else {
                        self.substitute_type(&resolved, type_map)
                    }
                })
            }
            Type::Constructor {
                name,
                generics,
                traits,
            } => {
                let new_generics = generics
                    .iter()
                    .map(|t| self.substitute_type(t, type_map))
                    .collect();
                Arc::new(Type::Constructor {
                    name: name.clone(),
                    generics: new_generics,
                    traits: traits.clone(),
                })
            }
            Type::Function {
                params,
                return_type,
            } => {
                let new_params = params
                    .iter()
                    .map(|t| self.substitute_type(t, type_map))
                    .collect();
                let new_return = self.substitute_type(return_type, type_map);
                Arc::new(Type::Function {
                    params: new_params,
                    return_type: (new_return),
                })
            }
            Type::Tuple(types) => {
                let new_types = types
                    .iter()
                    .map(|t| self.substitute_type(t, type_map))
                    .collect();
                Arc::new(Type::Tuple(new_types))
            }
            Type::Union(types) => {
                let new_types = types
                    .iter()
                    .map(|t| self.substitute_type(t, type_map))
                    .collect();
                Arc::new(Type::Union(new_types))
            }
            _ => ty.clone(),
        }
    }
    /// Properly monomorphizes the AST with correct order and type replacement
    pub fn monomorphize_ast(&mut self, ast: Vec<TypedASTNode>) -> Vec<TypedASTNode> {
        // First pass: collect all generic functions
        let mut generic_fns = HashMap::new();
        let mut non_generic_nodes = Vec::new();

        for node in ast {
            if let TypedASTNode::Function((ref func, ref _span)) = node {
                if func.is_generic() {
                    if let TypedASTNode::Function((func, span)) = node {
                        generic_fns.insert(func.name.clone(), (func, span));
                    }
                    continue;
                }
            }
            non_generic_nodes.push(node);
        }

        // Second pass: process nodes and specialize functions
        let mut specialized_fns = HashMap::new();
        let mut processed_nodes = Vec::new();

        for node in non_generic_nodes {
            processed_nodes.push(self.process_node(node, &generic_fns, &mut specialized_fns));
        }

        // Third pass: add specialized functions to the beginning of the AST
        let mut result = Vec::new();
        for (_, (func, span)) in specialized_fns {
            result.push(TypedASTNode::Function((func, span)));
        }
        result.extend(processed_nodes);
        result
    }

    fn process_node(
        &mut self,
        node: TypedASTNode,
        generic_fns: &HashMap<String, (TypedFunction, Range<usize>)>,
        specialized_fns: &mut HashMap<String, (TypedFunction, Range<usize>)>,
    ) -> TypedASTNode {
        match node {
            TypedASTNode::Expr((expr, span)) => {
                TypedASTNode::Expr((self.process_expr(expr, generic_fns, specialized_fns), span))
            }
            TypedASTNode::Function((func, span)) => {
                // Process function body for nested generics
                let (body_expr, body_span) = *func.body;
                let processed_body = self.process_expr(body_expr, generic_fns, specialized_fns);
                TypedASTNode::Function((
                    TypedFunction {
                        body: Box::new((processed_body, body_span)),
                        ..func
                    },
                    span,
                ))
            }
            _ => node,
        }
    }

    fn process_expr(
        &mut self,
        expr: TypedExpr,
        generic_fns: &HashMap<String, (TypedFunction, Range<usize>)>,
        specialized_fns: &mut HashMap<String, (TypedFunction, Range<usize>)>,
    ) -> TypedExpr {
        match expr.kind {
            TypedExprKind::Call { function, args } => {
                let new_function = self.process_expr(*function, generic_fns, specialized_fns);
                let new_args = args
                    .into_iter()
                    .map(|arg| self.process_expr(arg, generic_fns, specialized_fns))
                    .collect::<Vec<_>>();

                if let TypedExprKind::Variable(func_name) = &new_function.kind {
                    if let Some((generic_func, orig_span)) = generic_fns.get(func_name) {
                        // Create specialized function
                        let specialized_func =
                            self.specialize_function_at_call(generic_func, &new_args);
                        let spec_name = specialized_func.name.clone();

                        let func_ty = Arc::new(Type::Function {
                            params: specialized_func
                                .args
                                .iter()
                                .map(|(_, ty, _)| ty.clone())
                                .collect(),
                            return_type: specialized_func.return_type.0.clone(),
                        });

                        // Store specialized function
                        specialized_fns.insert(
                            spec_name.clone(),
                            (specialized_func.clone(), orig_span.clone()),
                        );

                        // Return new call with concrete type
                        return TypedExpr {
                            kind: TypedExprKind::Call {
                                function: Box::new(TypedExpr {
                                    kind: TypedExprKind::Variable(spec_name),
                                    ty: func_ty,
                                    range: new_function.range,
                                }),
                                args: new_args,
                            },
                            ty: specialized_func.return_type.0.clone(),
                            range: expr.range,
                        };
                    } else if let Some(struct_ty) = self.get_struct(func_name) {
                        if !struct_ty.generics.is_empty() {
                            return self.specialize_struct_constructor(
                                func_name,
                                &struct_ty,
                                &new_args,
                                expr.range,
                                specialized_fns,
                            );
                        }
                    }
                }

                // Non-generic call
                TypedExpr {
                    kind: TypedExprKind::Call {
                        function: Box::new(new_function),
                        args: new_args,
                    },
                    ty: expr.ty,
                    range: expr.range,
                }
            }
            // ... handle other expression types ...
            _ => expr,
        }
    }

    fn specialize_struct_constructor(
        &mut self,
        struct_name: &str,
        struct_ty: &Arc<StructTy>,
        args: &[TypedExpr],
        call_range: Range<usize>,
        specialized_fns: &mut HashMap<String, (TypedFunction, Range<usize>)>,
    ) -> TypedExpr {
        // Create type mapping from field type variables to argument types
        let mut type_map = HashMap::new();
        for (i, (_, field_ty)) in struct_ty.fields.iter().enumerate() {
            if let Type::Variable(id) = &**field_ty {
                let resolved_arg_ty = self.resolve_deep(args[i].ty.clone());
                type_map.insert(*id, resolved_arg_ty);
            }
        }

        // Create specialized return type with concrete generics
        let specialized_return = Arc::new(Type::Constructor {
            name: struct_name.to_string(),
            generics: struct_ty
                .fields
                .iter()
                .filter_map(|(_, ty)| {
                    if let Type::Variable(id) = &**ty {
                        type_map.get(id).cloned()
                    } else {
                        None
                    }
                })
                .collect(),
            traits: vec![],
        });

        // Create the specialized constructor function
        let spec_name = struct_name.to_string();
        if !specialized_fns.contains_key(&spec_name) {
            let constructor_func =
                self.create_constructor_function(struct_ty, &spec_name, &type_map);
            specialized_fns.insert(spec_name.clone(), (constructor_func, call_range.clone()));
        }

        // Return the new call expression
        TypedExpr {
            kind: TypedExprKind::Call {
                function: Box::new(TypedExpr {
                    kind: TypedExprKind::Variable(spec_name.clone()),
                    ty: Arc::new(Type::Function {
                        params: args.iter().map(|a| a.ty.clone()).collect(),
                        return_type: specialized_return.clone(),
                    }),
                    range: call_range.clone(),
                }),
                args: args.to_vec(),
            },
            ty: specialized_return,
            range: call_range,
        }
    }

    fn create_constructor_function(
        &self,
        struct_ty: &StructTy,
        spec_name: &str,
        type_map: &HashMap<usize, Arc<Type>>,
    ) -> TypedFunction {
        let specialized_fields: Vec<(String, Arc<Type>, Range<usize>)> = struct_ty
            .fields
            .iter()
            .map(|(name, ty)| {
                let spec_ty = self.substitute_type(ty, type_map);
                (name.clone(), spec_ty, 0..0)
            })
            .collect();

        // Create specialized return type with concrete generics
        let specialized_return = Arc::new(Type::Constructor {
            name: struct_ty.name.clone(),
            generics: struct_ty
                .fields
                .iter()
                .filter_map(|(_, ty)| {
                    if let Type::Variable(id) = &**ty {
                        type_map.get(id).cloned()
                    } else {
                        None
                    }
                })
                .collect(),
            traits: vec![],
        });

        let dummy_body = TypedExpr {
            kind: TypedExprKind::Error,
            ty: specialized_return.clone(),
            range: 0..0,
        };

        TypedFunction {
            name: spec_name.to_string(),
            args: specialized_fields,
            body: Box::new((dummy_body, 0..0)),
            return_type: (specialized_return, 0..0),
        }
    }

    fn specialize_function_at_call(
        &mut self, // Changed to mutable
        func: &TypedFunction,
        args: &[TypedExpr],
    ) -> TypedFunction {
        let mut specialized = func.clone();
        let mut type_map = HashMap::new();

        // Create type mapping from arguments
        for (i, (_, param_ty, _)) in func.args.iter().enumerate() {
            if let Type::Variable(id) = &**param_ty {
                let resolved_ty = self.resolve_deep(args[i].ty.clone());
                type_map.insert(*id, resolved_ty);
            }
        }

        // Apply type mapping to function signature with RESOLUTION
        specialized.args = func
            .args
            .iter()
            .map(|(name, ty, span)| {
                let substituted = self.substitute_type(ty, &type_map);
                let resolved = self.resolve_deep(substituted); // RESOLVE AFTER SUBSTITUTION
                (name.clone(), resolved, span.clone())
            })
            .collect();

        // RESOLVE RETURN TYPE
        let return_type = self.resolve_deep(self.substitute_type(&func.return_type.0, &type_map));
        specialized.return_type.0 = return_type.clone();

        // Generate unique name based on CONCRETE TYPES
        let arg_types: Vec<String> = specialized
            .args
            .iter()
            .map(|(_, ty, _)| type_signature_string(ty))
            .collect();

        println!("{:?}", specialized.args);

        let return_str = type_signature_string(&return_type);
        let signature = format!("{}_{}_to_{}", func.name, arg_types.join("_"), return_str);
        let spec_name = sanitize_identifier(&signature);
        specialized.name = spec_name;

        // Apply substitution and RESOLUTION to function body
        let (body_expr, body_span) = *func.body.clone();
        let substituted_body = self.substitute_in_expr(body_expr, &type_map);
        let resolved_body = self.resolve_expr(substituted_body); // RESOLVE BODY
        specialized.body = Box::new((resolved_body, body_span));

        specialized
    }

    fn substitute_in_expr(
        &self,
        expr: TypedExpr,
        type_map: &HashMap<usize, Arc<Type>>,
    ) -> TypedExpr {
        let new_ty = self.substitute_type(&expr.ty, type_map);

        let new_kind = match expr.kind {
            TypedExprKind::Call { function, args } => {
                let new_function = Box::new(self.substitute_in_expr(*function, type_map));
                let new_args = args
                    .into_iter()
                    .map(|arg| self.substitute_in_expr(arg, type_map))
                    .collect();
                TypedExprKind::Call {
                    function: new_function,
                    args: new_args,
                }
            }
            TypedExprKind::Variable(name) => TypedExprKind::Variable(name),
            TypedExprKind::BinOp {
                operator,
                l_value,
                r_value,
            } => {
                let new_l = Box::new(self.substitute_in_expr(*l_value, type_map));
                let new_r = Box::new(self.substitute_in_expr(*r_value, type_map));
                TypedExprKind::BinOp {
                    operator,
                    l_value: new_l,
                    r_value: new_r,
                }
            }
            TypedExprKind::Let { var, value } => {
                let new_value = Box::new(self.substitute_in_expr(*value, type_map));
                TypedExprKind::Let {
                    var,
                    value: new_value,
                }
            }
            TypedExprKind::IfElse {
                condition,
                if_branch,
                else_branch,
            } => {
                let new_cond = Box::new(self.substitute_in_expr(*condition, type_map));
                let new_if = Box::new(self.substitute_in_expr(*if_branch, type_map));
                let new_else = else_branch.map(|b| Box::new(self.substitute_in_expr(*b, type_map)));
                TypedExprKind::IfElse {
                    condition: new_cond,
                    if_branch: new_if,
                    else_branch: new_else,
                }
            }
            // Add other expression types as needed
            kind => kind,
        };

        TypedExpr {
            kind: new_kind,
            ty: new_ty,
            range: expr.range,
        }
    }

    pub fn resolve_deep(&self, ty: Arc<Type>) -> Arc<Type> {
        let resolved = self.resolve(ty.clone());
        match &*resolved {
            // Handle function types specially for monomorphization
            Type::Function {
                params,
                return_type,
            } => {
                let concrete_params = params
                    .iter()
                    .map(|t| self.resolve_deep(t.clone()))
                    .collect();
                let concrete_return = self.resolve_deep(return_type.clone());

                Arc::new(Type::Function {
                    params: concrete_params,
                    return_type: (concrete_return),
                })
            }

            // Other types get normal resolution
            _ => match &*resolved {
                Type::Variable(_) => resolved,
                Type::Constructor {
                    name,
                    generics,
                    traits,
                } => {
                    let resolved_generics = generics
                        .iter()
                        .map(|t| self.resolve_deep(t.clone()))
                        .collect();
                    Arc::new(Type::Constructor {
                        name: name.clone(),
                        generics: resolved_generics,
                        traits: traits.clone(),
                    })
                }
                Type::Tuple(types) => {
                    let resolved_types =
                        types.iter().map(|t| self.resolve_deep(t.clone())).collect();
                    Arc::new(Type::Tuple(resolved_types))
                }
                Type::Union(types) => {
                    let resolved_types =
                        types.iter().map(|t| self.resolve_deep(t.clone())).collect();
                    Arc::new(Type::Union(resolved_types))
                }
                _ => resolved,
            },
        }
    }
}

/// Sanitizes a string to be a valid identifier
fn sanitize_identifier(name: &str) -> String {
    name.chars()
        .map(|c| {
            if c.is_alphanumeric() || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect()
}

/// Generates a unique string representation of a type for specialization names
fn type_signature_string(ty: &Arc<Type>) -> String {
    match &**ty {
        Type::Constructor { name, generics, .. } => {
            if generics.is_empty() {
                name.clone()
            } else {
                let generics_str = generics
                    .iter()
                    .map(type_signature_string)
                    .collect::<Vec<_>>()
                    .join("_");
                format!("{}<{}>", name, generics_str)
            }
        }
        Type::Variable(i) => format!("T{}", i),
        Type::Function {
            params,
            return_type,
        } => {
            let params_str = params
                .iter()
                .map(type_signature_string)
                .collect::<Vec<_>>()
                .join("_");
            let return_str = type_signature_string(return_type);
            format!("fn_{}_to_{}", params_str, return_str)
        }
        Type::Tuple(types) => {
            let types_str = types
                .iter()
                .map(type_signature_string)
                .collect::<Vec<_>>()
                .join("_");
            format!("tuple_{}", types_str)
        }
        Type::Union(types) => {
            let types_str = types
                .iter()
                .map(type_signature_string)
                .collect::<Vec<_>>()
                .join("_");
            format!("union_{}", types_str)
        }
        _ => format!("{:?}", ty).replace(" ", ""),
    }
}
