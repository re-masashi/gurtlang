#![allow(unused_imports)]

use crate::ast::Pattern;
use crate::ast::TypedMatchArm;
use crate::ast::TypedPattern;
use crate::typechecker::EnumVariantKind;
use crate::typechecker::EnumVariantKindTy;
use std::collections::HashMap;
use std::collections::HashSet;
use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    ASTNode, AssignOp, BinOp, Expr, Function, Struct, Type, TypeAnnot, TypedASTNode, TypedExpr,
    TypedExprKind, TypedFunction, TypedStruct, UnOp,
};
use crate::typechecker::{TypeEnv, type_annot_to_type, type_string};
use crate::{t_bool, t_float, t_int, t_list, t_string, t_unit, tvar};

impl TypeEnv<'_> {
    pub fn ast_to_typed_ast(&mut self, ast: Vec<(ASTNode, Range<usize>)>) -> Vec<TypedASTNode> {
        ast.into_iter()
            .map(|(node, span)| self.node_to_typed_node(node, &span))
            .collect()
    }

    pub fn node_to_typed_node(&mut self, node: ASTNode, span: &Range<usize>) -> TypedASTNode {
        match node {
            ASTNode::Error => unreachable!(),
            ASTNode::Expr(expr) => {
                let (expr, span) = expr;
                let typed_expr = self.expr_to_typed_expr((&expr, &span));
                TypedASTNode::Expr((typed_expr, span))
            }
            ASTNode::Function(func) => {
                let (typed_fun, span) = self.function_to_typed_function((&func, span));
                TypedASTNode::Function((typed_fun, span))
            }
            ASTNode::Struct(struct_) => {
                let dummy_span = 0..0; // Dummy span since structs don't have spans
                let (typed_struct, span) = self.struct_to_typed_struct((&struct_, &dummy_span));
                TypedASTNode::Struct((typed_struct, span))
            }
            ASTNode::Enum(enum_, range) => {
                let (typed_enum, span) = self.enum_to_typed_enum((&enum_, &range));
                TypedASTNode::Enum((typed_enum, span))
            }
            ASTNode::TypeAlias(_, _) => todo!(),
        }
    }

    pub fn expr_to_typed_expr(&mut self, expr: (&Expr, &Range<usize>)) -> TypedExpr {
        let (expr, span) = expr;
        let (exprkind, ty) = match expr {
            Expr::Bool(b) => (TypedExprKind::Bool(*b), t_bool!()),
            Expr::Int(i) => (TypedExprKind::Int(*i), t_int!()),
            Expr::Float(f) => (TypedExprKind::Float(*f), t_float!()),
            Expr::String(s) => (TypedExprKind::String(s.to_string()), t_string!()),
            Expr::Variable(name) => {
                let ty = if self.var_exists(name) {
                    self.get_var(name).unwrap()
                } else {
                    let new_typevar = self.new_typevar();
                    self.insert_var(name.clone(), new_typevar.clone());
                    new_typevar
                };
                (TypedExprKind::Variable(name.to_string()), ty)
            }
            Expr::Array { elements } => {
                if elements.is_empty() {
                    let new_typevar = self.new_typevar();

                    (
                        TypedExprKind::Array { elements: vec![] },
                        t_list!(new_typevar),
                    )
                } else if elements.len() == 1 {
                    let (elem, span) = &elements[0];
                    let new_elem = self.expr_to_typed_expr((elem, span));
                    let ty = new_elem.ty.clone();
                    (
                        TypedExprKind::Array {
                            elements: vec![new_elem],
                        },
                        t_list!(ty.clone()),
                    )
                } else {
                    let (first_elem, first_span) = &elements[0];
                    let new_first_elem = self.expr_to_typed_expr((first_elem, first_span));

                    let val_ty = new_first_elem.ty.clone();
                    let val_span = new_first_elem.range.clone();
                    let mut new_elems = vec![new_first_elem];

                    if let Some(elem) = elements.get(1) {
                        let (elem, span) = elem;
                        let typed = self.expr_to_typed_expr((elem, span));
                        let _ =
                            self.unify(val_ty.clone(), typed.ty.clone(), &val_span, &typed.range);
                        new_elems.push(typed);

                        for (elem, span) in elements.iter().skip(2) {
                            // let (elem, span) = &elements[i];
                            let typed = self.expr_to_typed_expr((elem, span));
                            let _ = self.unify(
                                val_ty.clone(),
                                typed.ty.clone(),
                                &val_span,
                                &typed.range,
                            );
                            new_elems.push(typed);
                        }
                    }

                    let ty = &new_elems[0].ty.clone();
                    (
                        TypedExprKind::Array {
                            elements: new_elems,
                        },
                        t_list!(ty.clone()),
                    )
                }
            }
            Expr::Index { array, index } => {
                // TODO: add custom indexing through traits
                let (expr, _span) = &**array;
                let (index, span) = &**index;

                let typed_array = self.expr_to_typed_expr((expr, span));
                let typed_index = self.expr_to_typed_expr((index, span));

                match &*typed_array.ty.clone() {
                    Type::Constructor {
                        name,
                        generics,
                        traits: _,
                    } if name == "List" => (
                        TypedExprKind::Index {
                            array: Box::new(typed_array),
                            index: Box::new(typed_index),
                        },
                        generics[0].clone(),
                    ),
                    // Type::Constructor { name, .. } if name == "string" => {
                    //     (t_string!())
                    // }
                    x => todo!("bs. who would even index sumn that's not an array. {:?}", x),
                }
            }
            Expr::Call { function, args } => {
                let (fun, span) = &**function;
                let typed_fun = self.expr_to_typed_expr((fun, span));

                match &*typed_fun.ty.clone() {
                    Type::Variable(_) => {
                        let new_args = args
                            .iter()
                            .map(|arg| {
                                let (arg, span) = arg;
                                self.expr_to_typed_expr((arg, span))
                            })
                            .collect::<Vec<_>>();
                        let ty = typed_fun.ty.clone();
                        (
                            TypedExprKind::Call {
                                function: Box::new(typed_fun),
                                args: new_args,
                            },
                            ty,
                        )
                    }
                    Type::Function {
                        params: _,
                        return_type,
                    } => {
                        let new_args = args
                            .iter()
                            .map(|arg| {
                                let (arg, span) = arg;
                                self.expr_to_typed_expr((arg, span))
                            })
                            .collect::<Vec<_>>();
                        (
                            TypedExprKind::Call {
                                function: Box::new(typed_fun),
                                args: new_args,
                            },
                            return_type.clone(),
                        )
                    }
                    _ => panic!("NOT A FUNCTION"),
                }
            }
            Expr::StructAccess {
                struct_val,
                field_name,
            } => {
                let (struct_expr, struct_span) = &**struct_val;
                let typed_struct = self.expr_to_typed_expr((struct_expr, struct_span));

                // Resolve the struct type
                let resolved_ty = self.resolve(typed_struct.ty.clone());

                // Extract struct name and generics
                let (struct_name, _generics) = match &*resolved_ty {
                    Type::Constructor { name, generics, .. } => (name, generics),
                    _ => {
                        // Report error: not a struct type
                        // self.add_error(
                        //     ReportKind::Error,
                        panic!(
                            "Expected a struct type, found {}",
                            type_string(&resolved_ty)
                        );
                        //     *span,
                        // );
                        // return TypedExpr {
                        //     kind: TypedExprKind::Error,
                        //     ty: t_unit!(),
                        //     range: span.clone(),
                        // };
                    }
                };

                // Look up struct definition
                if let Some(struct_ty) = self.get_struct(struct_name) {
                    // Find the field in struct definition
                    if let Some((_, field_ty)) =
                        struct_ty.fields.iter().find(|(name, _)| name == field_name)
                    {
                        return TypedExpr {
                            kind: TypedExprKind::StructAccess {
                                struct_val: Box::new(typed_struct),
                                field_name: field_name.clone(),
                            },
                            ty: field_ty.clone(),
                            range: span.clone(),
                        };
                    } else {
                        // Report error: field not found
                        // self.add_error(
                        //     ReportKind::Error,
                        panic!("Struct '{}' has no field '{}'", struct_name, field_name);
                        //     *span,
                        // );
                    }
                } else {
                    // Report error: struct not found
                    // self.add_error(
                    //     ReportKind::Error,
                    panic!("Struct '{}' not found", struct_name);
                    //     *span,
                    // );
                }

                // (TypedExprKind::Error, t_unit!())
            }

            // Expr::MethodCall {
            //     struct_val,
            //     method_name,
            //     args,
            // } => {
            //     let (struct_expr, struct_span) = &**struct_val;
            //     let typed_struct = self.expr_to_typed_expr((struct_expr, struct_span));

            //     // Resolve the struct type
            //     let resolved_ty = self.resolve(typed_struct.ty.clone());

            //     // Extract struct name
            //     let struct_name = match &*resolved_ty {
            //         Type::Constructor { name, .. } => name,
            //         _ => {
            //             // Report error: not a struct type
            //             // self.add_error(
            //             //     ReportKind::Error,
            //                 panic!("Expected a struct type, found {}", type_string(&resolved_ty));
            //             //     *span,
            //             // );
            //             // return TypedExpr {
            //             //     kind: TypedExprKind::Error,
            //             //     ty: t_unit!(),
            //             //     range: span.clone(),
            //             // };
            //         }
            //     };

            //     // Construct full method name: StructName.method_name
            //     let full_method_name = format!("{}.{}", struct_name, method_name);

            //     // Look up method in environment
            //     if let Some(method_ty) = self.get_var(&full_method_name) {
            //         let resolved_method_ty = self.resolve(method_ty.clone());

            //         if let Type::Function { params, return_type } = &*resolved_method_ty {
            //             // Check if the first parameter is the struct type
            //             if let Some(first_param) = params.first() {
            //                 let _ = self.unify(
            //                     first_param.clone(),
            //                     resolved_ty,
            //                     struct_span,
            //                     struct_span,
            //                 );
            //             }

            //             // Type check arguments
            //             let mut typed_args = vec![];
            //             for (i, arg) in args.iter().enumerate() {
            //                 let (arg_expr, arg_span) = arg;
            //                 let typed_arg = self.expr_to_typed_expr((arg_expr, arg_span));

            //                 // Check against method parameter type
            //                 if let Some(param_ty) = params.get(i + 1) { // Skip first param (self)
            //                     let _ = self.unify(
            //                         param_ty.clone(),
            //                         typed_arg.ty.clone(),
            //                         arg_span,
            //                         arg_span,
            //                     );
            //                 }

            //                 typed_args.push(typed_arg);
            //             }

            //             return TypedExpr {
            //                 kind: TypedExprKind::MethodCall {
            //                     struct_val: Box::new(typed_struct),
            //                     method_name: method_name.clone(),
            //                     args: typed_args,
            //                 },
            //                 ty: return_type.clone(),
            //                 range: span.clone(),
            //             };
            //         }
            //     }

            //     // Report error: method not found
            //     // self.errors.push(
            //     //     ReportKind::Error,
            //         panic!("Method '{}' not found for struct '{}'", method_name, struct_name);
            //     //     *span,
            //     // );
            //     panic!("Method '{}' not found for struct '{}'", method_name, struct_name);

            //     // (TypedExprKind::Error, t_unit!())
            // }
            Expr::MethodCall { .. } => todo!(),
            Expr::BinOp {
                operator,
                l_value,
                r_value,
            } => {
                let (l_expr, l_span) = &**l_value;
                let l_value_typed = self.expr_to_typed_expr((l_expr, l_span));

                let (r_expr, r_span) = &**r_value;
                let r_value_typed = self.expr_to_typed_expr((r_expr, r_span));

                let ty = match (operator, &*l_value_typed.ty, &*l_value_typed.ty) {
                    (
                        BinOp::Add,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "int" && name_r == "int" => {
                        t_int!()
                    }
                    (
                        BinOp::Add,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "float" && name_r == "int" => {
                        t_float!()
                    }
                    (
                        BinOp::Add,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "int" && name_r == "float" => {
                        t_float!()
                    }
                    (
                        BinOp::Add,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "string" || name_r == "string" => {
                        t_string!()
                    }

                    (
                        BinOp::Sub,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "int" && name_r == "int" => {
                        t_int!()
                    }
                    (
                        BinOp::Sub,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "float" && name_r == "int" => {
                        t_float!()
                    }
                    (
                        BinOp::Sub,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "int" && name_r == "float" => {
                        t_float!()
                    }

                    (
                        BinOp::Mul,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "int" && name_r == "int" => {
                        t_int!()
                    }
                    (
                        BinOp::Mul,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "float" && name_r == "int" => {
                        t_float!()
                    }
                    (
                        BinOp::Mul,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "int" && name_r == "float" => {
                        t_float!()
                    }
                    (
                        BinOp::Mul,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if (name_l == "string" && name_r == "int")
                        || (name_l == "int" && name_r == "string") =>
                    {
                        t_string!()
                    }

                    (
                        BinOp::Div,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "int" && name_r == "int" => {
                        t_float!()
                    }
                    (
                        BinOp::Div,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "float" && name_r == "int" => {
                        t_float!()
                    }
                    (
                        BinOp::Div,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "int" && name_r == "float" => {
                        t_float!()
                    }
                    (BinOp::Eq, ..) => {
                        t_bool!()
                    }
                    (BinOp::LessEq, ..) => {
                        t_bool!()
                    }
                    (BinOp::GreaterEq, ..) => {
                        t_bool!()
                    }
                    (BinOp::NotEq, ..) => {
                        t_bool!()
                    }

                    _ => {
                        let _ = self.unify(
                            l_value_typed.ty.clone(),
                            r_value_typed.ty.clone(),
                            l_span,
                            r_span,
                        );
                        l_value_typed.ty.clone()
                    }
                };

                (
                    TypedExprKind::BinOp {
                        operator: operator.clone(),
                        l_value: Box::new(l_value_typed),
                        r_value: Box::new(r_value_typed),
                    },
                    ty.clone(),
                )
            }
            Expr::UnOp { unop, expression } => {
                let (expr, span) = &**expression;
                let typed_expr = self.expr_to_typed_expr((expr, span));
                let ty = typed_expr.ty.clone();
                if matches!(unop, UnOp::Not) {
                    let _ = self.unify(ty.clone(), t_bool!(), span, span);
                }
                (
                    TypedExprKind::UnOp {
                        unop: unop.clone(),
                        expression: Box::new(typed_expr),
                    },
                    ty,
                )
            }
            Expr::Assign {
                l_value,
                r_value,
                assign_op,
            } => {
                let (l_expr, l_span) = &**l_value;
                let l_value_typed = self.expr_to_typed_expr((l_expr, l_span));

                let is_valid = matches!(
                    l_value_typed.kind,
                    TypedExprKind::Variable(_)
                        | TypedExprKind::Index { .. }
                        | TypedExprKind::StructAccess { .. }
                );

                if !is_valid {
                    todo!("Invalid assignment target");
                }

                let (r_expr, r_span) = &**r_value;
                let r_value_typed = self.expr_to_typed_expr((r_expr, r_span));

                // let _ty = match (assign_op, &*l_value_typed.ty, &*l_value_typed.ty) {
                //     (AssignOp::Assign, Type::Variable(_), Type::Variable(_)) => {

                //     }
                //     _=>todo!()
                // };

                let ty = r_value_typed.ty.clone();

                let _ = self.unify(
                    l_value_typed.ty.clone(),
                    r_value_typed.ty.clone(),
                    l_span,
                    r_span,
                );

                (
                    TypedExprKind::Assign {
                        assign_op: assign_op.clone(),
                        l_value: Box::new(l_value_typed),
                        r_value: Box::new(r_value_typed),
                    },
                    ty.clone(),
                )
            }
            Expr::Do { expressions } => {
                let mut typed_exprs = vec![];
                for expression in expressions {
                    let (expr, span) = expression;
                    typed_exprs.push(self.expr_to_typed_expr((expr, span)));
                }
                let Some(ex) = typed_exprs.last() else {
                    todo!()
                };
                let ty = ex.ty.clone();
                (
                    TypedExprKind::Do {
                        expressions: typed_exprs,
                    },
                    ty,
                )
            }
            Expr::Let {
                var,
                type_annot,
                value,
            } => {
                let (val_expr, val_span) = &**value;
                let typed_val = self.expr_to_typed_expr((val_expr, val_span));

                let (var_ty, var_span) = match type_annot {
                    Some((annot, span)) => (type_annot_to_type(annot), span),
                    None => (typed_val.ty.clone(), val_span),
                };

                self.unify(var_ty.clone(), typed_val.ty.clone(), var_span, val_span);

                let ty = typed_val.ty.clone();

                self.insert_var(var.clone(), var_ty.clone());

                // let _ = self.unify(var_ty, typed_val.clone(), val_span, var_span);

                (
                    TypedExprKind::Let {
                        var: var.clone(),
                        value: Box::new(typed_val),
                    },
                    ty,
                )
            }
            Expr::IfElse {
                condition,
                if_branch,
                else_branch,
            } => {
                let (condition, condition_span) = &**condition;
                let (if_branch, if_branch_span) = &**if_branch;

                let typed_condition = self.expr_to_typed_expr((condition, condition_span));
                let typed_if_branch = self.expr_to_typed_expr((if_branch, if_branch_span));

                let _ = self.unify(
                    typed_condition.ty.clone(),
                    t_bool!(),
                    condition_span,
                    condition_span,
                );

                match else_branch {
                    Some(else_branch) => {
                        let ty = typed_if_branch.ty.clone();
                        let (else_branch, else_branch_span) = &**else_branch;
                        let typed_else_branch =
                            self.expr_to_typed_expr((else_branch, else_branch_span));
                        let _ = self.unify(
                            typed_if_branch.ty.clone(),
                            typed_else_branch.ty.clone(),
                            if_branch_span,
                            else_branch_span,
                        );
                        (
                            TypedExprKind::IfElse {
                                condition: Box::new(typed_condition),
                                if_branch: Box::new(typed_if_branch),
                                else_branch: Some(Box::new(typed_else_branch)),
                            },
                            ty,
                        )
                    }
                    None => (
                        TypedExprKind::IfElse {
                            condition: Box::new(typed_condition),
                            if_branch: Box::new(typed_if_branch),
                            else_branch: None,
                        },
                        t_unit!(),
                    ),
                }
            }
            Expr::Tuple(expressions) => {
                let mut typed_exprs = vec![];
                let mut types = vec![];

                for expression in expressions {
                    let (expr, span) = expression;
                    let typed = self.expr_to_typed_expr((expr, span));
                    types.push(typed.ty.clone());
                    typed_exprs.push(typed);
                }
                (
                    TypedExprKind::Tuple(typed_exprs),
                    Arc::new(Type::Tuple(types)),
                )
            }
            Expr::EnumVariant {
                enum_name,
                variant_name,
                fields,
                range: _,
            } => {
                let enum_ty = self
                    .enums
                    .get(enum_name)
                    .unwrap_or_else(|| panic!("Enum {enum_name} not found"))
                    .clone();
                let variant_ty_info = enum_ty
                    .variants
                    .get(variant_name)
                    .expect("Variant not found");

                let variant_kind = &variant_ty_info.kind;
                let variant_ty = &variant_ty_info.ty;

                let mut typed_fields = vec![];
                // let mut field_types: Vec<i32> = vec![];

                match variant_kind {
                    EnumVariantKindTy::Unit => {
                        if !fields.is_empty() {
                            panic!(
                                "Variant {} expects 0 fields, got {}",
                                variant_name,
                                fields.len()
                            );
                            // Report error: expected 0 fields
                        }
                    }
                    EnumVariantKindTy::Tuple(field_types) => {
                        for (i, (field_name_opt, (field_expr, field_span))) in
                            fields.iter().enumerate()
                        {
                            if field_name_opt.is_some() {
                                panic!(
                                    "Variant {} expects {} fields, got {}",
                                    variant_name,
                                    field_types.len(),
                                    fields.len()
                                )
                            }
                            if let Some(field_type) = field_types.get(i) {
                                let typed_expr = self.expr_to_typed_expr((field_expr, field_span));
                                self.unify(
                                    typed_expr.ty.clone(),
                                    field_type.clone(),
                                    // type_annot_to_type(&field_type.clone()),
                                    field_span,
                                    field_span,
                                );
                                typed_fields.push((None, typed_expr));
                            }
                        }
                    }
                    EnumVariantKindTy::Struct(field_specs) => {
                        let field_map: HashMap<_, _> = field_specs
                            .iter()
                            .map(|(name, ty)| (name.clone(), ty.clone()))
                            .collect();

                        for (field_name_opt, (field_expr, field_span)) in fields {
                            let field_name = field_name_opt.as_ref().expect("Field name missing");
                            if let Some(field_type) = field_map.get(field_name) {
                                let typed_expr = self.expr_to_typed_expr((field_expr, field_span));
                                self.unify(
                                    typed_expr.ty.clone(),
                                    field_type.clone(),
                                    field_span,
                                    field_span,
                                );
                                typed_fields.push((Some(field_name.clone()), typed_expr));
                            } else {
                                // Report error: unknown field
                                panic!("UNKNOWN FIELD");
                            }
                        }
                    }
                }

                let kind = TypedExprKind::EnumVariant {
                    enum_name: enum_name.clone(),
                    variant_name: variant_name.clone(),
                    fields: typed_fields,
                };
                (kind, variant_ty.clone())
            }
            Expr::Match {
                expr: match_expr,
                arms,
                range: _,
            } => {
                let (match_expr_inner, match_span) = &**match_expr;
                let typed_match_expr = self.expr_to_typed_expr((match_expr_inner, match_span));
                let match_ty = typed_match_expr.ty.clone();

                let mut typed_arms = vec![];
                let mut arm_tys = vec![];

                for arm in arms {
                    let old_vars = self.variables.clone();
                    let typed_pattern = self.check_pattern(&arm.pattern, &match_ty, &arm.range);
                    match typed_pattern {
                        TypedPattern::Variable(_, ref ty) => {
                            self.unify(ty.clone(), match_ty.clone(), match_span, match_span)
                        }
                        TypedPattern::Literal(ref exp) => {
                            self.unify(exp.ty.clone(), match_ty.clone(), match_span, match_span)
                        }
                        _ => false,
                    };

                    let (body_expr, body_span) = &*arm.body;
                    let typed_body = self.expr_to_typed_expr((body_expr, body_span));

                    typed_arms.push(TypedMatchArm {
                        pattern: typed_pattern,
                        body: Box::new(typed_body.clone()),
                    });
                    arm_tys.push(typed_body.ty.clone());

                    self.variables = old_vars;
                }

                // if let Type::Constructor { name,  .. } = &*match_ty {
                //     if let Some(enum_ty) = self.enums.get(name) {
                //         let covered_variants: HashSet<_> = arms.iter().filter_map(|arm| {
                //             if let Pattern::EnumVariant { variant_name, .. } = &arm.pattern {
                //                 Some(variant_name.to_string())
                //             } else {
                //                 None
                //             }
                //         }).collect();

                //         let all_variants: HashSet<_> = enum_ty.variants.keys().collect();

                //         if covered_variants.len() < all_variants.len() {
                //             let missing: Vec<_> = all_variants.difference(&covered_variants).collect();
                //             // self.add_error(
                //                 // ReportKind::Error,
                //                 panic!(
                //                     "Match is not exhaustive. Missing variant{}:",
                //                     if missing.len() > 1 { "s" } else { "" },
                //                     // missing.join(", ")
                //                 );
                //                 // *span,
                //             // );
                //         }
                //     }
                // }

                let return_ty = if let Some(first_ty) = arm_tys.first() {
                    for ty in arm_tys.iter().skip(1) {
                        self.unify(first_ty.clone(), ty.clone(), &arms[0].range, &arms[1].range);
                    }
                    first_ty.clone()
                } else {
                    t_unit!()
                };

                let kind = TypedExprKind::Match {
                    expr: Box::new(typed_match_expr),
                    arms: typed_arms,
                };
                (kind, return_ty)
            }
            Expr::Error => unreachable!(),
        };
        TypedExpr {
            kind: exprkind,
            ty,
            range: span.clone(),
        }
    }

    fn check_pattern(
        &mut self,
        pattern: &Pattern,
        match_ty: &Arc<Type>,
        pattern_span: &Range<usize>,
    ) -> TypedPattern {
        match pattern {
            Pattern::Variable(name) => {
                self.insert_var(name.clone(), match_ty.clone());
                TypedPattern::Variable(name.clone(), match_ty.clone())
            }
            Pattern::EnumVariant {
                enum_name,
                variant_name,
                subpatterns,
            } => {
                let enum_name = enum_name.as_ref().expect("Enum name missing");
                let Some(enum_ty) = self.enums.get(enum_name) else {
                    // self.add_error(
                    // ReportKind::Error,
                    panic!("Enum '{}' not found", enum_name);
                    // *pattern_span,
                    // );
                    // return TypedPattern::Error;
                };
                let enum_ty = enum_ty.clone();

                let Some(variant_ty_info) = enum_ty.variants.get(variant_name) else {
                    // self.add_error(
                    // ReportKind::Error,
                    panic!(
                        "Variant '{}' not found in enum '{}'",
                        variant_name, enum_name
                    );
                    // *pattern_span,
                    // );
                    // return TypedPattern::Error;
                };

                // Unify the expected type with the variant type
                if !self.unify(
                    match_ty.clone(),
                    variant_ty_info.ty.clone(),
                    pattern_span,
                    pattern_span,
                ) {
                    // self.add_error(
                    //     ReportKind::Error,
                    panic!(
                        "Pattern type {} does not match expected type {}",
                        type_string(&variant_ty_info.ty),
                        type_string(match_ty)
                    );
                    //     *pattern_span,
                    // );
                }

                // Check subpatterns
                let typed_subpatterns = match &variant_ty_info.kind.clone() {
                    EnumVariantKindTy::Unit => {
                        if !subpatterns.is_empty() {
                            // self.add_error(
                            // ReportKind::Error,
                            panic!(
                                "Variant {} expects 0 fields, got {}",
                                variant_name,
                                subpatterns.len()
                            );
                            // *pattern_span,
                            // );
                        }
                        vec![]
                    }
                    EnumVariantKindTy::Tuple(field_types) => {
                        if subpatterns.len() != field_types.len() {
                            // self.add_error(
                            // ReportKind::Error,
                            panic!(
                                "Variant {} expects {} fields, got {}",
                                variant_name,
                                field_types.len(),
                                subpatterns.len()
                            )
                            // *pattern_span,
                            // );
                        }

                        subpatterns
                            .iter()
                            .zip(field_types)
                            .map(|((subpat, span), field_type)| {
                                self.check_pattern(subpat, field_type, span)
                            })
                            .collect()
                    }
                    EnumVariantKindTy::Struct(field_specs) => {
                        let _field_map: HashMap<_, _> = field_specs
                            .iter()
                            .map(|(name, ty)| (name.clone(), ty.clone()))
                            .collect();

                        // subpatterns
                        //     .iter()
                        //     .zip(field_specs)
                        //     .map(|((subpat, span), field_type)| {
                        //         self.check_pattern(subpat, field_type, span)
                        //     })
                        //     .collect()
                        // subpatterns
                        vec![]
                    }
                };

                TypedPattern::EnumVariant {
                    enum_name: enum_name.clone(),
                    variant_name: variant_name.clone(),
                    subpatterns: typed_subpatterns,
                }
            }
            Pattern::Wildcard => TypedPattern::Wildcard,
            Pattern::Literal(expr) => {
                // let (expr, span) = expr;
                let typed_expr = self.expr_to_typed_expr((expr, pattern_span));

                // Create a concrete expected type by resolving
                let concrete_expected = self.resolve(match_ty.clone());

                // Unify literal type with expected type
                if !self.unify(
                    typed_expr.ty.clone(),
                    concrete_expected.clone(),
                    pattern_span,
                    pattern_span,
                ) {
                    // self.add_error(
                    //     ReportKind::Error,
                    panic!(
                        "Literal type {} does not match expected type {}",
                        type_string(&typed_expr.ty),
                        type_string(&concrete_expected)
                    )
                    //     *pattern_span,
                    // );
                }

                TypedPattern::Literal(typed_expr)
            }
            Pattern::Union(subpatterns) => {
                // Process each subpattern independently
                let mut typed_subpatterns = Vec::new();

                for (subpattern, span) in subpatterns {
                    let typed_subpattern = self.check_pattern(subpattern, match_ty, span);
                    typed_subpatterns.push(typed_subpattern);
                }

                TypedPattern::Union(typed_subpatterns)
            }
            Pattern::Tuple(_) | Pattern::Error => todo!(),
        }
    }
}

pub fn typed_pattern_to_type(pat: TypedPattern) -> Arc<Type> {
    match pat {
        TypedPattern::Variable(_, ty) => ty.clone(),
        TypedPattern::Literal(exp) => exp.ty.clone(),
        _ => todo!(),
    }
}
