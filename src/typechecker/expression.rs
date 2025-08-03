#![allow(unused_imports)]

use crate::ast::Pattern;
use crate::ast::TypedExtern;
use crate::ast::TypedMatchArm;
use crate::ast::TypedPattern;
use crate::typechecker::EnumVariantKind;
use crate::typechecker::EnumVariantKindTy;
use std::collections::HashMap;
use std::collections::HashSet;
use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    ASTNode, AssignOp, BinOp, Expr, Extern, Function, Struct, Type, TypeAnnot, TypedASTNode,
    TypedExpr, TypedExprKind, TypedFunction, TypedStruct, UnOp,
};
use crate::typechecker::{TypeEnv, type_annot_to_type, type_string};
use crate::{t_bool, t_char, t_float, t_int, t_list, t_string, t_unit, tvar};

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
                TypedASTNode::Function(self.function_to_typed_function((&func, span)))
            }
            ASTNode::Extern(func) => {
                TypedASTNode::Extern(self.extern_to_typed_extern((&func, span)))
                // todo!()
            }
            ASTNode::Struct(struct_) => {
                TypedASTNode::Struct(self.struct_to_typed_struct((&struct_, span)))
            }
            ASTNode::Enum(enum_, range) => {
                TypedASTNode::Enum(self.enum_to_typed_enum((&enum_, &range)))
            }
            ASTNode::TypeAlias(_, _) => todo!(),
            ASTNode::Impl(_) => todo!(),
        }
    }

    pub fn extern_to_typed_extern(
        &mut self,
        extern_func: (&Extern, &std::ops::Range<usize>),
    ) -> (TypedExtern, std::ops::Range<usize>) {
        let (extern_func, fun_span) = extern_func;

        let args = extern_func
            .args
            .iter()
            .map(|(ty_annot, range)| {
                (
                    type_annot_to_type(ty_annot), // Convert to Arc<Type>
                    range.clone(),
                )
            })
            .collect::<Vec<_>>();

        let return_type = (
            type_annot_to_type(&extern_func.return_type.0),
            fun_span.clone(),
        );

        let typed_extern = TypedExtern {
            name: extern_func.name.clone(),
            args,
            return_type,
        };

        (typed_extern, fun_span.clone())
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
                    //     (
                    //         TypedExprKind::Index {
                    //             array: Box::new(typed_array),
                    //             index: Box::new(typed_index),
                    //         },
                    //         t_string!()
                    //     )
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
            Expr::Return(inner_expr) => {
                if !self.in_function {
                    panic!("Return statement outside function at {:?}", span);
                }

                if self.return_depth > 0 {
                    panic!("Nested return at {:?}", span);
                }
                self.return_depth += 1;

                let typed_inner = self.expr_to_typed_expr((inner_expr, span));

                self.return_depth -= 1;

                (
                    TypedExprKind::Return(Box::new(typed_inner.clone())),
                    typed_inner.ty,
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

                let typed_arm_0 = &typed_arms[0];

                for typed_arm in &mut typed_arms.iter().skip(1) {
                    self.unify(
                        typed_arm_0.body.ty.clone(),
                        typed_arm.body.ty.clone(),
                        match_span,
                        match_span,
                    );
                }

                self.default_unbound_generics(&match_ty);

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
            Expr::Lambda { args, expression } => {
                // Save current environment
                let old_vars = self.variables.clone();

                // Process arguments
                let mut typed_args = Vec::new();
                let mut param_types = Vec::new();

                for (arg_name, type_annot, arg_span) in args {
                    let arg_ty = if let Some(annot) = type_annot {
                        type_annot_to_type(annot)
                    } else {
                        self.new_typevar()
                    };

                    self.insert_var(arg_name.clone(), arg_ty.clone());
                    typed_args.push((arg_name.clone(), arg_ty.clone(), arg_span.clone()));
                    param_types.push(arg_ty.clone());
                }

                let (expression, body_span) = &**expression;

                // Type check body
                let typed_body = self.expr_to_typed_expr((expression, body_span));
                let return_type = typed_body.ty.clone();

                let lambda_type = self.resolve(Arc::new(Type::Function {
                    params: param_types,
                    return_type: return_type.clone(),
                }));

                // Restore environment
                self.variables = old_vars;

                (
                    TypedExprKind::Lambda {
                        args: typed_args,
                        expression: Box::new(typed_body),
                    },
                    lambda_type,
                )
            }
            Expr::Error => unreachable!(),
        };
        TypedExpr {
            kind: exprkind,
            ty,
            range: span.clone(),
        }
    }

    // Default any unbound generic types to string
    fn default_unbound_generics(&mut self, ty: &Arc<Type>) {
        let resolved = self.resolve(ty.clone());

        if let Type::Constructor { generics, .. } = &*resolved {
            for generic in generics {
                if let Type::Variable(i) = &**generic {
                    // If still unbound, default to string
                    if !self.substitutions.contains_key(i) {
                        self.substitutions.insert(*i, t_string!());
                    }
                }
            }
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
                                (self.check_pattern(subpat, field_type, span), span.clone())
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
                    typed_subpatterns.push((typed_subpattern, span.clone()));
                }

                TypedPattern::Union(typed_subpatterns)
            }
            Pattern::Tuple(_) | Pattern::Error => todo!(),
            Pattern::Guard(pattern, expr) => {
                let typed_pattern = self.check_pattern(&pattern.0, match_ty, &pattern.1);
                let (expr, span) = expr;
                let expr = self.expr_to_typed_expr((expr, span));
                TypedPattern::Guard(
                    Box::new((typed_pattern, pattern.1.clone())),
                    (expr, span.clone()),
                )
            }
        }
    }

    pub fn builtin_macro_evaluation(&mut self, expr: TypedExpr) -> TypedExpr {
        // First recursively process all subexpressions
        let processed_expr = match expr.kind {
            TypedExprKind::Return(expr) => {
                TypedExprKind::Return(Box::new(self.builtin_macro_evaluation(*expr)))
            }
            TypedExprKind::Lambda { args, expression } => TypedExprKind::Lambda {
                args,
                expression: Box::new(self.builtin_macro_evaluation(*expression)),
            },
            TypedExprKind::Array { elements } => TypedExprKind::Array {
                elements: elements
                    .into_iter()
                    .map(|e| self.builtin_macro_evaluation(e))
                    .collect(),
            },
            TypedExprKind::Index { array, index } => TypedExprKind::Index {
                array: Box::new(self.builtin_macro_evaluation(*array)),
                index: Box::new(self.builtin_macro_evaluation(*index)),
            },
            TypedExprKind::Call { function, args } => {
                let func = self.builtin_macro_evaluation(*function);
                let args = args
                    .into_iter()
                    .map(|a| self.builtin_macro_evaluation(a))
                    .collect::<Vec<_>>();

                // Now check if this is a macro call
                if let TypedExprKind::Variable(name) = &func.kind {
                    match name.as_str() {
                        "whattype" => {
                            if args.len() != 1 {
                                return TypedExpr {
                                    kind: TypedExprKind::Call {
                                        function: Box::new(func),
                                        args,
                                    },
                                    ty: expr.ty,
                                    range: expr.range,
                                };
                            }
                            let type_str = type_string(&args[0].ty);
                            return TypedExpr {
                                kind: TypedExprKind::String(type_str),
                                ty: t_string!(),
                                range: expr.range,
                            };
                        }
                        "len" => {
                            if args.len() != 1 {
                                // len() takes exactly one argument
                                return TypedExpr {
                                    kind: TypedExprKind::Call {
                                        function: Box::new(func),
                                        args,
                                    },
                                    ty: expr.ty,
                                    range: expr.range,
                                };
                            }
                            let arg = &args[0];
                            let range = expr.range.clone();

                            // Depending on the argument type, generate the proper IR-compatible expression:
                            match &*arg.ty {
                                Type::Constructor { name, .. } if name == "string" => {
                                    // Replace with call to string_length runtime function
                                    return TypedExpr {
                                        kind: TypedExprKind::Call {
                                            function: Box::new(TypedExpr {
                                                kind: TypedExprKind::Variable(
                                                    "string_length".to_string(),
                                                ),
                                                ty: Type::Function {
                                                    params: vec![t_string!()],
                                                    return_type: t_int!(), // int length
                                                }
                                                .into(),
                                                range: range.clone(),
                                            }),
                                            args: vec![arg.clone()],
                                        },
                                        ty: t_int!(),
                                        range,
                                    };
                                }
                                Type::Constructor { name, .. } if name == "List" => {
                                    // Replace with call to array_length runtime function
                                    return TypedExpr {
                                        kind: TypedExprKind::Call {
                                            function: Box::new(TypedExpr {
                                                kind: TypedExprKind::Variable(
                                                    "array_length".to_string(),
                                                ),
                                                ty: Type::Function {
                                                    params: vec![arg.ty.clone()],
                                                    return_type: t_int!(),
                                                }
                                                .into(),
                                                range: range.clone(),
                                            }),
                                            args: vec![arg.clone()],
                                        },
                                        ty: t_int!(),
                                        range,
                                    };
                                }
                                _ => {
                                    // For unsupported types, fallback to call (which will give error downstream)
                                    return TypedExpr {
                                        kind: TypedExprKind::Call {
                                            function: Box::new(func),
                                            args,
                                        },
                                        ty: expr.ty,
                                        range: expr.range,
                                    };
                                }
                            }
                        }
                        "print" | "println" => {
                            // If no arguments, handle special cases
                            if args.is_empty() {
                                let content = if name == "println" { "\n" } else { "" };
                                return TypedExpr {
                                    kind: TypedExprKind::String(content.to_string()),
                                    ty: t_string!(),
                                    range: expr.range,
                                };
                            }

                            // Convert all arguments to string representations
                            let string_args = args
                                .into_iter()
                                .map(|arg| self.value_to_string(arg))
                                .collect::<Vec<_>>();

                            // Join all strings with space separator
                            let mut joined = if let Some(first) = string_args.first() {
                                first.clone()
                            } else {
                                let range = expr.range.clone();
                                TypedExpr {
                                    kind: TypedExprKind::String("".to_string()),
                                    ty: t_string!(),
                                    range,
                                }
                            };

                            for arg in string_args.iter().skip(1) {
                                let range = expr.range.clone();

                                joined = TypedExpr {
                                    kind: TypedExprKind::BinOp {
                                        operator: BinOp::Add,
                                        l_value: Box::new(joined),
                                        r_value: Box::new(TypedExpr {
                                            kind: TypedExprKind::String(" ".to_string()),
                                            ty: t_string!(),
                                            range: range.clone(),
                                        }),
                                    },
                                    ty: t_string!(),
                                    range: range.clone(),
                                };
                                joined = TypedExpr {
                                    kind: TypedExprKind::BinOp {
                                        operator: BinOp::Add,
                                        l_value: Box::new(joined),
                                        r_value: Box::new(arg.clone()),
                                    },
                                    ty: t_string!(),
                                    range,
                                };
                            }

                            // Add newline for println
                            if name == "println" {
                                let range = expr.range.clone();

                                joined = TypedExpr {
                                    kind: TypedExprKind::BinOp {
                                        operator: BinOp::Add,
                                        l_value: Box::new(joined),
                                        r_value: Box::new(TypedExpr {
                                            kind: TypedExprKind::String("\n".to_string()), // This should create global
                                            ty: t_string!(),
                                            range: range.clone(),
                                        }),
                                    },
                                    ty: t_string!(),
                                    range,
                                };
                            }

                            // Create the actual print call
                            return TypedExpr {
                                kind: TypedExprKind::Call {
                                    function: Box::new(TypedExpr {
                                        kind: TypedExprKind::Variable("print_internal".to_string()),
                                        ty: Type::Function {
                                            params: vec![t_string!()],
                                            return_type: t_unit!(),
                                        }
                                        .into(),
                                        range: func.range,
                                    }),
                                    args: vec![joined],
                                },
                                ty: t_unit!(),
                                range: expr.range,
                            };
                        }
                        _ => {}
                    }
                }

                TypedExprKind::Call {
                    function: Box::new(func),
                    args,
                }
            }
            TypedExprKind::StructAccess {
                struct_val,
                field_name,
            } => TypedExprKind::StructAccess {
                struct_val: Box::new(self.builtin_macro_evaluation(*struct_val)),
                field_name,
            },
            TypedExprKind::MethodCall {
                struct_val,
                method_name,
                args,
            } => TypedExprKind::MethodCall {
                struct_val: Box::new(self.builtin_macro_evaluation(*struct_val)),
                method_name,
                args: args
                    .into_iter()
                    .map(|a| self.builtin_macro_evaluation(a))
                    .collect(),
            },
            TypedExprKind::BinOp {
                operator,
                l_value,
                r_value,
            } => TypedExprKind::BinOp {
                operator,
                l_value: Box::new(self.builtin_macro_evaluation(*l_value)),
                r_value: Box::new(self.builtin_macro_evaluation(*r_value)),
            },
            TypedExprKind::Assign {
                l_value,
                r_value,
                assign_op,
            } => TypedExprKind::Assign {
                l_value: Box::new(self.builtin_macro_evaluation(*l_value)),
                r_value: Box::new(self.builtin_macro_evaluation(*r_value)),
                assign_op,
            },
            TypedExprKind::UnOp { unop, expression } => TypedExprKind::UnOp {
                unop,
                expression: Box::new(self.builtin_macro_evaluation(*expression)),
            },
            TypedExprKind::Do { expressions } => TypedExprKind::Do {
                expressions: expressions
                    .into_iter()
                    .map(|e| self.builtin_macro_evaluation(e))
                    .collect(),
            },
            TypedExprKind::Let { var, value } => TypedExprKind::Let {
                var,
                value: Box::new(self.builtin_macro_evaluation(*value)),
            },
            TypedExprKind::IfElse {
                condition,
                if_branch,
                else_branch,
            } => TypedExprKind::IfElse {
                condition: Box::new(self.builtin_macro_evaluation(*condition)),
                if_branch: Box::new(self.builtin_macro_evaluation(*if_branch)),
                else_branch: else_branch.map(|b| Box::new(self.builtin_macro_evaluation(*b))),
            },
            TypedExprKind::Tuple(elements) => TypedExprKind::Tuple(
                elements
                    .into_iter()
                    .map(|e| self.builtin_macro_evaluation(e))
                    .collect(),
            ),
            TypedExprKind::EnumVariant {
                enum_name,
                variant_name,
                fields,
            } => TypedExprKind::EnumVariant {
                enum_name,
                variant_name,
                fields: fields
                    .into_iter()
                    .map(|(name, expr)| (name, self.builtin_macro_evaluation(expr)))
                    .collect(),
            },
            TypedExprKind::Match { expr, arms } => TypedExprKind::Match {
                expr: Box::new(self.builtin_macro_evaluation(*expr)),
                arms: arms
                    .into_iter()
                    .map(|arm| TypedMatchArm {
                        pattern: arm.pattern,
                        body: Box::new(self.builtin_macro_evaluation(*arm.body)),
                    })
                    .collect(),
            },
            // For simple expressions that don't have subexpressions
            TypedExprKind::Bool(_)
            | TypedExprKind::Int(_)
            | TypedExprKind::Float(_)
            | TypedExprKind::String(_)
            | TypedExprKind::Variable(_)
            | TypedExprKind::Error => expr.kind,
        };

        TypedExpr {
            kind: processed_expr,
            ty: expr.ty,
            range: expr.range,
        }
    }

    fn value_to_string(&self, expr: TypedExpr) -> TypedExpr {
        match expr.ty.as_ref() {
            Type::Constructor { name, .. } if name == "string" => expr,
            Type::Constructor { name, .. } => {
                let range = expr.range.clone();
                match name.as_str() {
                    "int" => TypedExpr {
                        kind: TypedExprKind::Call {
                            function: Box::new(TypedExpr {
                                kind: TypedExprKind::Variable("int_to_string".to_string()),
                                ty: Type::Function {
                                    params: vec![t_int!()],
                                    return_type: t_string!(),
                                }
                                .into(),
                                range: range.clone(),
                            }),
                            args: vec![expr],
                        },
                        ty: t_string!(),
                        range,
                    },
                    "float" => TypedExpr {
                        kind: TypedExprKind::Call {
                            function: Box::new(TypedExpr {
                                kind: TypedExprKind::Variable("float_to_string".to_string()),
                                ty: Type::Function {
                                    params: vec![t_float!()],
                                    return_type: t_string!(),
                                }
                                .into(),
                                range: range.clone(),
                            }),
                            args: vec![expr],
                        },
                        ty: t_string!(),
                        range,
                    },
                    "bool" => TypedExpr {
                        kind: TypedExprKind::Call {
                            function: Box::new(TypedExpr {
                                kind: TypedExprKind::Variable("bool_to_string".to_string()),
                                ty: Type::Function {
                                    params: vec![t_bool!()],
                                    return_type: t_string!(),
                                }
                                .into(),
                                range: range.clone(),
                            }),
                            args: vec![expr],
                        },
                        ty: t_string!(),
                        range,
                    },
                    "char" => TypedExpr {
                        kind: TypedExprKind::Call {
                            function: Box::new(TypedExpr {
                                kind: TypedExprKind::Variable("char_to_string".to_string()),
                                ty: Type::Function {
                                    params: vec![t_char!()],
                                    return_type: t_string!(),
                                }
                                .into(),
                                range: range.clone(),
                            }),
                            args: vec![expr],
                        },
                        ty: t_string!(),
                        range,
                    },
                    // For structs and enums, show type name as string literal
                    _ => {
                        let type_str = format!("<{}>", name);
                        TypedExpr {
                            kind: TypedExprKind::String(type_str),
                            ty: t_string!(),
                            range: expr.range,
                        }
                    }
                }
            }
            // Handle other types (tuples, functions, etc.)
            _ => {
                let type_str = type_string(&expr.ty);
                TypedExpr {
                    kind: TypedExprKind::String(format!("<{}>", type_str)),
                    ty: t_string!(),
                    range: expr.range,
                }
            }
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
