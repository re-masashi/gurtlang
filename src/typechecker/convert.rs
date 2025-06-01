#![allow(unused_imports)]

use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    BinOp,
    AssignOp,
    ASTNode, Expr, Function, Struct, Type, TypeAnnot, TypedASTNode, TypedExpr, TypedExprKind,
    TypedFunction, TypedStruct,
};
use crate::typechecker::TypeEnv;
use crate::{t_bool, t_float, t_int, t_list, t_string, tvar, t_unit};

impl TypeEnv<'_> {
    pub fn ast_to_typed_ast(&mut self, ast: Vec<ASTNode>, _span: &Range<usize>) -> Vec<TypedASTNode> {
        ast.into_iter().map(|node| self.node_to_typed_node(node, _span)).collect()
    }

    pub fn node_to_typed_node(&mut self, node: ASTNode, span: &Range<usize>) -> TypedASTNode {
        match node {
            ASTNode::Error => unreachable!(),
            ASTNode::Expr(expr) => {
                let (expr, span) = expr;
                let typed_expr = self.expr_to_typed_expr((&expr, &span));
                TypedASTNode::Expr((typed_expr, span))
            },
            ASTNode::Function(func) => {
                let (typed_fun, span) = self.function_to_typed_function((&func, &span));
                TypedASTNode::Function((typed_fun, span))
            },
            ASTNode::Struct(struct_) => {
                let dummy_span = 0..0; // Dummy span since structs don't have spans
                let (typed_struct, span) = self.struct_to_typed_struct((&struct_, &dummy_span));
                TypedASTNode::Struct((typed_struct, span))
            }
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
                    self.insert_var(name.clone(), tvar!(self.variables.len() + 1));
                    tvar!(self.variables.len())
                };
                (TypedExprKind::Variable(name.to_string()), ty)
            }
            Expr::Array { elements } => {
                if elements.is_empty() {
                    todo!("idek what to do here ts just stupid")
                } else {
                    // let first_elem = &elements[0];
                    let mut new_elems = vec![];
                    for elem in elements {
                        let typed = self.expr_to_typed_expr((&elem.0, &elem.1));
                        new_elems.push(typed);
                    }
                    let ty = &new_elems[0].ty.clone();
                    (
                        TypedExprKind::Array {
                            elements: new_elems,
                        },
                        ty.clone(),
                    )
                }
            }
            Expr::Index { array, index } => {
                // TODO: add custom indexing through traits
                let (expr, _span) = &**array;
                let (index, span) = &**index;

                let typed_array = self.expr_to_typed_expr((&expr, span));
                let typed_index = self.expr_to_typed_expr((&index, span));

                match &*typed_array.ty.clone() {
                    Type::Constructor {
                        name,
                        generics,
                        traits: _,
                    } if *name == "List".to_string() => (
                        TypedExprKind::Index {
                            array: Box::new(typed_array),
                            index: Box::new(typed_index),
                        },
                        generics[0].clone(),
                    ),
                    _ => panic!("bs. who would even index sumn that's not an array."),
                }
            }
            Expr::Call { function, args } => {
                let (fun, span) = &**function;
                let typed_fun = self.expr_to_typed_expr((&fun, span));

                match &*typed_fun.ty.clone() {
                    Type::Variable(_) => {
                        let new_args = args
                            .iter()
                            .map(|arg| {
                                let (arg, span) = &*arg;
                                self.expr_to_typed_expr((arg, span))
                            })
                            .collect::<Vec<_>>();
                        let ty = typed_fun.ty.clone();
                        (TypedExprKind::Call {function: Box::new(typed_fun), args: new_args}, ty)
                    }
                    Type::Function { params: _, return_type } => {
                        let new_args = args
                            .iter()
                            .map(|arg| {
                                let (arg, span) = &*arg;
                                self.expr_to_typed_expr((arg, span))
                            })
                            .collect::<Vec<_>>();
                        (TypedExprKind::Call {function: Box::new(typed_fun), args: new_args}, *return_type.clone())
                    }
                    _ => todo!(),
                }
            }
            Expr::StructAccess {
                struct_val: _,
                field_name: _,
            } => {
                todo!()
            },
            Expr::MethodCall { .. } => todo!(), // needs structs implemented
            Expr::BinOp { operator, l_value, r_value } => {
                let (l_expr, l_span) = &**l_value;
                let l_value_typed = self.expr_to_typed_expr((l_expr, l_span));

                let (r_expr, r_span) = &**r_value;
                let r_value_typed = self.expr_to_typed_expr((r_expr, r_span));

                let ty = match (operator, &*l_value_typed.ty, &*l_value_typed.ty) {
                    (BinOp::Add, Type::Constructor{name: name_l, ..}, Type::Constructor{name: name_r, ..}) if name_l == "int" && name_r == "int" =>{
                        t_int!()
                    },
                    (BinOp::Add, Type::Constructor{name: name_l, ..}, Type::Constructor{name: name_r, ..}) if name_l == "float" && name_r == "int" => {
                        t_float!()
                    }
                    (BinOp::Add, Type::Constructor{name: name_l, ..}, Type::Constructor{name: name_r, ..}) if name_l == "int" && name_r == "float" => {
                        t_float!()
                    },
                    (BinOp::Add, Type::Constructor{name: name_l, ..}, Type::Constructor{name: name_r, ..}) if name_l == "string" || name_r == "string" => {
                        t_string!()
                    },

                    (BinOp::Sub, Type::Constructor{name: name_l, ..}, Type::Constructor{name: name_r, ..}) if name_l == "int" && name_r == "int" =>{
                        t_int!()
                    },
                    (BinOp::Sub, Type::Constructor{name: name_l, ..}, Type::Constructor{name: name_r, ..}) if name_l == "float" && name_r == "int" => {
                        t_float!()
                    }
                    (BinOp::Sub, Type::Constructor{name: name_l, ..}, Type::Constructor{name: name_r, ..}) if name_l == "int" && name_r == "float" => {
                        t_float!()
                    },

                    (BinOp::Mul, Type::Constructor{name: name_l, ..}, Type::Constructor{name: name_r, ..}) if name_l == "int" && name_r == "int" =>{
                        t_int!()
                    },
                    (BinOp::Mul, Type::Constructor{name: name_l, ..}, Type::Constructor{name: name_r, ..}) if name_l == "float" && name_r == "int" => {
                        t_float!()
                    }
                    (BinOp::Mul, Type::Constructor{name: name_l, ..}, Type::Constructor{name: name_r, ..}) if name_l == "int" && name_r == "float" => {
                        t_float!()
                    },
                    (BinOp::Mul, Type::Constructor{name: name_l, ..}, Type::Constructor{name: name_r, ..}) if (name_l == "string" && name_r == "int") || (name_l == "int" && name_r == "string") => {
                        t_string!()
                    },

                    (BinOp::Div, Type::Constructor{name: name_l, ..}, Type::Constructor{name: name_r, ..}) if name_l == "int" && name_r == "int" =>{
                        t_float!()
                    },
                    (BinOp::Div, Type::Constructor{name: name_l, ..}, Type::Constructor{name: name_r, ..}) if name_l == "float" && name_r == "int" => {
                        t_float!()
                    }
                    (BinOp::Div, Type::Constructor{name: name_l, ..}, Type::Constructor{name: name_r, ..}) if name_l == "int" && name_r == "float" => {
                        t_float!()
                    },

                    _ => todo!()
                };

                (TypedExprKind::BinOp {
                    operator: operator.clone(),
                    l_value: Box::new(l_value_typed),
                    r_value: Box::new(r_value_typed),
                }, ty.clone())
            },
            Expr::UnOp { unop, expression } => {
                let (expr, span) = &**expression;
                let typed_expr = self.expr_to_typed_expr((expr, span));
                let ty = typed_expr.ty.clone();
                (TypedExprKind::UnOp {
                    unop: unop.clone(),
                    expression: Box::new(typed_expr),
                }, ty)
            },
            Expr::Assign { l_value, r_value, assign_op} => {
                let (l_expr, l_span) = &**l_value;
                let l_value_typed = self.expr_to_typed_expr((l_expr, l_span));

                let (r_expr, r_span) = &**r_value;
                let r_value_typed = self.expr_to_typed_expr((r_expr, r_span));

                // let _ty = match (assign_op, &*l_value_typed.ty, &*l_value_typed.ty) {
                //     (AssignOp::Assign, Type::Variable(_), Type::Variable(_)) => {

                //     }
                //     _=>todo!()
                // };

                let ty = r_value_typed.ty.clone();

                (TypedExprKind::Assign {
                    assign_op: assign_op.clone(),
                    l_value: Box::new(l_value_typed),
                    r_value: Box::new(r_value_typed),

                }, ty.clone())
            },
            Expr::Do { expressions } => {
                let mut typed_exprs = vec![];
                for expression in expressions {
                    let (expr, span) = expression;
                    typed_exprs.push(self.expr_to_typed_expr((expr, span)));
                }
                let Some(ex) = typed_exprs.last() else {todo!()};
                let ty = ex.ty.clone();
                (TypedExprKind::Do {expressions: typed_exprs}, ty)
            },
            Expr::Let { var, type_annot, value } => {
                let (val_expr, val_span) = &**value;
                let typed_val = self.expr_to_typed_expr((val_expr, val_span));
                
                let var_ty = match type_annot {
                    Some((annot, _)) => self.type_annot_to_type(annot),
                    None => typed_val.ty.clone(),
                };

                let ty = typed_val.ty.clone();
                
                self.insert_var(var.clone(), var_ty.clone());
                
                (TypedExprKind::Let { 
                    var: var.clone(), 
                    value: Box::new(typed_val) 
                }, ty)
            }
            Expr::IfElse { condition, if_branch, else_branch } => {
                let (condition, condition_span) = &**condition;
                let (if_branch, if_branch_span) = &**if_branch;

                let typed_condition = self.expr_to_typed_expr((condition, condition_span));
                let typed_if_branch = self.expr_to_typed_expr((if_branch, if_branch_span));

                match else_branch {
                    Some(else_branch) => {
                        let ty = typed_if_branch.ty.clone();
                        let (else_branch, else_branch_span) = &**else_branch;
                        let typed_else_branch = self.expr_to_typed_expr((else_branch, else_branch_span));
                        (TypedExprKind::IfElse { 
                            condition: Box::new(typed_condition),
                            if_branch: Box::new(typed_if_branch),
                            else_branch: Some(Box::new(typed_else_branch)),
                        }, ty)
                    }
                    None => {
                        (TypedExprKind::IfElse { 
                            condition: Box::new(typed_condition),
                            if_branch: Box::new(typed_if_branch),
                            else_branch: None,
                        }, t_unit!())
                    }
                }
            },
            Expr::Tuple(expressions) => {
                let mut typed_exprs = vec![];
                let mut types = vec![];

                for expression in expressions {
                    let (expr, span) = expression;
                    let typed = self.expr_to_typed_expr((expr, span));
                    types.push(typed.ty.clone());
                    typed_exprs.push(typed);
                }
                (TypedExprKind::Tuple(typed_exprs), Arc::new(Type::Tuple(types)))
            },
            Expr::Error => unreachable!(),
        };
        TypedExpr {
            kind: exprkind,
            ty,
            range: span.clone(),
        }
    }

    pub fn type_annot_to_type(&mut self, type_annot: &TypeAnnot) -> Arc<Type> {
        match type_annot {
            TypeAnnot::Bool => t_bool!(),
            TypeAnnot::Int => t_int!(),
            TypeAnnot::Float => t_float!(),
            TypeAnnot::String => t_string!(),
            TypeAnnot::Boring(name) => Arc::new(Type::Constructor { name: name.to_string(), generics: vec![], traits: vec![]}),
            TypeAnnot::Generic(name, generics) => {
                let generics = generics
                    .iter()
                    .map(|generic| self.type_annot_to_type(generic))
                    .collect::<Vec<_>>();
                Arc::new(Type::Constructor {
                    name: name.clone(),
                    generics: generics.clone(),
                    traits: vec![],
                })
            }
            TypeAnnot::Union(unions) => {
                let unions = unions
                    .iter()
                    .map(|union_| self.type_annot_to_type(union_))
                    .collect::<Vec<_>>();
                Arc::new(Type::Union(unions))
            }
            TypeAnnot::Function { params, return_type } => {
                let params = params
                    .iter()
                    .map(|param| self.type_annot_to_type(param))
                    .collect::<Vec<_>>();
                let return_type = self.type_annot_to_type(return_type);
                Arc::new(Type::Function {
                    params: params,
                    return_type: Box::new(return_type),
                })
            }
            TypeAnnot::Tuple(tuple) => {
                let tuple = tuple
                    .iter()
                    .map(|tuple| self.type_annot_to_type(tuple))
                    .collect::<Vec<_>>();
                Arc::new(Type::Tuple(tuple))
            }
            TypeAnnot::Trait(name) => Arc::new(Type::Trait(name.clone())),
        }
    }
}
