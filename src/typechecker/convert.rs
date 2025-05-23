#![allow(unused_imports)]

use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    ASTNode, Expr, Function, Struct, Type, TypeAnnot, TypedASTNode, TypedExpr, TypedExprKind,
    TypedFunction, TypedStruct,
};
use crate::typechecker::TypeEnv;
use crate::{t_bool, t_float, t_int, t_list, t_string, tvar};

impl TypeEnv<'_> {
    pub fn ast_to_typed_ast(&mut self, _ast: Vec<ASTNode>) -> Vec<TypedASTNode> {
        todo!()
    }

    pub fn node_to_typed_node(&mut self, node: ASTNode) -> TypedASTNode {
        match node {
            ASTNode::Error => unreachable!(),
            ASTNode::Expr(_) => todo!(),
            ASTNode::Function(_) => todo!(),
            ASTNode::Struct(_) => todo!(),
        }
    }

    pub fn expr_to_typed_expr(&mut self, expr: (&Expr, Range<usize>)) -> TypedExpr {
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
                        let typed = self.expr_to_typed_expr((&elem.0, elem.1.clone()));
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

                let typed_array = self.expr_to_typed_expr((&expr, span.clone()));
                let typed_index = self.expr_to_typed_expr((&index, span.clone()));

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
                let typed_fun = self.expr_to_typed_expr((&fun, span.clone()));

                match &*typed_fun.ty.clone() {
                    Type::Variable(_) => {
                        let _new_args = args
                            .iter()
                            .map(|arg| {
                                let (arg, span) = &*arg;
                                self.expr_to_typed_expr((arg, span.clone()))
                            })
                            .collect::<Vec<_>>();
                        todo!()
                        // (TypedExprKind::Call {function: Box::new(typed_fun), args: new_args},)
                    }
                    Type::Function { params: _, return_type } => {
                        let new_args = args
                            .iter()
                            .map(|arg| {
                                let (arg, span) = &*arg;
                                self.expr_to_typed_expr((arg, span.clone()))
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
            } => todo!(),
            Expr::MethodCall { .. } => todo!(),
            Expr::BinOp { .. } => todo!(),
            Expr::UnOp { .. } => todo!(),
            Expr::Assign { .. } => todo!(),
            Expr::Do { .. } => todo!(),
            Expr::Let { .. } => todo!(),
            Expr::IfElse { .. } => todo!(),
            Expr::Tuple(_) => todo!(),

            _ => todo!(),
        };
        TypedExpr {
            kind: exprkind,
            ty,
            range: span,
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
