#![allow(unused_imports)]

use ariadne::ColorGenerator;
use ariadne::Fmt;
use ariadne::Label;
use ariadne::Report;
use ariadne::ReportKind;
use yansi::Color;
use yansi::Paint;

use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    ASTNode, AssignOp, BinOp, Expr, Function, Struct, Type, TypeAnnot, TypedASTNode, TypedExpr,
    TypedExprKind, TypedFunction, TypedStruct,
};
use crate::typechecker::{TypeEnv, type_annot_to_type, type_string};
use crate::{t_bool, t_float, t_int, t_list, t_string, t_unit, tvar};

impl TypeEnv<'_> {
    pub fn unify(
        &mut self,
        t1: Arc<Type>,
        t2: Arc<Type>,
        span1: &Range<usize>,
        span2: &Range<usize>,
    ) -> bool {
        let t1 = self.resolve(t1);
        let t2 = self.resolve(t2);

        match (&*t1, &*t2) {
            // Same types
            _ if t1 == t2 => true,

            // Variable unification
            (Type::Variable(i), _) => self.bind(*i, t2, span1),
            (_, Type::Variable(i)) => self.bind(*i, t1, span2),

            // Function types
            (
                Type::Function {
                    params: p1,
                    return_type: r1,
                },
                Type::Function {
                    params: p2,
                    return_type: r2,
                },
            ) if p1.len() == p2.len() => {
                let mut f = true;
                for (a, b) in p1.iter().zip(p2.iter()) {
                    if !self.unify(a.clone(), b.clone(), span1, span2) {
                        f = false;
                    }
                }
                if f {
                    self.unify(*r1.clone(), *r2.clone(), span1, span2)
                } else {
                    f
                }
            }

            // Tuple types
            (Type::Tuple(t1), Type::Tuple(t2)) if t1.len() == t2.len() => {
                let mut f = true;
                for (a, b) in t1.iter().zip(t2.iter()) {
                    if !self.unify(a.clone(), b.clone(), span1, span2) {
                        f = false;
                    }
                }
                f
            }

            // Constructor types
            (
                Type::Constructor {
                    name: n1,
                    generics: g1,
                    ..
                },
                Type::Constructor {
                    name: n2,
                    generics: g2,
                    ..
                },
            ) if n1 == n2 && g1.len() == g2.len() => {
                let mut f = true;
                for (a, b) in g1.iter().zip(g2.iter()) {
                    if !self.unify(a.clone(), b.clone(), span1, span2) {
                        f = false;
                    }
                }
                f
            }

            (Type::Union(u1), Type::Union(u2)) => {
                let mut success = false;
                for t1 in u1 {
                    for t2 in u2 {
                        if self.unify(t1.clone(), t2.clone(), span1, span2) {
                            success = true;
                            break;
                        }
                    }
                    if success {
                        break;
                    }
                }
                success
            }

            // Union and other types
            (Type::Union(u), _other) => {
                let mut success = false;
                for t in u {
                    if self.unify(t.clone(), t2.clone(), span1, span2) {
                        success = true;
                        break;
                    }
                }
                success
            }

            (_other, Type::Union(u)) => {
                let mut success = false;
                for t in u {
                    if self.unify(t1.clone(), t.clone(), span1, span2) {
                        success = true;
                        break;
                    }
                }
                success
            }

            // (
            //     Type::Trait(n1) ,
            //     Type::Trait(n2),
            // ) if n1 == n2 {
            //    // handled in the first case
            // }

            // Mismatched types
            _ => false,
        }
    }

    fn bind(&mut self, var: usize, ty: Arc<Type>, span: &Range<usize>) -> bool {
        if let Type::Variable(i) = *ty {
            if i == var {
                return true; // Same variable
            }
        }

        if self.occurs(var, &ty) {
            panic!(
                "Recursive type involving {} at {:?}",
                type_string(&ty),
                span
            );
        }

        self.substitutions.insert(var, ty);
        true
    }

    fn occurs(&self, var: usize, ty: &Type) -> bool {
        match ty {
            Type::Variable(i) => {
                *i == var
                    || self
                        .substitutions
                        .get(i)
                        .is_some_and(|t| self.occurs(var, t))
            }
            Type::Constructor { generics, .. } => generics.iter().any(|t| self.occurs(var, t)),
            Type::Function {
                params,
                return_type,
            } => params.iter().any(|t| self.occurs(var, t)) || self.occurs(var, return_type),
            Type::Tuple(types) => types.iter().any(|t| self.occurs(var, t)),
            Type::Union(types) => types.iter().any(|t| self.occurs(var, t)),
            _ => false,
        }
    }

    pub fn resolve(&self, ty: Arc<Type>) -> Arc<Type> {
        match &*ty {
            Type::Variable(i) => self
                .substitutions
                .get(i)
                .map(|t| self.resolve(t.clone()))
                .unwrap_or(ty),
            _ => ty,
        }
    }
}
