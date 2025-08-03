#![allow(unused_imports)]

use ariadne::ColorGenerator;
use ariadne::Fmt;
use ariadne::Label;
use ariadne::Report;
use ariadne::ReportKind;
use yansi::Color;
use yansi::Paint;

use std::collections::HashMap;
use std::collections::HashSet;
use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    ASTNode, AssignOp, BinOp, Expr, Function, Struct, Type, TypeAnnot, TypedASTNode, TypedExpr,
    TypedExprKind, TypedFunction, TypedStruct,
};
use crate::typechecker::{StructTy, TypeEnv, type_annot_to_type, type_string};
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

        let fl = match (&*t1, &*t2) {
            // Same types
            _ if t1 == t2 => true,

            // Variable unification
            (Type::Variable(i), _) => self.bind(*i, t2.clone(), span1),
            (_, Type::Variable(i)) => self.bind(*i, t1.clone(), span2),

            // Union type unification
            (Type::Union(u1), Type::Union(u2)) => self.unify_unions(u1, u2, span1, span2),
            (Type::Union(u), _) => self.unify_with_union(u, &t2, span1, span2),
            (_, Type::Union(u)) => self.unify_with_union(u, &t1, span2, span1),

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
                let mut success = true;
                for (a, b) in p1.iter().zip(p2.iter()) {
                    if !self.unify(a.clone(), b.clone(), span1, span2) {
                        success = false;
                    }
                }
                if success {
                    self.unify(r1.clone(), r2.clone(), span1, span2)
                } else {
                    false
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

            // (Type::Union(u1), Type::Union(u2)) => {
            //     let mut success = false;
            //     for t1 in u1 {
            //         for t2 in u2 {
            //             if self.unify(t1.clone(), t2.clone(), span1, span2) {
            //                 success = true;
            //                 break;
            //             }
            //         }
            //         if success {
            //             break;
            //         }
            //     }
            //     success
            // }

            // Union and other types
            // (Type::Union(u), _other) => {
            //     let mut success = false;
            //     for t in u {
            //         if self.unify(t.clone(), t2.clone(), span1, span2) {
            //             success = true;
            //             break;
            //         }
            //     }
            //     success
            // }

            // (_other, Type::Union(u)) => {
            //     let mut success = false;
            //     for t in u {
            //         if self.unify(t1.clone(), t.clone(), span1, span2) {
            //             success = true;
            //             break;
            //         }
            //     }
            //     success
            // }

            // (
            //     Type::Trait(n1) ,
            //     Type::Trait(n2),
            // ) if n1 == n2 {
            //    // handled in the first case
            // }

            // Mismatched types
            _ => false,
        };

        if !fl {
            panic!(
                "unification error {}, {}",
                type_string(&t1),
                type_string(&t2)
            )
        }
        fl
    }

    pub fn bind(&mut self, var: usize, ty: Arc<Type>, span: &Range<usize>) -> bool {
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

    fn unify_with_union(
        &mut self,
        u: &[Arc<Type>],
        other: &Arc<Type>,
        span_union: &Range<usize>,
        span_other: &Range<usize>,
    ) -> bool {
        // If the other type is already in the union, succeed
        if u.iter()
            .any(|t| self.resolve(t.clone()) == self.resolve(other.clone()))
        {
            return true;
        }

        // Try to unify with each variant in the union
        for variant in u {
            if self.unify(variant.clone(), other.clone(), span_union, span_other) {
                return true;
            }
        }

        false
    }

    fn unify_unions(
        &mut self,
        u1: &[Arc<Type>],
        u2: &[Arc<Type>],
        span1: &Range<usize>,
        span2: &Range<usize>,
    ) -> bool {
        // Create sets of resolved types
        let set1: HashSet<_> = u1.iter().map(|t| self.resolve(t.clone())).collect();
        let set2: HashSet<_> = u2.iter().map(|t| self.resolve(t.clone())).collect();

        // If both sets are equal, unification succeeds
        if set1 == set2 {
            return true;
        }

        // If either set is a subset of the other, we can unify to the superset
        if set1.is_subset(&set2) {
            return self.bind_union_to_superset(u1, u2, span1, span2);
        } else if set2.is_subset(&set1) {
            return self.bind_union_to_superset(u2, u1, span2, span1);
        }

        // Try to find a common type
        for t1 in u1 {
            for t2 in u2 {
                if self.unify(t1.clone(), t2.clone(), span1, span2) {
                    return true;
                }
            }
        }

        false
    }

    fn bind_union_to_superset(
        &mut self,
        subset: &[Arc<Type>],
        superset: &[Arc<Type>],
        span_subset: &Range<usize>,
        _span_superset: &Range<usize>,
    ) -> bool {
        // Create type variables for each element in the subset
        for t in subset {
            if let Type::Variable(i) = &**t {
                self.bind(*i, Arc::new(Type::Union(superset.to_vec())), span_subset);
            }
        }
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
}
