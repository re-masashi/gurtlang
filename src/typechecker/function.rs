#![allow(unused_imports)]

use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    ASTNode, AssignOp, BinOp, Expr, Function, Struct, Type, TypeAnnot, TypedASTNode, TypedExpr,
    TypedExprKind, TypedFunction, TypedStruct,
};
use crate::typechecker::{TypeEnv, type_annot_to_type};
use crate::{t_bool, t_float, t_int, t_list, t_string, t_unit, tvar};

impl TypeEnv<'_> {
    pub fn function_to_typed_function(
        &mut self,
        function: (&Function, &Range<usize>),
    ) -> (TypedFunction, Range<usize>) {
        let (function, fun_span) = function;
        let Function {
            name,
            args,
            body,
            return_type,
        } = function;
        let typed_args = args
            .iter()
            .map(|(argname, typeannot, range)| match typeannot {
                Some(ty) => (argname.to_string(), type_annot_to_type(ty), range.clone()),
                None => {
                    self.insert_var(argname.clone(), tvar!(self.variables.len() + 1));
                    (
                        argname.to_string(),
                        tvar!(self.variables.len()),
                        range.clone(),
                    )
                }
            })
            .collect::<Vec<_>>();
        let (body, body_span) = &**body;
        let typed_body = (
            self.expr_to_typed_expr((body, body_span)),
            body_span.clone(),
        );

        let typed_return_type = match return_type {
            Some((return_type, span)) => (type_annot_to_type(return_type), span.clone()),
            None => (typed_body.0.ty.clone(), typed_body.0.range.clone()),
        };
        let function_type = Arc::new(Type::Function {
            params: typed_args.iter().map(|(_, ty, _)| ty.clone()).collect(),
            return_type: typed_return_type.0.clone(),
        });
        self.insert_var(name.clone(), function_type.clone());
        (
            TypedFunction {
                args: typed_args,
                body: Box::new(typed_body),
                name: name.to_string(),
                return_type: typed_return_type,
            },
            fun_span.clone(),
        )
    }
}
