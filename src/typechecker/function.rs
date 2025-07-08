#![allow(unused_imports)]

use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    ASTNode, AssignOp, BinOp, Expr, Extern, Function, Struct, Type, TypeAnnot, TypedASTNode,
    TypedExpr, TypedExprKind, TypedExtern, TypedFunction, TypedStruct,
};
use crate::typechecker::{TypeEnv, type_annot_to_type};
use crate::{t_bool, t_float, t_int, t_list, t_string, t_unit, tvar};

impl TypeEnv<'_> {
    pub fn function_to_typed_function(
        &mut self,
        function: (&Function, &Range<usize>),
    ) -> (TypedFunction, Range<usize>) {
        let was_in_function = self.in_function;
        self.in_function = true;
        self.return_depth = 0; // Reset depth for new function

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
                    let new_typevar = self.new_typevar();
                    self.insert_var(argname.clone(), new_typevar.clone());
                    (argname.to_string(), new_typevar, range.clone())
                }
            })
            .collect::<Vec<_>>();
        let arg_types: Vec<_> = typed_args.iter().map(|(_, ty, _)| ty.clone()).collect();
        let provisional_return_type = match return_type {
            Some((ty, _)) => type_annot_to_type(ty),
            None => self.new_typevar(),
        };

        let function_type = Arc::new(Type::Function {
            params: arg_types,
            return_type: provisional_return_type.clone(),
        });

        self.insert_var(name.clone(), function_type.clone());

        let (body, body_span) = &**body;
        let typed_body = (
            self.expr_to_typed_expr((body, body_span)),
            body_span.clone(),
        );
        self.unify(
            typed_body.0.ty.clone(),
            provisional_return_type.clone(),
            body_span,
            body_span,
        );

        self.in_function = was_in_function;

        let final_return_type = self.resolve_deep(provisional_return_type);

        let typed_return_type = match return_type {
            Some((return_type, span)) => (type_annot_to_type(return_type), span.clone()),
            None => (final_return_type, typed_body.0.range.clone()),
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
                is_constructor: false,
            },
            fun_span.clone(),
        )
    }

    pub fn extern_to_typed_extern(
        &mut self,
        function: (&Extern, &Range<usize>),
    ) -> (TypedExtern, Range<usize>) {
        let (function, fun_span) = function;
        let Extern {
            name,
            args,
            return_type,
        } = function;
        let typed_args = args
            .iter()
            .map(|(typeannot, range)| (type_annot_to_type(typeannot), range.clone()))
            .collect::<Vec<_>>();
        let arg_types = typed_args
            .iter()
            .map(|(t, _)| t.clone())
            .collect::<Vec<_>>();
        let typed_return_type = type_annot_to_type(&return_type.0);

        let function_type = Arc::new(Type::Function {
            params: arg_types,
            return_type: typed_return_type.clone(),
        });

        self.insert_var(name.clone(), function_type.clone());

        (
            TypedExtern {
                args: typed_args,
                name: name.to_string(),
                return_type: (typed_return_type, return_type.1.clone()),
            },
            fun_span.clone(),
        )
    }
}
