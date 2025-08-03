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
        self.return_depth = 0;

        let (function, fun_span) = function;
        let Function {
            name,
            args,
            body,
            return_type,
        } = function;

        // Step 1: Process arguments and create provisional function type
        let typed_args = args
            .iter()
            .map(|(argname, typeannot, range)| match typeannot {
                Some(ty) => (argname.to_string(), type_annot_to_type(ty), range.clone()),
                None => {
                    let new_typevar = self.new_typevar();
                    (argname.to_string(), new_typevar, range.clone())
                }
            })
            .collect::<Vec<_>>();

        let arg_types: Vec<_> = typed_args.iter().map(|(_, ty, _)| ty.clone()).collect();

        // Step 2: Create provisional return type (critical for recursion)
        let provisional_return_type = match return_type {
            Some((ty, _)) => type_annot_to_type(ty),
            None => {
                // For recursive functions without explicit return type,
                // we need to create a type variable that can be unified later
                self.new_typevar()
            }
        };

        // Step 3: Create and register function type BEFORE analyzing body
        let function_type = Arc::new(Type::Function {
            params: arg_types.clone(),
            return_type: provisional_return_type.clone(),
        });

        // CRITICAL: Insert function into environment before analyzing body
        // This allows recursive calls to find the function
        self.insert_var(name.clone(), function_type.clone());

        // Step 4: Create new scope for function parameters
        let old_vars = self.variables.clone();
        for (arg_name, arg_type, _) in &typed_args {
            self.insert_var(arg_name.clone(), arg_type.clone());
        }

        // Step 5: Analyze function body
        let (body_expr, body_span) = &**body;
        let typed_body = self.expr_to_typed_expr((body_expr, body_span));

        // Step 6: Unify body type with return type
        if !self.unify(
            typed_body.ty.clone(),
            provisional_return_type.clone(),
            &typed_body.range,
            body_span,
        ) {
            // If unification fails and we don't have an explicit return type,
            // try to resolve the type variable to something concrete
            if return_type.is_none() {
                let resolved_return = self.resolve_deep(provisional_return_type.clone());
                if let Type::Variable(_) = &*resolved_return {
                    // Still unresolved - default to the body type
                    self.unify(
                        provisional_return_type.clone(),
                        typed_body.ty.clone(),
                        body_span,
                        &typed_body.range,
                    );
                }
            }
        }

        // Step 7: Resolve final types
        let final_return_type = self.resolve_deep(provisional_return_type.clone());
        let final_arg_types: Vec<_> = typed_args
            .iter()
            .map(|(name, ty, span)| (name.clone(), self.resolve_deep(ty.clone()), span.clone()))
            .collect();

        // Step 8: Update function type in environment with resolved types
        let final_function_type = Arc::new(Type::Function {
            params: final_arg_types
                .iter()
                .map(|(_, ty, _)| ty.clone())
                .collect(),
            return_type: final_return_type.clone(),
        });
        self.insert_var(name.clone(), final_function_type.clone());

        // Step 9: Restore previous variable scope
        self.variables = old_vars;
        // Re-insert the final function type
        self.insert_var(name.clone(), final_function_type);

        self.in_function = was_in_function;

        let typed_return_type = match return_type {
            Some((_, span)) => (final_return_type, span.clone()),
            None => (final_return_type, typed_body.range.clone()),
        };

        (
            TypedFunction {
                args: final_arg_types,
                body: Box::new((typed_body, body_span.clone())),
                name: name.to_string(),
                return_type: typed_return_type,
                is_constructor: false,
            },
            fun_span.clone(),
        )
    }
}
