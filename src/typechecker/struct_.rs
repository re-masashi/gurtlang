#![allow(unused_imports)]

use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    ASTNode, AssignOp, BinOp, Expr, Function, Struct, Type, TypeAnnot, TypedASTNode, TypedExpr,
    TypedExprKind, TypedFunction, TypedStruct,
};
use crate::typechecker::{StructTy, TypeEnv, type_annot_to_type};
use crate::{t_bool, t_float, t_int, t_list, t_string, t_unit, tvar};

impl TypeEnv<'_> {
    pub fn struct_to_typed_struct(
        &mut self,
        struct_: (&Struct, &Range<usize>),
    ) -> (TypedStruct, Range<usize>) {
        let (struct_, _span) = struct_;
        let Struct {
            name,
            generics,
            fields,
        } = struct_;

        let last_field_span = match fields.last() {
            Some(f) => &f.2,
            None => &name.1,
        };

        let typed_fields = fields
            .iter()
            .map(|(field_name, type_annot, range)| {
                let ty = type_annot_to_type(type_annot);
                (field_name.to_string(), ty, range.clone())
            })
            .collect::<Vec<_>>();

        let struct_ty = Arc::new(StructTy {
            name: name.0.clone(),
            generics: generics.iter().map(|(g, _)| g.clone()).collect(),
            fields: typed_fields
                .iter()
                .map(|(name, ty, _)| (name.clone(), ty.clone()))
                .collect(),
        });

        self.insert_struct(name.0.clone(), struct_ty);

        let function_type = Arc::new(Type::Function {
            params: typed_fields.iter().map(|(_, ty, _)| ty.clone()).collect(),
            return_type: Arc::new(Type::Constructor {
                name: name.0.to_string(),
                generics: vec![],
                traits: vec![],
            }),
        });
        self.insert_var(name.0.clone(), function_type.clone()); // constructor

        (
            TypedStruct {
                name: name.0.clone(),
                generics: generics.clone(),
                fields: typed_fields,
            },
            name.1.start..last_field_span.end,
        )
    }
}
