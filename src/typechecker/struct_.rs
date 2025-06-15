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

        // Create a mapping of generic parameter names
        let generic_names: Vec<_> = generics.iter().map(|(name, _)| name.clone()).collect();

        let typed_fields = fields
            .iter()
            .map(|(field_name, type_annot, range)| {
                // Use special conversion that handles generic params
                let ty = self.convert_type_annot(type_annot, &generic_names);
                (field_name.to_string(), ty, range.clone())
            })
            .collect::<Vec<_>>();

        let struct_ty = Arc::new(StructTy {
            name: name.0.clone(),
            generics: generic_names.clone(),
            fields: typed_fields
                .iter()
                .map(|(name, ty, _)| (name.clone(), ty.clone()))
                .collect(),
        });

        self.insert_struct(name.0.clone(), struct_ty);

        // Create constructor function type
        let return_type = Arc::new(Type::Constructor {
            name: name.0.clone(),
            generics: generics.iter().map(|_| self.new_typevar()).collect(),
            traits: vec![],
        });

        let function_type = Arc::new(Type::Function {
            params: typed_fields.iter().map(|(_, ty, _)| ty.clone()).collect(),
            return_type: (return_type),
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

    fn convert_type_annot(&self, type_annot: &TypeAnnot, _generic_names: &[String]) -> Arc<Type> {
        match type_annot {
            _ => type_annot_to_type(type_annot),
        }
    }
}
