#![allow(unused_imports)]

use std::collections::HashMap;
use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    ASTNode, AssignOp, BinOp, Expr, Function, Struct, Type, TypeAnnot, TypedASTNode, TypedExpr,
    TypedExprKind, TypedFunction, TypedStruct,
};
use crate::typechecker::{StructTy, TypeEnv, type_annot_to_type};
use crate::{t_bool, t_float, t_int, t_list, t_string, t_unit, tvar};

// typechecker/struct_.rs
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

        // Create mapping from generic names to type variables
        let mut generic_map: HashMap<String, Arc<Type>> = HashMap::new();
        for (generic_name, _) in generics {
            let tv = self.new_typevar();
            generic_map.insert(generic_name.clone(), tv);
        }

        let typed_fields = fields
            .iter()
            .map(|(field_name, type_annot, range)| {
                // Replace generics with type variables
                let ty = self.convert_type_annot(type_annot, &generic_map);
                (field_name.to_string(), ty, range.clone())
            })
            .collect::<Vec<_>>();

        let struct_ty = Arc::new(StructTy {
            name: name.0.clone(),
            generics: generics.iter().map(|(name, _)| name.clone()).collect(),
            fields: typed_fields
                .iter()
                .map(|(name, ty, _)| (name.clone(), ty.clone()))
                .collect(),
        });

        self.insert_struct(name.0.clone(), struct_ty);

        // Create constructor function type
        let return_type = Arc::new(Type::Constructor {
            name: name.0.clone(),
            generics: generic_map.values().cloned().collect(),
            traits: vec![],
        });

        let function_type = Arc::new(Type::Function {
            params: typed_fields.iter().map(|(_, ty, _)| ty.clone()).collect(),
            return_type,
        });

        self.insert_var(name.0.clone(), function_type.clone());

        (
            TypedStruct {
                name: name.0.clone(),
                generics: generics.clone(),
                fields: typed_fields,
            },
            name.1.start..last_field_span.end,
        )
    }

    fn convert_type_annot(
        &self,
        type_annot: &TypeAnnot,
        generic_map: &HashMap<String, Arc<Type>>,
    ) -> Arc<Type> {
        match type_annot {
            // Handle bare generic parameters
            TypeAnnot::Boring(name) => generic_map.get(name).cloned().unwrap_or_else(|| {
                Arc::new(Type::Constructor {
                    name: name.clone(),
                    generics: vec![],
                    traits: vec![],
                })
            }),

            // Handle generic types with parameters
            TypeAnnot::Generic(name, generics) => {
                // First check if it's a generic parameter
                if generic_map.contains_key(name) {
                    generic_map[name].clone()
                } else {
                    let generics = generics
                        .iter()
                        .map(|t| self.convert_type_annot(t, generic_map))
                        .collect();
                    Arc::new(Type::Constructor {
                        name: name.clone(),
                        generics,
                        traits: vec![],
                    })
                }
            }

            // Handle other type annotations
            TypeAnnot::Union(unions) => {
                let unions = unions
                    .iter()
                    .map(|t| self.convert_type_annot(t, generic_map))
                    .collect();
                Arc::new(Type::Union(unions))
            }
            TypeAnnot::Function {
                params,
                return_type,
            } => {
                let params = params
                    .iter()
                    .map(|t| self.convert_type_annot(t, generic_map))
                    .collect();
                let return_type = self.convert_type_annot(return_type, generic_map);
                Arc::new(Type::Function {
                    params,
                    return_type,
                })
            }
            TypeAnnot::Tuple(tuple) => {
                let tuple = tuple
                    .iter()
                    .map(|t| self.convert_type_annot(t, generic_map))
                    .collect();
                Arc::new(Type::Tuple(tuple))
            }
            _ => type_annot_to_type(type_annot),
        }
    }
}
