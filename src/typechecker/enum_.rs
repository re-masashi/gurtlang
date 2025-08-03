// typechecker/enum_.rs
use super::*;
use crate::ast::TypedEnum;
use crate::ast::TypedEnumVariant;
use crate::ast::TypedEnumVariantKind;
use crate::ast::{Enum, EnumVariant};
use crate::typechecker::struct_::convert_type_annot;

impl TypeEnv<'_> {
    pub fn enum_to_typed_enum(
        &mut self,
        enum_: (&Enum, &Range<usize>),
    ) -> (TypedEnum, Range<usize>) {
        let (enum_, span) = enum_;
        let Enum {
            name,
            generics,
            variants,
        } = enum_;

        // Create mapping for generic parameters
        let mut generic_map = HashMap::new();
        let mut generic_tys = Vec::new(); // Maintain order of generic parameters

        for (generic_name, _) in generics {
            let tv = self.new_typevar();
            generic_map.insert(generic_name.clone(), tv.clone());
            generic_tys.push(tv);
        }

        // Convert each variant
        let mut typed_variants = Vec::new();
        let mut enum_ty_variants = HashMap::new();

        for variant in variants {
            let EnumVariant {
                name: variant_name,
                kind: variant_kind,
                range: variant_span,
            } = variant;

            let (typed_kind, variant_ty_kind) = match variant_kind {
                EnumVariantKind::Unit => (TypedEnumVariantKind::Unit, EnumVariantKindTy::Unit),
                EnumVariantKind::Tuple(fields) => {
                    let field_types: Vec<_> = fields
                        .iter()
                        .map(|(ty, _)| convert_type_annot(ty, &generic_map))
                        .collect();
                    (
                        TypedEnumVariantKind::Tuple(field_types.clone()),
                        EnumVariantKindTy::Tuple(field_types),
                    )
                }
                EnumVariantKind::Struct(fields) => {
                    let field_types: Vec<_> = fields
                        .iter()
                        .map(|(field_name, ty, _)| {
                            (field_name.clone(), convert_type_annot(ty, &generic_map))
                        })
                        .collect();
                    (
                        TypedEnumVariantKind::Struct(field_types.clone()),
                        EnumVariantKindTy::Struct(field_types),
                    )
                }
            };

            let enum_type = Arc::new(Type::Constructor {
                name: name.0.clone(),
                generics: generic_tys.clone(), // Use ordered list
                traits: vec![],
            });

            enum_ty_variants.insert(
                variant_name.0.clone(),
                EnumVariantTy {
                    kind: variant_ty_kind,
                    ty: enum_type.clone(),
                },
            );
            typed_variants.push(TypedEnumVariant {
                name: variant_name.0.clone(),
                kind: typed_kind,
                range: variant_span.clone(),
            });
        }

        let enum_ty = Arc::new(EnumTy {
            name: name.0.clone(),
            generics: generics.iter().map(|(name, _)| name.clone()).collect(),
            variants: enum_ty_variants,
        });

        // println!("insertng enum {:?}", name);

        self.enums.insert(name.0.clone(), enum_ty);

        let typed_enum = TypedEnum {
            name: name.0.clone(),
            generics: generics.clone(),
            variants: typed_variants,
        };

        (typed_enum, span.clone())
    }
}
