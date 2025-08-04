use super::*;

use crate::ast::ASTNode;
use crate::ast::Enum;
use crate::ast::EnumVariant;
use crate::ast::Function;
use crate::ast::MatchArm;
use crate::ast::Pattern;
use crate::ast::Struct;
use crate::ast::TypedASTNode;
use crate::ast::TypedFunction;
use crate::ast::TypedPattern;
use crate::ast::UnOp;
use crate::ast::{BinOp, Expr, Type, TypedExpr, TypedExprKind};
use crate::t_float;
use crate::t_int;
use crate::t_string;

#[test]
fn test_simple_addition() {
    let program = vec![(
        ASTNode::Expr((
            Expr::BinOp {
                operator: BinOp::Add,
                l_value: Box::new((Expr::Int(5), 0..1)),
                r_value: Box::new((Expr::Int(3), 2..3)),
            },
            0..3,
        )),
        0..3,
    )];

    let mut type_env = TypeEnv::new("test".to_string());
    let typed_ast = type_env.ast_to_typed_ast(program);
    assert!(type_env.errors.is_empty());

    if let TypedASTNode::Expr((TypedExpr { kind, ty, .. }, _)) = &typed_ast[0] {
        assert!(matches!(kind, TypedExprKind::BinOp { .. }));
        assert_eq!(*ty, (t_int!()).into());
    } else {
        panic!("Expected expression");
    }
}

#[test]
fn test_variable_declaration_and_usage() {
    let program = vec![
        (
            ASTNode::Expr((
                Expr::Let {
                    var: "x".to_string(),
                    type_annot: Some((TypeAnnot::Int, 0..3)),
                    value: Box::new((Expr::Int(42), 6..8)),
                },
                0..9,
            )),
            0..9,
        ),
        (
            ASTNode::Expr((
                Expr::BinOp {
                    operator: BinOp::Add,
                    l_value: Box::new((Expr::Variable("x".to_string()), 12..13)),
                    r_value: Box::new((Expr::Int(10), 14..16)),
                },
                12..17,
            )),
            12..17,
        ),
    ];

    let mut type_env = TypeEnv::new("test".to_string());
    let typed_ast = type_env.ast_to_typed_ast(program);
    assert!(type_env.errors.is_empty());

    // Check variable declaration
    if let TypedASTNode::Expr((TypedExpr { kind, ty, .. }, _)) = &typed_ast[0] {
        assert!(matches!(kind, TypedExprKind::Let { .. }));
        assert_eq!(*ty, t_int!());
    }

    // Check variable usage
    if let TypedASTNode::Expr((TypedExpr { kind, ty, .. }, _)) = &typed_ast[1] {
        assert!(matches!(kind, TypedExprKind::BinOp { .. }));
        assert_eq!(*ty, t_int!());
    }
}

#[test]
fn test_function_definition_and_call() {
    let program = vec![
        (
            ASTNode::Function(Function {
                name: "add".to_string(),
                args: vec![
                    ("a".to_string(), Some(TypeAnnot::Int), 10..16),
                    ("b".to_string(), Some(TypeAnnot::Int), 17..21),
                ],
                body: Box::new((
                    Expr::BinOp {
                        operator: BinOp::Add,
                        l_value: Box::new((Expr::Variable("a".to_string()), 30..31)),
                        r_value: Box::new((Expr::Variable("b".to_string()), 32..33)),
                    },
                    30..34,
                )),
                return_type: Some((TypeAnnot::Int, 25..26)),
            }),
            0..35,
        ),
        (
            ASTNode::Expr((
                Expr::Call {
                    function: Box::new((Expr::Variable("add".to_string()), 40..43)),
                    args: vec![(Expr::Int(5), 44..45), (Expr::Int(7), 46..47)],
                },
                40..48,
            )),
            40..48,
        ),
    ];

    let mut type_env = TypeEnv::new("test".to_string());
    let typed_ast = type_env.ast_to_typed_ast(program);
    assert!(type_env.errors.is_empty());

    // Check function call
    if let TypedASTNode::Expr((TypedExpr { kind, ty, .. }, _)) = &typed_ast[1] {
        assert!(matches!(kind, TypedExprKind::Call { .. }));
        assert_eq!(*ty, t_int!());
    }
}

#[test]
fn test_struct_definition_and_usage() {
    let program = vec![
        (
            ASTNode::Struct(Struct {
                name: ("Point".to_string(), 0..5),
                generics: vec![],
                fields: vec![
                    ("x".to_string(), TypeAnnot::Float, 6..7),
                    ("y".to_string(), TypeAnnot::Float, 8..9),
                ],
            }),
            0..10,
        ),
        (
            ASTNode::Expr((
                Expr::Let {
                    var: "p".to_string(),
                    type_annot: None,
                    value: Box::new((
                        Expr::Call {
                            function: Box::new((Expr::Variable("Point".to_string()), 20..25)),
                            args: vec![(Expr::Float(1.0), 26..29), (Expr::Float(2.0), 30..33)],
                        },
                        20..34,
                    )),
                },
                15..35,
            )),
            15..35,
        ),
        (
            ASTNode::Expr((
                Expr::StructAccess {
                    struct_val: Box::new((Expr::Variable("p".to_string()), 40..41)),
                    field_name: "x".to_string(),
                },
                40..43,
            )),
            40..43,
        ),
    ];

    let mut type_env = TypeEnv::new("test".to_string());
    let typed_ast = type_env.ast_to_typed_ast(program);
    assert!(type_env.errors.is_empty());

    // Check struct access
    if let TypedASTNode::Expr((TypedExpr { kind, ty, .. }, _)) = &typed_ast[2] {
        assert!(matches!(kind, TypedExprKind::StructAccess { .. }));
        assert_eq!(*ty, t_float!());
    }
}

#[test]
fn test_if_else_expression() {
    let program = vec![(
        ASTNode::Expr((
            Expr::IfElse {
                condition: Box::new((Expr::Bool(true), 0..4)),
                if_branch: Box::new((Expr::Int(1), 8..9)),
                else_branch: Some(Box::new((Expr::Int(0), 15..16))),
            },
            0..17,
        )),
        0..17,
    )];

    let mut type_env = TypeEnv::new("test".to_string());
    let typed_ast = type_env.ast_to_typed_ast(program);
    assert!(type_env.errors.is_empty());

    if let TypedASTNode::Expr((TypedExpr { kind, ty, .. }, _)) = &typed_ast[0] {
        assert!(matches!(kind, TypedExprKind::IfElse { .. }));
        assert_eq!(*ty, t_int!());
    }
}

#[test]
fn test_array_operations() {
    let program = vec![
        (
            ASTNode::Expr((
                Expr::Let {
                    var: "arr".to_string(),
                    type_annot: None,
                    value: Box::new((
                        Expr::Array {
                            elements: vec![
                                (Expr::Int(1), 10..11),
                                (Expr::Int(2), 12..13),
                                (Expr::Int(3), 14..15),
                            ],
                        },
                        7..16,
                    )),
                },
                0..17,
            )),
            0..17,
        ),
        (
            ASTNode::Expr((
                Expr::Index {
                    array: Box::new((Expr::Variable("arr".to_string()), 20..23)),
                    index: Box::new((Expr::Int(1), 24..25)),
                },
                20..26,
            )),
            20..26,
        ),
    ];

    let mut type_env = TypeEnv::new("test".to_string());
    let typed_ast = type_env.ast_to_typed_ast(program);
    assert!(type_env.errors.is_empty());

    // Check array index
    if let TypedASTNode::Expr((TypedExpr { kind, ty, .. }, _)) = &typed_ast[1] {
        assert!(matches!(kind, TypedExprKind::Index { .. }));
        assert_eq!(*ty, t_int!());
    }
}

#[test]
#[should_panic]
fn test_type_mismatch_errors() {
    let program = vec![
        (
            ASTNode::Expr((
                Expr::Let {
                    var: "x".to_string(),
                    type_annot: Some((TypeAnnot::Int, 0..3)),
                    value: Box::new((Expr::Float(3.14), 6..10)),
                },
                0..11,
            )),
            0..11,
        ),
        (
            ASTNode::Expr((
                Expr::BinOp {
                    operator: BinOp::Add,
                    l_value: Box::new((Expr::Int(5), 15..16)),
                    r_value: Box::new((Expr::Bool(true), 17..21)),
                },
                15..22,
            )),
            15..22,
        ),
    ];

    let mut type_env = TypeEnv::new("test".to_string());
    let _ = type_env.ast_to_typed_ast(program);

    // Should have 2 errors:
    // 1. Int vs Float in let binding
    // 2. Int vs Bool in addition
    assert_eq!(type_env.errors.len(), 2);
}

#[test]
fn test_generic_function() {
    let program = vec![
        (
            ASTNode::Function(Function {
                name: "identity".to_string(),
                args: vec![("x".to_string(), None, 15..21)],
                body: Box::new((Expr::Variable("x".to_string()), 30..31)),
                return_type: None,
            }),
            0..32,
        ),
        (
            ASTNode::Expr((
                Expr::Call {
                    function: Box::new((Expr::Variable("identity".to_string()), 40..48)),
                    args: vec![(Expr::Int(42), 49..51)],
                },
                40..52,
            )),
            40..52,
        ),
    ];

    let mut type_env = TypeEnv::new("test".to_string());
    let typed_ast = type_env.ast_to_typed_ast(program);
    let resolved_ast = type_env.resolve_all(typed_ast);
    let mono_ast = type_env.monomorphize_ast(resolved_ast);
    assert!(type_env.errors.is_empty());

    // Check function call
    if let TypedASTNode::Expr((TypedExpr { kind, ty, .. }, _)) = &mono_ast[1] {
        assert!(matches!(kind, TypedExprKind::Call { .. }));
        assert_eq!(*ty, t_int!());
    }
}

#[test]
fn test_do_block() {
    let program = vec![(
        ASTNode::Expr((
            Expr::Do {
                expressions: vec![
                    (
                        Expr::Let {
                            var: "a".to_string(),
                            type_annot: None,
                            value: Box::new((Expr::Int(1), 5..6)),
                        },
                        0..7,
                    ),
                    (
                        Expr::Let {
                            var: "b".to_string(),
                            type_annot: None,
                            value: Box::new((Expr::Int(2), 10..11)),
                        },
                        8..12,
                    ),
                    (
                        Expr::BinOp {
                            operator: BinOp::Add,
                            l_value: Box::new((Expr::Variable("a".to_string()), 15..16)),
                            r_value: Box::new((Expr::Variable("b".to_string()), 17..18)),
                        },
                        15..19,
                    ),
                ],
            },
            0..20,
        )),
        0..20,
    )];

    let mut type_env = TypeEnv::new("test".to_string());
    let typed_ast = type_env.ast_to_typed_ast(program);
    assert!(type_env.errors.is_empty());

    if let TypedASTNode::Expr((TypedExpr { kind, ty, .. }, _)) = &typed_ast[0] {
        assert!(matches!(kind, TypedExprKind::Do { .. }));
        assert_eq!(*ty, t_int!());
    }
}

// #[test]
// fn test_tuple_operations() {
//     let program = vec![
//         (
//             ASTNode::Expr((
//                 Expr::Let {
//                     var: "t".to_string(),
//                     type_annot: None,
//                     value: Box::new((
//                         Expr::Tuple(vec![(Expr::Int(1), 10..11), (Expr::Bool(true), 12..16)]),
//                         7..17,
//                     )),
//                 },
//                 0..18,
//             )),
//             0..18,
//         ),
//         (
//             ASTNode::Expr((
//                 Expr::Call {
//                     function: Box::new((Expr::Variable("t".to_string()), 22..23)),
//                     args: vec![(Expr::Int(0), 24..25)],
//                 },
//                 22..26,
//             )),
//             22..26,
//         ),
//     ];

//     let mut type_env = TypeEnv::new("test".to_string());
//     let typed_ast = type_env.ast_to_typed_ast(program);
//     assert!(type_env.errors.is_empty());

//     // Check tuple index
//     if let TypedASTNode::Expr((TypedExpr { kind, ty, .. }, _)) = &typed_ast[1] {
//         assert!(matches!(kind, TypedExprKind::Call { .. }));
//         assert_eq!(*ty, t_int!());
//     }
// }

#[test]
fn test_enum_definition_and_usage() {
    let program = vec![
        // Define an Option enum
        (
            ASTNode::Enum(
                Enum {
                    name: ("Option".to_string(), 0..6),
                    generics: vec![("T".to_string(), 7..8)],
                    variants: vec![
                        EnumVariant {
                            name: ("Some".to_string(), 10..14),
                            kind: EnumVariantKind::Tuple(vec![(
                                TypeAnnot::Boring("T".to_string()),
                                15..16,
                            )]),
                            range: 10..16,
                        },
                        EnumVariant {
                            name: ("None".to_string(), 18..22),
                            kind: EnumVariantKind::Unit,
                            range: 18..22,
                        },
                    ],
                },
                0..23,
            ),
            0..23,
        ),
        // Use the Option enum
        (
            ASTNode::Expr((
                Expr::Let {
                    var: "x".to_string(),
                    type_annot: Some((
                        TypeAnnot::Generic("Option".to_string(), vec![TypeAnnot::Int]),
                        25..33,
                    )),
                    value: Box::new((
                        Expr::EnumVariant {
                            enum_name: "Option".to_string(),
                            variant_name: "Some".to_string(),
                            fields: vec![(None, (Expr::Int(42), 38..40))],
                            range: 35..41,
                        },
                        35..41,
                    )),
                },
                24..42,
            )),
            24..42,
        ),
    ];

    let mut type_env = TypeEnv::new("test".to_string());
    let typed_ast = type_env.ast_to_typed_ast(program);
    // println!("typed_ast {:?}", typed_ast);

    let resolved_ast = type_env.resolve_all(typed_ast);
    // println!("resolved_ast {:?}", resolved_ast);

    let mono_ast = type_env.monomorphize_ast(resolved_ast);
    // println!("mono_ast {:?}", mono_ast);

    assert!(type_env.errors.is_empty());

    // Check that x has type Option<int>
    if let TypedASTNode::Expr((TypedExpr { kind, ty, .. }, _)) = &mono_ast[1] {
        if let TypedExprKind::Let { var, value: _ } = kind {
            assert_eq!(var, "x");
            assert_eq!(
                *ty,
                Arc::new(Type::Constructor {
                    name: "Option".to_string(),
                    generics: vec![t_int!()],
                    traits: vec![],
                })
            );
        } else {
            panic!("Expected let expression");
        }
    } else {
        panic!("Expected expression");
    }
}

#[test]
fn test_enum_pattern_matching() {
    let program = vec![
        // Define an Option enum
        (
            ASTNode::Enum(
                Enum {
                    name: ("Option".to_string(), 0..6),
                    generics: vec![("T".to_string(), 7..8)],
                    variants: vec![
                        EnumVariant {
                            name: ("Some".to_string(), 10..14),
                            kind: EnumVariantKind::Tuple(vec![(
                                TypeAnnot::Boring("T".to_string()),
                                15..16,
                            )]),
                            range: 10..16,
                        },
                        EnumVariant {
                            name: ("None".to_string(), 18..22),
                            kind: EnumVariantKind::Unit,
                            range: 18..22,
                        },
                    ],
                },
                0..23,
            ),
            0..23,
        ),
        // Create an Option value
        (
            ASTNode::Expr((
                Expr::Let {
                    var: "x".to_string(),
                    type_annot: None,
                    value: Box::new((
                        Expr::EnumVariant {
                            enum_name: "Option".to_string(),
                            variant_name: "Some".to_string(),
                            fields: vec![(None, (Expr::Int(42), 30..32))],
                            range: 25..33,
                        },
                        25..33,
                    )),
                },
                24..34,
            )),
            24..34,
        ),
        // Pattern match on the Option
        (
            ASTNode::Expr((
                Expr::Match {
                    expr: Box::new((Expr::Variable("x".to_string()), 40..41)),
                    arms: vec![
                        MatchArm {
                            pattern: Pattern::Guard(
                                Box::new((
                                    Pattern::EnumVariant {
                                        enum_name: Some("Option".to_string()),
                                        variant_name: "Some".to_string(),
                                        subpatterns: vec![(
                                            Pattern::Variable("value".to_string()),
                                            55..60,
                                        )],
                                    },
                                    50..75,
                                )),
                                (
                                    Expr::BinOp {
                                        operator: BinOp::Sub,
                                        l_value: Box::new((
                                            Expr::Variable("value".to_string()),
                                            0..0,
                                        )),
                                        r_value: Box::new((Expr::Int(0), 0..0)),
                                    },
                                    70..75,
                                ),
                            ),
                            body: Box::new((Expr::Variable("value".to_string()), 70..75)),
                            range: 50..75,
                        },
                        MatchArm {
                            pattern: Pattern::EnumVariant {
                                enum_name: Some("Option".to_string()),
                                variant_name: "Some".to_string(),
                                subpatterns: vec![(Pattern::Variable("value".to_string()), 55..60)],
                            },
                            body: Box::new((Expr::Variable("value".to_string()), 70..75)),
                            range: 50..75,
                        },
                        MatchArm {
                            pattern: Pattern::EnumVariant {
                                enum_name: Some("Option".to_string()),
                                variant_name: "None".to_string(),
                                subpatterns: vec![],
                            },
                            body: Box::new((Expr::Int(0), 85..86)),
                            range: 80..86,
                        },
                    ],
                    range: 37..87,
                },
                37..87,
            )),
            37..87,
        ),
    ];

    let mut type_env = TypeEnv::new("test".to_string());
    let typed_ast = type_env.ast_to_typed_ast(program);

    // Resolve types
    let resolved_ast = type_env.resolve_all(typed_ast);

    // Monomorphize
    let mono_ast = type_env.monomorphize_ast(resolved_ast);

    assert!(type_env.errors.is_empty());

    // println!("{:#?}", mono_ast);

    // Check that the match expression has type int
    if let TypedASTNode::Expr((TypedExpr { kind: _, ty, .. }, _)) = &mono_ast[2] {
        let resolved_ty = type_env.resolve(ty.clone());
        assert_eq!(
            *resolved_ty,
            *t_int!(),
            "Match expression should return int"
        );
    } else {
        panic!("Expected match expression");
    }
}

#[test]
#[should_panic]
fn test_enum_pattern_matching_error() {
    let program = vec![
        // Define an Option enum
        (
            ASTNode::Enum(
                Enum {
                    name: ("Option".to_string(), 0..6),
                    generics: vec![("T".to_string(), 7..8)],
                    variants: vec![
                        EnumVariant {
                            name: ("Some".to_string(), 10..14),
                            kind: EnumVariantKind::Tuple(vec![(
                                TypeAnnot::Boring("T".to_string()),
                                15..16,
                            )]),
                            range: 10..16,
                        },
                        EnumVariant {
                            name: ("None".to_string(), 18..22),
                            kind: EnumVariantKind::Unit,
                            range: 18..22,
                        },
                    ],
                },
                0..23,
            ),
            0..23,
        ),
        // Create an Option value
        (
            ASTNode::Expr((
                Expr::Let {
                    var: "x".to_string(),
                    type_annot: None,
                    value: Box::new((
                        Expr::EnumVariant {
                            enum_name: "Option".to_string(),
                            variant_name: "Some".to_string(),
                            fields: vec![(None, (Expr::Int(42), 30..32))],
                            range: 25..33,
                        },
                        25..33,
                    )),
                },
                24..34,
            )),
            24..34,
        ),
        // Incorrect pattern match (wrong arm types)
        (
            ASTNode::Expr((
                Expr::Match {
                    expr: Box::new((Expr::Variable("x".to_string()), 40..41)),
                    arms: vec![
                        MatchArm {
                            pattern: Pattern::EnumVariant {
                                enum_name: Some("Option".to_string()),
                                variant_name: "Some".to_string(),
                                subpatterns: vec![(Pattern::Variable("value".to_string()), 55..60)],
                            },
                            body: Box::new((Expr::Variable("value".to_string()), 70..75)),
                            range: 50..75,
                        },
                        MatchArm {
                            pattern: Pattern::EnumVariant {
                                enum_name: Some("Option".to_string()),
                                variant_name: "None".to_string(),
                                subpatterns: vec![],
                            },
                            body: Box::new((Expr::String("nothing".to_string()), 85..93)),
                            range: 80..93,
                        },
                    ],
                    range: 37..94,
                },
                37..94,
            )),
            37..94,
        ),
    ];

    let mut type_env = TypeEnv::new("test".to_string());
    let typed_ast = type_env.ast_to_typed_ast(program);

    // Resolve types
    let resolved_ast = type_env.resolve_all(typed_ast);
    let _ = type_env.monomorphize_ast(resolved_ast);

    // Should have error: match arms have different types (int vs string)
    // assert!(!type_env.errors.is_empty());

    // ill do it later
}

#[test]
#[should_panic]
fn test_enum_pattern_matching_wrong_field_type() {
    let program = vec![
        // Define an Option enum
        (
            ASTNode::Enum(
                Enum {
                    name: ("Option".to_string(), 0..6),
                    generics: vec![("T".to_string(), 7..8)],
                    variants: vec![
                        EnumVariant {
                            name: ("Some".to_string(), 10..14),
                            kind: EnumVariantKind::Tuple(vec![(
                                TypeAnnot::Boring("T".to_string()),
                                15..16,
                            )]),
                            range: 10..16,
                        },
                        EnumVariant {
                            name: ("None".to_string(), 18..22),
                            kind: EnumVariantKind::Unit,
                            range: 18..22,
                        },
                    ],
                },
                0..23,
            ),
            0..23,
        ),
        // Pattern match with wrong field type
        (
            ASTNode::Expr((
                Expr::Match {
                    expr: Box::new((Expr::Variable("opt".to_string()), 30..33)),
                    arms: vec![
                        MatchArm {
                            pattern: Pattern::EnumVariant {
                                enum_name: Some("Option".to_string()),
                                variant_name: "Some".to_string(),
                                subpatterns: vec![(
                                    Pattern::Literal(Expr::String("text".to_string())),
                                    45..51,
                                )],
                            },
                            body: Box::new((Expr::Int(1), 60..61)),
                            range: 40..61,
                        },
                        MatchArm {
                            pattern: Pattern::EnumVariant {
                                enum_name: Some("Option".to_string()),
                                variant_name: "None".to_string(),
                                subpatterns: vec![],
                            },
                            body: Box::new((Expr::Int(0), 70..71)),
                            range: 65..71,
                        },
                    ],
                    range: 27..72,
                },
                27..72,
            )),
            27..72,
        ),
    ];

    let mut type_env = TypeEnv::new("test".to_string());
    // Add variable with Option<int> type
    type_env.insert_var(
        "opt".to_string(),
        Arc::new(Type::Constructor {
            name: "Option".to_string(),
            generics: vec![t_int!()],
            traits: vec![],
        }),
    );

    let _ = type_env.ast_to_typed_ast(program);

    // Should have error: string pattern doesn't match int type
    assert!(!type_env.errors.is_empty());
}

#[test]
fn test_basic_enum_matching() {
    let mut env = TypeEnv::new("test".to_string());

    // Define Option enum
    let option_enum = Enum {
        name: ("Option".to_string(), 0..0),
        generics: vec![("T".to_string(), 0..0)],
        variants: vec![
            EnumVariant {
                name: ("Some".to_string(), 0..0),
                kind: EnumVariantKind::Tuple(vec![(TypeAnnot::Boring("T".to_string()), 0..0)]),
                range: 0..0,
            },
            EnumVariant {
                name: ("None".to_string(), 0..0),
                kind: EnumVariantKind::Unit,
                range: 0..0,
            },
        ],
    };

    // Register enum in environment
    let (_, _) = env.enum_to_typed_enum((&option_enum, &(0..0)));

    // Create expression: let x = Option::Some(42)
    let some_expr = Expr::EnumVariant {
        enum_name: "Option".to_string(),
        variant_name: "Some".to_string(),
        fields: vec![(None, (Expr::Int(42), 0..0))],
        range: 0..0,
    };

    // Create match expression
    let match_expr = Expr::Match {
        expr: Box::new((Expr::Variable("x".to_string()), 0..0)),
        arms: vec![
            MatchArm {
                pattern: Pattern::EnumVariant {
                    enum_name: Some("Option".to_string()),
                    variant_name: "Some".to_string(),
                    subpatterns: vec![(Pattern::Variable("a".to_string()), 0..0)],
                },
                body: Box::new((Expr::Variable("a".to_string()), 0..0)),
                range: 0..0,
            },
            MatchArm {
                pattern: Pattern::EnumVariant {
                    enum_name: Some("Option".to_string()),
                    variant_name: "None".to_string(),
                    subpatterns: vec![],
                },
                body: Box::new((Expr::Int(0), 0..0)),
                range: 0..0,
            },
        ],
        range: 0..0,
    };

    // Type check expressions
    let typed_some = env.expr_to_typed_expr((&some_expr, &(0..0)));
    let typed_match = env.expr_to_typed_expr((&match_expr, &(0..0)));

    // RESOLVE TYPES
    let resolved_some = env.resolve_expr(typed_some);
    let resolved_match = env.resolve_expr(typed_match);

    // Validate types
    assert_eq!(type_string(&resolved_some.ty), "Option<int>");

    assert_eq!(type_string(&resolved_match.ty), "int");

    // Validate pattern variable type
    if let TypedExprKind::Match { arms, .. } = &resolved_match.kind {
        if let TypedPattern::EnumVariant { subpatterns, .. } = &arms[0].pattern {
            if let (TypedPattern::Variable(_, ty), _) = &subpatterns[0] {
                assert_eq!(type_string(ty), "int");
            } else {
                panic!("Expected variable pattern");
            }
        }
    }
}

#[test]
fn test_generic_enum_matching() {
    let mut env = TypeEnv::new("test".to_string());

    // Define Result enum
    let result_enum = Enum {
        name: ("Result".to_string(), 0..0),
        generics: vec![("T".to_string(), 0..0), ("E".to_string(), 0..0)],
        variants: vec![
            EnumVariant {
                name: ("Ok".to_string(), 0..0),
                kind: EnumVariantKind::Tuple(vec![(TypeAnnot::Boring("T".to_string()), 0..0)]),
                range: 0..0,
            },
            EnumVariant {
                name: ("Err".to_string(), 0..0),
                kind: EnumVariantKind::Tuple(vec![(TypeAnnot::Boring("E".to_string()), 0..0)]),
                range: 0..0,
            },
        ],
    };

    // Register enum
    let (_, _) = env.enum_to_typed_enum((&result_enum, &(0..0)));

    // Create expression: let res = Result::Ok(42)
    let ok_expr = Expr::EnumVariant {
        enum_name: "Result".to_string(),
        variant_name: "Ok".to_string(),
        fields: vec![(None, (Expr::Int(42), 0..0))],
        range: 0..0,
    };

    // Create match expression
    let match_expr = Expr::Match {
        expr: Box::new((Expr::Variable("res".to_string()), 0..0)),
        arms: vec![
            MatchArm {
                pattern: Pattern::EnumVariant {
                    enum_name: Some("Result".to_string()),
                    variant_name: "Ok".to_string(),
                    subpatterns: vec![(Pattern::Variable("value".to_string()), 0..0)],
                },
                body: Box::new((Expr::Variable("value".to_string()), 0..0)),
                range: 0..0,
            },
            MatchArm {
                pattern: Pattern::EnumVariant {
                    enum_name: Some("Result".to_string()),
                    variant_name: "Err".to_string(),
                    subpatterns: vec![(Pattern::Variable("error".to_string()), 0..0)],
                },
                body: Box::new((Expr::Int(-1), 0..0)),
                range: 0..0,
            },
        ],
        range: 0..0,
    };

    // Type check
    let typed_ok = env.expr_to_typed_expr((&ok_expr, &(0..0)));
    let typed_match = env.expr_to_typed_expr((&match_expr, &(0..0)));

    let resolved_ok = env.resolve_expr(typed_ok);
    let resolved_match = env.resolve_expr(typed_match);

    // Validate types
    assert_eq!(type_string(&resolved_ok.ty), "Result<int, string>");

    assert_eq!(type_string(&resolved_match.ty), "int");
}

#[test]
fn test_union_pattern_matching() {
    let mut env = TypeEnv::new("test".to_string());

    // Define Status enum
    let status_enum = Enum {
        name: ("Status".to_string(), 0..0),
        generics: vec![],
        variants: vec![
            EnumVariant {
                name: ("Success".to_string(), 0..0),
                kind: EnumVariantKind::Unit,
                range: 0..0,
            },
            EnumVariant {
                name: ("Warning".to_string(), 0..0),
                kind: EnumVariantKind::Unit,
                range: 0..0,
            },
            EnumVariant {
                name: ("Error".to_string(), 0..0),
                kind: EnumVariantKind::Unit,
                range: 0..0,
            },
        ],
    };

    // Register enum
    let (_, _) = env.enum_to_typed_enum((&status_enum, &(0..0)));

    // Create match expression with union pattern
    let match_expr = Expr::Match {
        expr: Box::new((Expr::Variable("status".to_string()), 0..0)),
        arms: vec![
            MatchArm {
                pattern: Pattern::Union(vec![
                    (
                        Pattern::EnumVariant {
                            enum_name: Some("Status".to_string()),
                            variant_name: "Success".to_string(),
                            subpatterns: vec![],
                        },
                        0..0,
                    ),
                    (
                        Pattern::EnumVariant {
                            enum_name: Some("Status".to_string()),
                            variant_name: "Warning".to_string(),
                            subpatterns: vec![],
                        },
                        0..0,
                    ),
                ]),
                body: Box::new((Expr::Int(1), 0..0)),
                range: 0..0,
            },
            MatchArm {
                pattern: Pattern::EnumVariant {
                    enum_name: Some("Status".to_string()),
                    variant_name: "Error".to_string(),
                    subpatterns: vec![],
                },
                body: Box::new((Expr::Int(0), 0..0)),
                range: 0..0,
            },
        ],
        range: 0..0,
    };

    // Type check
    let typed_match = env.expr_to_typed_expr((&match_expr, &(0..0)));

    // Validate type
    assert_eq!(type_string(&typed_match.ty), "int");
}

#[test]
fn test_complex_pattern_matching() {
    let mut env = TypeEnv::new("test".to_string());

    // Define complex enum
    let data_enum = Enum {
        name: ("Data".to_string(), 0..0),
        generics: vec![("T".to_string(), 0..0)],
        variants: vec![
            EnumVariant {
                name: ("Point".to_string(), 0..0),
                kind: EnumVariantKind::Tuple(vec![
                    (TypeAnnot::Boring("T".to_string()), 0..0),
                    (TypeAnnot::Boring("T".to_string()), 0..0),
                ]),
                range: 0..0,
            },
            EnumVariant {
                name: ("Line".to_string(), 0..0),
                kind: EnumVariantKind::Struct(vec![
                    (
                        "start".to_string(),
                        TypeAnnot::Boring("T".to_string()),
                        0..0,
                    ),
                    ("end".to_string(), TypeAnnot::Boring("T".to_string()), 0..0),
                ]),
                range: 0..0,
            },
        ],
    };

    // Register enum
    let (_, _) = env.enum_to_typed_enum((&data_enum, &(0..0)));

    // Create expression: let point = Data::Point(1.0, 2.0)
    let point_expr = Expr::EnumVariant {
        enum_name: "Data".to_string(),
        variant_name: "Point".to_string(),
        fields: vec![
            (None, (Expr::Float(1.0), 0..0)),
            (None, (Expr::Float(2.0), 0..0)),
        ],
        range: 0..0,
    };

    // Create match expression
    let match_expr = Expr::Match {
        expr: Box::new((Expr::Variable("data".to_string()), 0..0)),
        arms: vec![
            MatchArm {
                pattern: Pattern::EnumVariant {
                    enum_name: Some("Data".to_string()),
                    variant_name: "Point".to_string(),
                    subpatterns: vec![
                        (Pattern::Variable("x".to_string()), 0..0),
                        (Pattern::Variable("y".to_string()), 0..0),
                    ],
                },
                body: Box::new((
                    Expr::BinOp {
                        operator: BinOp::Add,
                        l_value: Box::new((Expr::Variable("x".to_string()), 0..0)),
                        r_value: Box::new((Expr::Variable("y".to_string()), 0..0)),
                    },
                    0..0,
                )),
                range: 0..0,
            },
            MatchArm {
                pattern: Pattern::EnumVariant {
                    enum_name: Some("Data".to_string()),
                    variant_name: "Line".to_string(),
                    subpatterns: vec![
                        (Pattern::Variable("start".to_string()), 0..0),
                        (Pattern::Variable("end".to_string()), 0..0),
                    ],
                },
                body: Box::new((
                    Expr::BinOp {
                        operator: BinOp::Sub,
                        l_value: Box::new((Expr::Variable("end".to_string()), 0..0)),
                        r_value: Box::new((Expr::Variable("start".to_string()), 0..0)),
                    },
                    0..0,
                )),
                range: 0..0,
            },
        ],
        range: 0..0,
    };

    // Type check
    let typed_point = env.expr_to_typed_expr((&point_expr, &(0..0)));
    let typed_match = env.expr_to_typed_expr((&match_expr, &(0..0)));

    let resolved_point = env.resolve_expr(typed_point);
    let resolved_match = env.resolve_expr(typed_match);

    // Validate types
    assert_eq!(type_string(&resolved_point.ty), "Data<float>");

    assert_eq!(type_string(&resolved_match.ty), "float");
}

#[test]
fn test_recursive_resolution() {
    let mut env = TypeEnv::new("test".to_string());

    // Define nested data structure
    let list_enum = Enum {
        name: ("List".to_string(), 0..0),
        generics: vec![("T".to_string(), 0..0)],
        variants: vec![
            EnumVariant {
                name: ("Cons".to_string(), 0..0),
                kind: EnumVariantKind::Tuple(vec![
                    (TypeAnnot::Boring("T".to_string()), 0..0),
                    (
                        TypeAnnot::Generic(
                            "List".to_string(),
                            vec![TypeAnnot::Boring("T".to_string())],
                        ),
                        0..0,
                    ),
                ]),
                range: 0..0,
            },
            EnumVariant {
                name: ("Nil".to_string(), 0..0),
                kind: EnumVariantKind::Unit,
                range: 0..0,
            },
        ],
    };

    // Register enum
    let (_, _) = env.enum_to_typed_enum((&list_enum, &(0..0)));

    // Create expression: let list = List::Cons(1, List::Cons(2, List::Nil))
    let list_expr = Expr::EnumVariant {
        enum_name: "List".to_string(),
        variant_name: "Cons".to_string(),
        fields: vec![
            (None, (Expr::Int(1), 0..0)),
            (
                None,
                (
                    Expr::EnumVariant {
                        enum_name: "List".to_string(),
                        variant_name: "Cons".to_string(),
                        fields: vec![
                            (None, (Expr::Int(2), 0..0)),
                            (
                                None,
                                (
                                    Expr::EnumVariant {
                                        enum_name: "List".to_string(),
                                        variant_name: "Nil".to_string(),
                                        fields: vec![],
                                        range: 0..0,
                                    },
                                    0..0,
                                ),
                            ),
                        ],
                        range: 0..0,
                    },
                    0..0,
                ),
            ),
        ],
        range: 0..0,
    };

    // Create match expression
    let match_expr = Expr::Match {
        expr: Box::new((Expr::Variable("list".to_string()), 0..0)),
        arms: vec![
            MatchArm {
                pattern: Pattern::EnumVariant {
                    enum_name: Some("List".to_string()),
                    variant_name: "Cons".to_string(),
                    subpatterns: vec![
                        (Pattern::Variable("head".to_string()), 0..0),
                        (Pattern::Variable("tail".to_string()), 0..0),
                    ],
                },
                body: Box::new((Expr::Variable("head".to_string()), 0..0)),
                range: 0..0,
            },
            MatchArm {
                pattern: Pattern::EnumVariant {
                    enum_name: Some("List".to_string()),
                    variant_name: "Nil".to_string(),
                    subpatterns: vec![],
                },
                body: Box::new((Expr::Int(0), 0..0)),
                range: 0..0,
            },
        ],
        range: 0..0,
    };

    // Type check
    let typed_list = env.expr_to_typed_expr((&list_expr, &(0..0)));
    let typed_match = env.expr_to_typed_expr((&match_expr, &(0..0)));

    let resolved_list = env.resolve_expr(typed_list);
    let resolved_match = env.resolve_expr(typed_match);

    // Validate types
    assert_eq!(type_string(&resolved_list.ty), "List<int>");

    assert_eq!(type_string(&resolved_match.ty), "int");

    // Validate pattern variable types
    if let TypedExprKind::Match { arms, .. } = &resolved_match.kind {
        if let TypedPattern::EnumVariant { subpatterns, .. } = &arms[0].pattern {
            // Head should be int
            if let (TypedPattern::Variable(_, ty), _) = &subpatterns[0] {
                assert_eq!(type_string(&ty), "int");
            }

            // Tail should be List<int>
            if let (TypedPattern::Variable(_, ty), _) = &subpatterns[1] {
                assert_eq!(type_string(&ty), "List<int>");
            }
        }
    }
}

#[test]
fn test_union_pattern_with_variables() {
    let mut env = TypeEnv::new("test".to_string());

    // Define simple enum
    let simple_enum = Enum {
        name: ("Simple".to_string(), 0..0),
        generics: vec![],
        variants: vec![
            EnumVariant {
                name: ("A".to_string(), 0..0),
                kind: EnumVariantKind::Tuple(vec![(TypeAnnot::Int, 0..0)]),
                range: 0..0,
            },
            EnumVariant {
                name: ("B".to_string(), 0..0),
                kind: EnumVariantKind::Tuple(vec![(TypeAnnot::String, 0..0)]),
                range: 0..0,
            },
        ],
    };

    // Register enum
    let (_, _) = env.enum_to_typed_enum((&simple_enum, &(0..0)));

    // Create match expression with union pattern
    let match_expr = Expr::Match {
        expr: Box::new((Expr::Variable("value".to_string()), 0..0)),
        arms: vec![MatchArm {
            pattern: Pattern::Union(vec![
                (
                    Pattern::EnumVariant {
                        enum_name: Some("Simple".to_string()),
                        variant_name: "A".to_string(),
                        subpatterns: vec![(Pattern::Variable("num".to_string()), 0..0)],
                    },
                    0..0,
                ),
                (
                    Pattern::EnumVariant {
                        enum_name: Some("Simple".to_string()),
                        variant_name: "B".to_string(),
                        subpatterns: vec![(Pattern::Variable("str".to_string()), 0..0)],
                    },
                    0..0,
                ),
            ]),
            body: Box::new((
                Expr::Int(1), // Should be int to match both arms
                0..0,
            )),
            range: 0..0,
        }],
        range: 0..0,
    };

    // Type check
    let typed_match = env.expr_to_typed_expr((&match_expr, &(0..0)));

    // Validate types
    assert_eq!(type_string(&typed_match.ty), "int");

    // Validate pattern variable types
    if let TypedExprKind::Match { arms, .. } = &typed_match.kind {
        if let TypedPattern::Union(subpatterns) = &arms[0].pattern {
            // First subpattern: num should be int
            if let (TypedPattern::EnumVariant { subpatterns, .. }, _) = &subpatterns[0] {
                if let (TypedPattern::Variable(_, ty), _) = &subpatterns[0] {
                    assert_eq!(type_string(ty), "int");
                }
            }

            // Second subpattern: str should be string
            if let (TypedPattern::EnumVariant { subpatterns, .. }, _) = &subpatterns[1] {
                if let (TypedPattern::Variable(_, ty), _) = &subpatterns[0] {
                    assert_eq!(type_string(ty), "string");
                }
            }
        }
    }
}

#[test]
fn test_valid_return_inside_function() {
    let mut env = TypeEnv::new("test".to_string());

    // Create function with return statement
    let func = Function {
        name: "test".to_string(),
        args: vec![],
        body: Box::new((Expr::Return(Box::new(Expr::Int(42))), 0..0)),
        return_type: Some((TypeAnnot::Int, 0..0)),
    };

    // Type check function - should not panic
    let (typed_func, _) = env.function_to_typed_function((&func, &(0..0)));

    // Verify return type
    assert_eq!(type_string(&typed_func.return_type.0), "int");

    // Verify body expression
    if let TypedExprKind::Return(inner) = &typed_func.body.0.kind {
        assert_eq!(type_string(&inner.ty), "int");
    } else {
        panic!("Expected return expression");
    }

    // Should have no errors
    assert!(env.errors.is_empty());
}

#[test]
#[should_panic(expected = "Return statement outside function")]
fn test_return_outside_function() {
    let mut env = TypeEnv::new("test".to_string());

    // Create return expression at top level
    let return_expr = Expr::Return(Box::new(Expr::Int(42)));

    // Type check return expression - should panic
    env.expr_to_typed_expr((&return_expr, &(0..0)));
}

#[test]
#[should_panic]
fn test_multiple_returns_in_function() {
    let mut env = TypeEnv::new("test".to_string());

    // Create function with multiple returns
    let func = Function {
        name: "abs".to_string(),
        args: vec![("x".to_string(), Some(TypeAnnot::Int), 0..0)],
        body: Box::new((
            Expr::IfElse {
                condition: Box::new((
                    Expr::BinOp {
                        operator: BinOp::Less,
                        l_value: Box::new((Expr::Variable("x".to_string()), 0..0)),
                        r_value: Box::new((Expr::Int(0), 0..0)),
                    },
                    0..0,
                )),
                if_branch: Box::new((
                    Expr::Return(Box::new(Expr::UnOp {
                        unop: UnOp::Minus,
                        expression: Box::new((Expr::Variable("x".to_string()), 0..0)),
                    })),
                    0..0,
                )),
                else_branch: Some(Box::new((
                    Expr::Return(Box::new(Expr::Variable("x".to_string()))),
                    0..0,
                ))),
            },
            0..0,
        )),
        return_type: Some((TypeAnnot::Int, 0..0)),
    };

    // Type check function
    let (typed_func, _) = env.function_to_typed_function((&func, &(0..0)));

    // Verify return type
    assert_eq!(type_string(&typed_func.return_type.0), "int");

    // Should have no errors
    assert!(!env.errors.is_empty());
}

#[test]
#[should_panic(expected = "Nested return")]
fn test_nested_return() {
    let mut env = TypeEnv::new("test".to_string());

    // Create function with nested return (invalid)
    let func = Function {
        name: "invalid".to_string(),
        args: vec![],
        body: Box::new((
            Expr::Return(Box::new(Expr::Return(Box::new(Expr::Int(42))))),
            0..0,
        )),
        return_type: Some((TypeAnnot::Int, 0..0)),
    };

    // Type check function - should panic on nested return
    env.function_to_typed_function((&func, &(0..0)));
}

#[test]
fn test_valid_nested_expressions() {
    let mut env = TypeEnv::new("test".to_string());

    // Create function with return containing expression (not nested return)
    let func = Function {
        name: "valid".to_string(),
        args: vec![],
        body: Box::new((
            Expr::Return(Box::new(Expr::BinOp {
                operator: BinOp::Add,
                l_value: Box::new((Expr::Int(20), 0..0)),
                r_value: Box::new((Expr::Int(22), 0..0)),
            })),
            0..0,
        )),
        return_type: Some((TypeAnnot::Int, 0..0)),
    };

    // Type check function - should succeed
    let (typed_func, _) = env.function_to_typed_function((&func, &(0..0)));

    // Verify return type
    assert_eq!(type_string(&typed_func.return_type.0), "int");

    // Verify body expression
    if let TypedExprKind::Return(inner) = &typed_func.body.0.kind {
        assert_eq!(type_string(&inner.ty), "int");
    } else {
        panic!("Expected return expression");
    }

    // Should have no errors
    assert!(env.errors.is_empty());
}

#[test]
fn test_lambda_basic() {
    let mut env = TypeEnv::new("test".to_string());

    // Create lambda: fn(a: int, b: int) -> int { a + b }
    let lambda = Expr::Lambda {
        args: vec![
            ("a".to_string(), Some(TypeAnnot::Int), 0..0),
            ("b".to_string(), Some(TypeAnnot::Int), 0..0),
        ],
        expression: Box::new((
            Expr::BinOp {
                operator: BinOp::Add,
                l_value: Box::new((Expr::Variable("a".to_string()), 0..0)),
                r_value: Box::new((Expr::Variable("b".to_string()), 0..0)),
            },
            0..0,
        )),
    };

    // Type check
    let typed_lambda = env.expr_to_typed_expr((&lambda, &(0..0)));
    let resolved = env.resolve_expr(typed_lambda);

    // Verify type
    assert_eq!(type_string(&resolved.ty), "fn(int, int) -> int");
}

#[test]
fn test_lambda_inferred_types() {
    let mut env = TypeEnv::new("test".to_string());

    // Create lambda: fn(a, b) a + b
    let lambda = Expr::Lambda {
        args: vec![("a".to_string(), None, 0..0), ("b".to_string(), None, 0..0)],
        expression: Box::new((
            Expr::BinOp {
                operator: BinOp::Add,
                l_value: Box::new((Expr::Variable("a".to_string()), 0..0)),
                r_value: Box::new((Expr::Variable("b".to_string()), 0..0)),
            },
            0..0,
        )),
    };

    // Type check
    let typed_lambda = env.expr_to_typed_expr((&lambda, &(0..0)));
    let resolved = env.resolve_expr(typed_lambda);

    // Verify type
    assert_eq!(type_string(&resolved.ty), "fn(?T2, ?T2) -> ?T2");
}

#[test]
fn test_lambda_closure() {
    let mut env = TypeEnv::new("test".to_string());

    // Add outer variable
    env.insert_var("x".to_string(), t_int!());

    // Create lambda: fn(a) a + x
    let lambda = Expr::Lambda {
        args: vec![("a".to_string(), None, 0..0)],
        expression: Box::new((
            Expr::BinOp {
                operator: BinOp::Add,
                l_value: Box::new((Expr::Variable("a".to_string()), 0..0)),
                r_value: Box::new((Expr::Variable("x".to_string()), 0..0)),
            },
            0..0,
        )),
    };

    // Type check
    let typed_lambda = env.expr_to_typed_expr((&lambda, &(0..0)));
    let resolved = env.resolve_expr(typed_lambda);

    // Verify type
    assert_eq!(type_string(&resolved.ty), "fn(int) -> int");
}

#[test]
fn test_lambda_application() {
    let mut env = TypeEnv::new("test".to_string());

    // Create lambda: fn(a: int, b: int) a + b
    let lambda = Expr::Lambda {
        args: vec![
            ("a".to_string(), Some(TypeAnnot::Int), 0..0),
            ("b".to_string(), Some(TypeAnnot::Int), 0..0),
        ],
        expression: Box::new((
            Expr::BinOp {
                operator: BinOp::Add,
                l_value: Box::new((Expr::Variable("a".to_string()), 0..0)),
                r_value: Box::new((Expr::Variable("b".to_string()), 0..0)),
            },
            0..0,
        )),
    };

    // Create application: (lambda)(3, 4)
    let application = Expr::Call {
        function: Box::new((lambda, 0..0)),
        args: vec![(Expr::Int(3), 0..0), (Expr::Int(4), 0..0)],
    };

    // Type check
    let typed_app = env.expr_to_typed_expr((&application, &(0..0)));
    let resolved = env.resolve_expr(typed_app);

    // Verify type
    assert_eq!(type_string(&resolved.ty), "int");
}

#[test]
fn test_nested_lambdas() {
    let mut env = TypeEnv::new("test".to_string());

    // Create nested lambda: fn(a) fn(b) a + b
    let inner_lambda = Expr::Lambda {
        args: vec![("b".to_string(), Some(TypeAnnot::Int), 0..0)],
        expression: Box::new((
            Expr::BinOp {
                operator: BinOp::Add,
                l_value: Box::new((Expr::Variable("a".to_string()), 0..0)),
                r_value: Box::new((Expr::Variable("b".to_string()), 0..0)),
            },
            0..0,
        )),
    };

    let outer_lambda = Expr::Lambda {
        args: vec![("a".to_string(), None, 0..0)],
        expression: Box::new((inner_lambda, 0..0)),
    };

    // Type check
    let typed_lambda = env.expr_to_typed_expr((&outer_lambda, &(0..0)));

    let ast_node = TypedASTNode::Expr((typed_lambda, 0..0));

    let resolved_ast = env.resolve_all(vec![ast_node]);

    if let TypedASTNode::Expr((resolved_expr, _)) = &resolved_ast[0] {
        let type_str = type_string(&resolved_expr.ty);
        assert_eq!(type_str, "fn(int) -> fn(int) -> int");
    } else {
        panic!("Expected expression node");
    }
}

#[test]
fn test_lambda_complex_body() {
    let mut env = TypeEnv::new("test".to_string());

    // Create lambda: fn(x: float) { let y = x * 2.0; y + 1.0 }
    let lambda = Expr::Lambda {
        args: vec![("x".to_string(), Some(TypeAnnot::Float), 0..0)],
        expression: Box::new((
            Expr::Do {
                expressions: vec![
                    (
                        Expr::Let {
                            var: "y".to_string(),
                            type_annot: None,
                            value: Box::new((
                                Expr::BinOp {
                                    operator: BinOp::Mul,
                                    l_value: Box::new((Expr::Variable("x".to_string()), 0..0)),
                                    r_value: Box::new((Expr::Float(2.0), 0..0)),
                                },
                                0..0,
                            )),
                        },
                        0..0,
                    ),
                    (
                        Expr::BinOp {
                            operator: BinOp::Add,
                            l_value: Box::new((Expr::Variable("y".to_string()), 0..0)),
                            r_value: Box::new((Expr::Float(1.0), 0..0)),
                        },
                        0..0,
                    ),
                ],
            },
            0..0,
        )),
    };

    // Type check
    let typed_lambda = env.expr_to_typed_expr((&lambda, &(0..0)));
    let resolved = env.resolve_expr(typed_lambda);

    // Verify type
    assert_eq!(type_string(&resolved.ty), "fn(float) -> float");

    // Verify no errors
    assert!(env.errors.is_empty());
}

#[test]
fn test_recursive_function() {
    // Initialize a TypeEnv for type checking
    let mut env = TypeEnv::new("test.rs".to_string());

    // Define the recursive call 'fact(val - 1)' separately to be embedded
    let fact_call_recursive = Expr::Call {
        function: Box::new((Expr::Variable("fact".to_string()), 41..44)), // Range for 'fact' in the call
        args: vec![(
            Expr::BinOp {
                operator: BinOp::Sub,
                l_value: Box::new((Expr::Variable("val".to_string()), 52..55)), // Range for 'val'
                r_value: Box::new((Expr::Int(1), 56..57)),                      // Range for '1'
            },
            52..57, // Range for the 'val - 1' expression
        )],
    };

    // Build the AST for the 'fact' function
    let fact_function = Function {
        name: "fact".to_string(),
        args: vec![("val".to_string(), None, 5..8)], // 'val' parameter with no explicit type annotation
        body: Box::new((
            Expr::IfElse {
                condition: Box::new((
                    Expr::BinOp {
                        operator: BinOp::LessEq,
                        l_value: Box::new((Expr::Variable("val".to_string()), 15..18)), // 'val' in condition
                        r_value: Box::new((Expr::Int(1), 22..23)), // '1' in condition
                    },
                    15..23, // Range for 'val <= 1'
                )),
                if_branch: Box::new((Expr::Int(1), 29..30)), // '1' in the if branch
                else_branch: Some(Box::new((
                    Expr::BinOp {
                        operator: BinOp::Mul,
                        l_value: Box::new((Expr::Variable("val".to_string()), 37..40)), // 'val' in else branch
                        r_value: Box::new((fact_call_recursive, 41..57)), // The recursive call 'fact(val - 1)'
                    },
                    37..57, // Range for 'val * fact(val - 1)'
                ))),
            },
            10..60, // Range for the entire if-else body
        )),
        return_type: None, // No explicit return type annotation
    };

    // Define the top-level program as a vector of ASTNodes
    // This includes the function definition and the final call to 'fact(4)'
    let fact_program = vec![
        (ASTNode::Function(fact_function), 0..0), // Range for the entire function definition node
        (
            ASTNode::Expr((
                Expr::Call {
                    function: Box::new((Expr::Variable("fact".to_string()), 62..66)), // Range for 'fact' in the top-level call
                    args: vec![(
                        Expr::Int(4), // Argument for the top-level call
                        67..68,       // Range for the '4' literal
                    )],
                },
                62..69, // Range for the 'fact(4)' expression
            )),
            0..0,
        ), // Range for the entire expression node
    ];

    // Attempt to type-check the AST
    let typed_ast = env.ast_to_typed_ast(fact_program);
    let typed_ast = env.resolve_all(typed_ast);
    let typed_ast = env.monomorphize_ast(typed_ast);

    assert!(
        !typed_ast.is_empty(),
        "The typed AST should not be empty after processing."
    );

    if let Some(TypedASTNode::Function((typed_fact_fn, _))) = typed_ast.get(0) {
        assert_eq!(
            typed_fact_fn.name, "fact",
            "Function name should be 'fact_int_to_int'"
        );
        assert_eq!(
            typed_fact_fn.args.len(),
            1,
            "Fact function should have 1 argument"
        );

        if let Some((arg_name, arg_type, _)) = typed_fact_fn.args.get(0) {
            assert_eq!(arg_name, "val", "Argument name should be 'val'");
            if let Type::Constructor { name, .. } = &**arg_type {
                assert_eq!(name, "int", "Argument 'val' should be inferred as 'int'");
            } else {
                panic!(
                    "Argument 'val' type is not a constructor type. it is {:?}",
                    arg_type
                );
            }
        }

        if let Type::Constructor { name, .. } = &*typed_fact_fn.return_type.0 {
            assert_eq!(
                name, "int",
                "Function return type should be inferred as 'int'"
            );
        } else {
            panic!("Function return type is not a constructor type.");
        }
    } else {
        panic!("The typed AST did not contain the 'fact' function at the expected position.");
    }
}

#[test]
fn test_function_type_resolution() {
    let mut env = TypeEnv::new("test.rs".to_string());

    // Create a function type with type variables
    let func_type = Arc::new(Type::Function {
        params: vec![tvar!(1), tvar!(2)],
        return_type: tvar!(1),
    });

    // Add substitutions
    env.substitutions.insert(1, t_int!());
    env.substitutions.insert(2, t_string!());

    // Resolve the function type
    let resolved = env.resolve(func_type);

    // Verify resolution
    if let Type::Function {
        params,
        return_type,
    } = &*resolved
    {
        assert_eq!(params.len(), 2);
        assert_eq!(type_string(&params[0]), "int");
        assert_eq!(type_string(&params[1]), "string");
        assert_eq!(type_string(return_type), "int");
    } else {
        panic!("Expected function type");
    }
}

#[test]
fn test_monomorphization() {
    let mut env = TypeEnv::new("test.rs".to_string());

    // Build identity function AST
    let identity_function = TypedFunction {
        name: "identity".to_string(),
        args: vec![("n".to_string(), tvar!(1), 10..11)],
        body: Box::new((
            TypedExpr {
                kind: TypedExprKind::Variable("n".to_string()),
                ty: tvar!(1),
                range: 15..16,
            },
            15..16,
        )),
        is_constructor: false,
        return_type: (tvar!(1), 15..16),
    };

    // Build call expressions
    let int_call = TypedASTNode::Expr((
        TypedExpr {
            kind: TypedExprKind::Call {
                function: Box::new(TypedExpr {
                    kind: TypedExprKind::Variable("identity".to_string()),
                    ty: Arc::new(Type::Function {
                        params: vec![tvar!(1)],
                        return_type: tvar!(1),
                    }),
                    range: 20..28,
                }),
                args: vec![TypedExpr {
                    kind: TypedExprKind::Int(1),
                    ty: t_int!(),
                    range: 29..30,
                }],
            },
            ty: tvar!(1),
            range: 20..31,
        },
        20..31,
    ));

    let string_call = TypedASTNode::Expr((
        TypedExpr {
            kind: TypedExprKind::Call {
                function: Box::new(TypedExpr {
                    kind: TypedExprKind::Variable("identity".to_string()),
                    ty: Arc::new(Type::Function {
                        params: vec![tvar!(1)],
                        return_type: tvar!(1),
                    }),
                    range: 32..40,
                }),
                args: vec![TypedExpr {
                    kind: TypedExprKind::String("aaa".to_string()),
                    ty: t_string!(),
                    range: 41..46,
                }],
            },
            ty: tvar!(1),
            range: 32..47,
        },
        32..47,
    ));

    // Create AST
    let ast = vec![
        TypedASTNode::Function((identity_function, 4..12)),
        int_call,
        string_call,
    ];

    // Monomorphize AST
    let mono_ast = env.monomorphize_ast(ast);

    // Verify we have 2 specialized functions + 2 call expressions
    assert_eq!(mono_ast.len(), 4);

    // Verify specialized functions
    let mut found_int_fn = false;
    let mut found_string_fn = false;

    for node in &mono_ast {
        if let TypedASTNode::Function((func, _)) = node {
            if func.name.starts_with("identity_") {
                if func.args[0].1 == t_int!() {
                    found_int_fn = true;
                    assert_eq!(func.return_type.0, t_int!());
                } else if func.args[0].1 == t_string!() {
                    found_string_fn = true;
                    assert_eq!(func.return_type.0, t_string!());
                }
            }
        }
    }

    assert!(found_int_fn);
    assert!(found_string_fn);

    // Verify call expressions
    let mut found_int_call = false;
    let mut found_string_call = false;

    for node in &mono_ast {
        if let TypedASTNode::Expr((expr, _)) = node {
            if let TypedExprKind::Call { function, .. } = &expr.kind {
                if let TypedExprKind::Variable(name) = &function.kind {
                    if name.starts_with("identity_") {
                        // Verify function type is concrete
                        if let Type::Function {
                            params,
                            return_type,
                        } = &*function.ty
                        {
                            assert!(!params.iter().any(|t| matches!(**t, Type::Variable(_))));
                            assert!(!matches!(**return_type, Type::Variable(_)));

                            if params[0] == t_int!() {
                                found_int_call = true;
                                assert_eq!(**return_type, *t_int!());
                            } else if params[0] == t_string!() {
                                found_string_call = true;
                                assert_eq!(**return_type, *t_string!());
                            }
                        }
                    }
                }
            }
        }
    }

    assert!(found_int_call);
    assert!(found_string_call);
}
