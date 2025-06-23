use super::*;
use crate::ast::ASTNode;
use crate::ast::Enum;
use crate::ast::EnumVariant;
use crate::ast::Function;
use crate::ast::Struct;
use crate::ast::TypedASTNode;
use crate::ast::{BinOp, Expr, Type, TypedExpr, TypedExprKind};
use crate::t_float;
use crate::t_int;

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

// #[test]
// fn test_type_mismatch_errors() {
//     let program = vec![
//         (
//             ASTNode::Expr((
//                 Expr::Let {
//                     var: "x".to_string(),
//                     type_annot: Some((TypeAnnot::Int, 0..3)),
//                     value: Box::new((Expr::Float(3.14), 6..10)),
//                 },
//                 0..11,
//             )),
//             0..11,
//         ),
//         (
//             ASTNode::Expr((
//                 Expr::BinOp {
//                     operator: BinOp::Add,
//                     l_value: Box::new((Expr::Int(5), 15..16)),
//                     r_value: Box::new((Expr::Bool(true), 17..21)),
//                 },
//                 15..22,
//             )),
//             15..22,
//         ),
//     ];

//     let mut type_env = TypeEnv::new("test".to_string());
//     let _ = type_env.ast_to_typed_ast(program);

//     // Should have 2 errors:
//     // 1. Int vs Float in let binding
//     // 2. Int vs Bool in addition
//     assert_eq!(type_env.errors.len(), 2);
// }

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
    println!("typed_ast {:?}", typed_ast);

    let resolved_ast = type_env.resolve_all(typed_ast);
    println!("resolved_ast {:?}", resolved_ast);
    
    let mono_ast = type_env.monomorphize_ast(resolved_ast);
    println!("mono_ast {:?}", mono_ast);

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
