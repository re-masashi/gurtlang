// Assuming these are in a file like `src/type_checker.rs`
// and your AST definitions are in `src/ast.rs`

use crate::ast::AssignOp;
use crate::ast::TypedEnum;
use crate::ast::TypedEnumVariant;
use crate::ast::TypedEnumVariantKind;
use crate::ast::TypedFunction;
use crate::ast::TypedStruct;
use crate::ast::UnOp;
use crate::validation::no_typevars::*;
use std::collections::HashSet;
use std::ops::Range;

use super::*;
use crate::ast::BinOp;
use crate::ast::Type;
use crate::ast::TypedExpr;
use crate::ast::TypedExprKind;
use std::sync::Arc; // Import everything from the current module and parent module (ast)

// Helper function to create a dummy range
fn r() -> Range<usize> {
    0..0
}

// Helper function to create an Arc<Type::Variable>
fn type_var(index: usize) -> Arc<Type> {
    Arc::new(Type::Variable(index))
}

// Helper function to create an Arc<Type::Constructor>
fn type_constructor(name: &str, generics: Vec<Arc<Type>>) -> Arc<Type> {
    Arc::new(Type::Constructor {
        name: name.to_string(),
        generics,
        traits: vec![], // Dummy traits for testing
    })
}

// Helper function to create a TypedExpr
fn typed_expr(kind: TypedExprKind, ty: Arc<Type>) -> TypedExpr {
    TypedExpr {
        kind,
        ty,
        range: r(),
    }
}

// Helper for TypedExprKind::Call
fn typed_call(func: TypedExpr, args: Vec<TypedExpr>, ty: Arc<Type>) -> TypedExpr {
    typed_expr(
        TypedExprKind::Call {
            function: Box::new(func),
            args,
        },
        ty,
    )
}

// Helper for TypedExprKind::BinOp
fn typed_bin_op(op: BinOp, l: TypedExpr, r: TypedExpr, ty: Arc<Type>) -> TypedExpr {
    typed_expr(
        TypedExprKind::BinOp {
            operator: op,
            l_value: Box::new(l),
            r_value: Box::new(r),
        },
        ty,
    )
}

// Helper for TypedExprKind::IfElse
fn typed_if_else(
    cond: TypedExpr,
    if_b: TypedExpr,
    else_b: Option<TypedExpr>,
    ty: Arc<Type>,
) -> TypedExpr {
    typed_expr(
        TypedExprKind::IfElse {
            condition: Box::new(cond),
            if_branch: Box::new(if_b),
            else_branch: else_b.map(Box::new),
        },
        ty,
    )
}

// Helper for TypedExprKind::Array
fn typed_array(elements: Vec<TypedExpr>, ty: Arc<Type>) -> TypedExpr {
    typed_expr(TypedExprKind::Array { elements }, ty)
}

// Helper for TypedExprKind::Tuple
fn typed_tuple(elements: Vec<TypedExpr>, ty: Arc<Type>) -> TypedExpr {
    typed_expr(TypedExprKind::Tuple(elements), ty)
}

// Helper for TypedExprKind::Lambda
fn typed_lambda(
    args: Vec<(String, Arc<Type>, Range<usize>)>,
    expression: TypedExpr,
    ty: Arc<Type>,
) -> TypedExpr {
    typed_expr(
        TypedExprKind::Lambda {
            args,
            expression: Box::new(expression),
        },
        ty,
    )
}

use crate::ast::TypedMatchArm;
use crate::ast::TypedPattern;

// Test `check_for_type_vars` with TypedASTNode::Error
#[test]
fn test_check_for_type_vars_error_node() {
    let ast = vec![(TypedASTNode::Error)];
    let errors = check_for_type_vars(&ast, "test.rs");
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].2, "Error node found in AST");
}

// Test `check_type` for all Type variants
#[test]
fn test_check_type_variants() {
    let mut reported = HashSet::new();
    let mut errors;

    // Type::Constructor with generics (recursive call)
    errors = check_type(
        &type_constructor("Vec", vec![type_var(1)]),
        r(),
        &mut reported,
    );
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].1, "unresolved type variable");
    assert!(reported.contains(&(1, r()))); // Check if reported

    // Type::Variable (new)
    reported.clear();
    errors = check_type(&type_var(2), r(), &mut reported);
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].1, "unresolved type variable");
    assert!(reported.contains(&(2, r())));

    // Type::Variable (already reported) - should not add new error
    errors = check_type(&type_var(2), r(), &mut reported);
    assert_eq!(errors.len(), 0); // No new error, as it was already reported

    // Type::Trait
    errors = check_type(
        &Arc::new(Type::Trait(vec!["Send".to_string()])),
        r(),
        &mut reported,
    );
    assert_eq!(errors.len(), 0);

    // Type::Function (recursive calls for params and return type)
    errors = check_type(
        &Arc::new(Type::Function {
            params: vec![type_var(3), type_constructor("String", vec![])],
            return_type: type_var(4),
        }),
        r(),
        &mut reported,
    );
    assert_eq!(errors.len(), 2); // For type_var(3) and type_var(4)
    assert!(reported.contains(&(3, r())));
    assert!(reported.contains(&(4, r())));

    // Type::Tuple (recursive calls for elements)
    errors = check_type(
        &Arc::new(Type::Tuple(vec![
            type_var(5),
            type_constructor("bool", vec![]),
        ])),
        r(),
        &mut reported,
    );
    assert_eq!(errors.len(), 1); // For type_var(5)
    assert!(reported.contains(&(5, r())));

    // Type::Union (recursive calls for types)
    errors = check_type(
        &Arc::new(Type::Union(vec![
            type_var(6),
            type_constructor("int", vec![]),
        ])),
        r(),
        &mut reported,
    );
    assert_eq!(errors.len(), 1); // For type_var(6)
    assert!(reported.contains(&(6, r())));

    // Type::Never
    errors = check_type(&Arc::new(Type::Never), r(), &mut reported);
    assert_eq!(errors.len(), 0);

    // Type::Unit
    errors = check_type(&Arc::new(Type::Unit), r(), &mut reported);
    assert_eq!(errors.len(), 0);
}

// Test `check_typed_expr` for all TypedExprKind variants
#[test]
fn test_check_typed_expr_variants() {
    let mut reported = HashSet::new();
    let mut errors;
    let dummy_int_type = type_constructor("int", vec![]);
    let dummy_bool_type = type_constructor("bool", vec![]);

    // TypedExprKind::Bool
    errors = check_typed_expr(
        &typed_expr(TypedExprKind::Bool(true), dummy_bool_type.clone()),
        &mut reported,
    );
    assert_eq!(errors.len(), 0);

    // TypedExprKind::Int
    errors = check_typed_expr(
        &typed_expr(TypedExprKind::Int(10), dummy_int_type.clone()),
        &mut reported,
    );
    assert_eq!(errors.len(), 0);

    // TypedExprKind::Float
    errors = check_typed_expr(
        &typed_expr(
            TypedExprKind::Float(10.0),
            type_constructor("float", vec![]),
        ),
        &mut reported,
    );
    assert_eq!(errors.len(), 0);

    // TypedExprKind::String
    errors = check_typed_expr(
        &typed_expr(
            TypedExprKind::String("hello".to_string()),
            type_constructor("String", vec![]),
        ),
        &mut reported,
    );
    assert_eq!(errors.len(), 0);

    // TypedExprKind::Variable
    errors = check_typed_expr(
        &typed_expr(
            TypedExprKind::Variable("x".to_string()),
            dummy_int_type.clone(),
        ),
        &mut reported,
    );
    assert_eq!(errors.len(), 0);

    // TypedExprKind::Return
    errors = check_typed_expr(
        &typed_expr(
            TypedExprKind::Return(Box::new(typed_expr(
                TypedExprKind::Int(5),
                dummy_int_type.clone(),
            ))),
            dummy_int_type.clone(),
        ),
        &mut reported,
    );
    assert_eq!(errors.len(), 0);

    // TypedExprKind::Lambda
    errors = check_typed_expr(
        &typed_lambda(
            vec![("a".to_string(), type_var(10), r())], // Arg with type var
            typed_expr(TypedExprKind::Int(1), dummy_int_type.clone()),
            type_constructor("Function", vec![type_var(10), dummy_int_type.clone()]), // Lambda type with type var
        ),
        &mut reported,
    );

    assert_eq!(errors.len(), 1);
    assert!(reported.contains(&(10, r())));

    // TypedExprKind::Array
    errors = check_typed_expr(
        &typed_array(
            vec![
                typed_expr(TypedExprKind::Int(1), dummy_int_type.clone()),
                typed_expr(TypedExprKind::Int(2), type_var(11)),
            ], // Element with type var
            type_constructor("List", vec![type_var(11)]), // Array type with type var
        ),
        &mut reported,
    );
    assert_eq!(errors.len(), 1); // For type_var(11)
    assert!(reported.contains(&(11, r())));

    // TypedExprKind::Index
    errors = check_typed_expr(
        &typed_expr(
            TypedExprKind::Index {
                array: Box::new(typed_array(
                    vec![],
                    type_constructor("List", vec![type_var(12)]),
                )), // Array type with type var
                index: Box::new(typed_expr(TypedExprKind::Int(0), dummy_int_type.clone())),
            },
            type_var(12), // Result type with type var
        ),
        &mut reported,
    );

    assert_eq!(errors.len(), 1); // For type_var(12)
    assert!(reported.contains(&(12, r())));

    // TypedExprKind::Call
    errors = check_typed_expr(
        &typed_call(
            typed_expr(
                TypedExprKind::Variable("f".to_string()),
                type_constructor("Function", vec![type_var(13), type_var(14)]),
            ), // Function type with type vars
            vec![typed_expr(TypedExprKind::Int(1), type_var(13))], // Arg with type var
            type_var(14),                                          // Return type with type var
        ),
        &mut reported,
    );
    assert_eq!(errors.len(), 2); // For type_var(13) and type_var(14)
    assert!(reported.contains(&(13, r())));
    assert!(reported.contains(&(14, r())));

    // TypedExprKind::StructAccess
    errors = check_typed_expr(
        &typed_expr(
            TypedExprKind::StructAccess {
                struct_val: Box::new(typed_expr(
                    TypedExprKind::Variable("s".to_string()),
                    type_constructor("MyStruct", vec![type_var(15)]),
                )), // Struct type with type var
                field_name: "field".to_string(),
            },
            type_var(15), // Field type with type var
        ),
        &mut reported,
    );
    assert_eq!(errors.len(), 1); // For type_var(15)
    assert!(reported.contains(&(15, r())));

    // TypedExprKind::MethodCall
    errors = check_typed_expr(
        &typed_expr(
            TypedExprKind::MethodCall {
                struct_val: Box::new(typed_expr(
                    TypedExprKind::Variable("s".to_string()),
                    type_constructor("MyStruct", vec![type_var(16)]),
                )), // Struct type with type var
                method_name: "method".to_string(),
                args: vec![typed_expr(TypedExprKind::Int(1), type_var(17))], // Arg with type var
            },
            type_var(18), // Return type with type var
        ),
        &mut reported,
    );
    assert_eq!(errors.len(), 3); // For type_var(16), (17), (18)
    assert!(reported.contains(&(16, r())));
    assert!(reported.contains(&(17, r())));
    assert!(reported.contains(&(18, r())));

    // TypedExprKind::BinOp
    errors = check_typed_expr(
        &typed_bin_op(
            BinOp::Add,
            typed_expr(TypedExprKind::Int(1), type_var(19)), // l_value with type var
            typed_expr(TypedExprKind::Int(2), dummy_int_type.clone()),
            type_var(19), // Result type with type var
        ),
        &mut reported,
    );
    assert_eq!(errors.len(), 1); // For type_var(19)
    assert!(reported.contains(&(19, r())));

    // TypedExprKind::Assign
    errors = check_typed_expr(
        &typed_expr(
            TypedExprKind::Assign {
                l_value: Box::new(typed_expr(
                    TypedExprKind::Variable("x".to_string()),
                    type_var(20),
                )), // l_value with type var
                r_value: Box::new(typed_expr(TypedExprKind::Int(5), type_var(20))), // r_value with type var
                assign_op: AssignOp::Assign,
            },
            type_constructor("Unit", vec![]), // Assign usually returns Unit
        ),
        &mut reported,
    );
    assert_eq!(errors.len(), 1); // For type_var(20)
    assert!(reported.contains(&(20, r())));

    // TypedExprKind::UnOp
    errors = check_typed_expr(
        &typed_expr(
            TypedExprKind::UnOp {
                unop: UnOp::Minus,
                expression: Box::new(typed_expr(TypedExprKind::Int(5), type_var(21))), // inner expr with type var
            },
            type_var(21), // Result type with type var
        ),
        &mut reported,
    );
    assert_eq!(errors.len(), 1); // For type_var(21)
    assert!(reported.contains(&(21, r())));

    // TypedExprKind::Do
    errors = check_typed_expr(
        &typed_expr(
            TypedExprKind::Do {
                expressions: vec![
                    typed_expr(TypedExprKind::Int(1), dummy_int_type.clone()),
                    typed_expr(TypedExprKind::Int(2), type_var(22)), // Expr with type var
                ],
            },
            type_var(22), // Result type with type var
        ),
        &mut reported,
    );
    assert_eq!(errors.len(), 1); // For type_var(22)
    assert!(reported.contains(&(22, r())));

    // TypedExprKind::Let
    errors = check_typed_expr(
        &typed_expr(
            TypedExprKind::Let {
                var: "y".to_string(),
                value: Box::new(typed_expr(TypedExprKind::Int(10), type_var(23))), // Value with type var
            },
            type_constructor("Unit", vec![]), // Let usually returns Unit
        ),
        &mut reported,
    );
    assert_eq!(errors.len(), 1); // For type_var(23)
    assert!(reported.contains(&(23, r())));

    // TypedExprKind::IfElse - with else_branch
    errors = check_typed_expr(
        &typed_if_else(
            typed_expr(TypedExprKind::Bool(true), dummy_bool_type.clone()),
            typed_expr(TypedExprKind::Int(1), type_var(24)), // if_branch with type var
            Some(typed_expr(TypedExprKind::Int(2), type_var(24))), // else_branch with type var
            type_var(24),                                    // Result type with type var
        ),
        &mut reported,
    );
    assert_eq!(errors.len(), 1); // For type_var(24)
    assert!(reported.contains(&(24, r())));

    // TypedExprKind::IfElse - without else_branch
    errors = check_typed_expr(
        &typed_if_else(
            typed_expr(TypedExprKind::Bool(true), dummy_bool_type.clone()),
            typed_expr(TypedExprKind::Int(1), type_var(25)), // if_branch with type var
            None,
            type_constructor("Unit", vec![]), // If-else without else often returns Unit
        ),
        &mut reported,
    );
    assert_eq!(errors.len(), 1); // For type_var(25)
    assert!(reported.contains(&(25, r())));

    // TypedExprKind::Tuple
    errors = check_typed_expr(
        &typed_tuple(
            vec![
                typed_expr(TypedExprKind::Int(1), dummy_int_type.clone()),
                typed_expr(TypedExprKind::Bool(false), type_var(26)),
            ], // Element with type var
            type_constructor("Tuple", vec![dummy_int_type.clone(), type_var(26)]), // Tuple type with type var
        ),
        &mut reported,
    );
    assert_eq!(errors.len(), 1); // For type_var(26)
    assert!(reported.contains(&(26, r())));

    // TypedExprKind::EnumVariant
    errors = check_typed_expr(
        &typed_expr(
            TypedExprKind::EnumVariant {
                enum_name: "MyEnum".to_string(),
                variant_name: "VariantA".to_string(),
                fields: vec![(None, typed_expr(TypedExprKind::Int(1), type_var(27)))], // Field with type var
            },
            type_constructor("MyEnum", vec![type_var(27)]), // Enum type with type var
        ),
        &mut reported,
    );
    assert_eq!(errors.len(), 1); // For type_var(27)
    assert!(reported.contains(&(27, r())));

    // TypedExprKind::Match
    errors = check_typed_expr(
        &typed_expr(
            TypedExprKind::Match {
                expr: Box::new(typed_expr(TypedExprKind::Int(1), type_var(28))), // Match expr with type var
                arms: vec![TypedMatchArm {
                    pattern: TypedPattern::Wildcard,
                    body: Box::new(typed_expr(TypedExprKind::Int(10), type_var(29))), // Arm body with type var
                }],
            },
            type_var(29), // Match result type with type var
        ),
        &mut reported,
    );
    assert_eq!(errors.len(), 2); // For type_var(28), and type_var(29) 
    assert!(reported.contains(&(28, r())));
    assert!(reported.contains(&(29, r())));

    // TypedExprKind::Error
    errors = check_typed_expr(
        &typed_expr(TypedExprKind::Error, type_constructor("Error", vec![])),
        &mut reported,
    );
    assert_eq!(errors.len(), 0);
}

// Test `check_typed_function`
#[test]
fn test_check_typed_function() {
    let mut reported = HashSet::new();
    let func = TypedFunction {
        name: "my_func".to_string(),
        args: vec![
            ("arg1".to_string(), type_var(30), r()), // Arg with type var
            ("arg2".to_string(), type_constructor("int", vec![]), r()),
        ],
        body: Box::new((typed_expr(TypedExprKind::Int(1), type_var(31)), r())), // Body with type var
        return_type: (type_var(32), r()), // Return type with type var
    };
    let errors = check_typed_function(&func, &mut reported);
    assert_eq!(errors.len(), 3); // For type_var(30), (31), (32)
    assert!(reported.contains(&(30, r())));
    assert!(reported.contains(&(31, r())));
    assert!(reported.contains(&(32, r())));
}

// Test `check_typed_struct`
#[test]
fn test_check_typed_struct() {
    let mut reported = HashSet::new();
    let strct = TypedStruct {
        name: "MyStruct".to_string(),
        generics: vec![],
        fields: vec![
            ("field1".to_string(), type_var(33), r()), // Field with type var
            ("field2".to_string(), type_constructor("bool", vec![]), r()),
        ],
    };
    let errors = check_typed_struct(&strct, &mut reported);
    assert_eq!(errors.len(), 1); // For type_var(33)
    assert!(reported.contains(&(33, r())));
}

// Test `check_typed_enum` and `check_typed_enum_variant`
#[test]
fn test_check_typed_enum_variants() {
    let mut reported = HashSet::new();
    let mut errors;

    // TypedEnumVariantKind::Unit
    let unit_variant = TypedEnumVariant {
        name: "UnitVariant".to_string(),
        kind: TypedEnumVariantKind::Unit,
        range: r(),
    };
    errors = check_typed_enum_variant(&unit_variant, &mut reported);
    assert_eq!(errors.len(), 0);

    // TypedEnumVariantKind::Tuple
    let tuple_variant = TypedEnumVariant {
        name: "TupleVariant".to_string(),
        kind: TypedEnumVariantKind::Tuple(vec![
            type_var(34), // Tuple element with type var
            type_constructor("String", vec![]),
        ]),
        range: r(),
    };
    errors = check_typed_enum_variant(&tuple_variant, &mut reported);
    assert_eq!(errors.len(), 1); // For type_var(34)
    assert!(reported.contains(&(34, r())));

    // TypedEnumVariantKind::Struct
    let struct_variant = TypedEnumVariant {
        name: "StructVariant".to_string(),
        kind: TypedEnumVariantKind::Struct(vec![
            ("x".to_string(), type_var(35)), // Struct field with type var
            ("y".to_string(), type_constructor("float", vec![])),
        ]),
        range: r(),
    };
    errors = check_typed_enum_variant(&struct_variant, &mut reported);
    assert_eq!(errors.len(), 1); // For type_var(35)
    assert!(reported.contains(&(35, r())));

    // Test `check_typed_enum` (which calls check_typed_enum_variant)
    let enm = TypedEnum {
        name: "MyEnum".to_string(),
        generics: vec![],
        variants: vec![unit_variant, tuple_variant, struct_variant],
    };
    // Clear reported set for a fresh run for `check_typed_enum`
    reported.clear();
    errors = check_typed_enum(&enm, &mut reported);
    // Expect errors from type_var(34) and type_var(35)
    assert_eq!(errors.len(), 2);
    assert!(reported.contains(&(34, r())));
    assert!(reported.contains(&(35, r())));
}

// Comprehensive test for `check_for_type_vars` covering multiple node types
#[test]
fn test_check_for_type_vars_comprehensive() {
    let file_path = "complex_test.rs";

    // Create a function with an unresolved type variable
    let func_with_type_var = TypedFunction {
        name: "generic_id".to_string(),
        args: vec![("x".to_string(), type_var(100), r())],
        body: Box::new((
            typed_expr(TypedExprKind::Variable("x".to_string()), type_var(100)),
            r(),
        )),
        return_type: (type_var(100), r()),
    };

    // Create an expression with an unresolved type variable
    let expr_with_type_var = typed_call(
        typed_expr(
            TypedExprKind::Variable("f".to_string()),
            type_constructor("Function", vec![type_var(101), type_var(102)]),
        ),
        vec![typed_expr(TypedExprKind::Int(1), type_var(101))],
        type_var(102),
    );

    // Create a struct with an unresolved type variable
    let struct_with_type_var = TypedStruct {
        name: "GenericData".to_string(),
        generics: vec![],
        fields: vec![("data".to_string(), type_var(103), r())],
    };

    // Create an enum with an unresolved type variable in a variant
    let enum_with_type_var = TypedEnum {
        name: "Result".to_string(),
        generics: vec![],
        variants: vec![
            TypedEnumVariant {
                name: "Ok".to_string(),
                kind: TypedEnumVariantKind::Tuple(vec![type_var(104)]),
                range: r(),
            },
            TypedEnumVariant {
                name: "Err".to_string(),
                kind: TypedEnumVariantKind::Unit,
                range: r(),
            },
        ],
    };

    let ast = vec![
        (TypedASTNode::Function((func_with_type_var, r()))),
        (TypedASTNode::Expr((expr_with_type_var, r()))),
        (TypedASTNode::Struct((struct_with_type_var, r()))),
        (TypedASTNode::Enum((enum_with_type_var, r()))),
        (TypedASTNode::Error), // To cover the error node case
    ];

    let errors = check_for_type_vars(&ast, file_path);

    // Expected errors:
    // 100 (from func_with_type_var: arg, body, return) - 3 errors
    // 101 (from expr_with_type_var: func type param, arg type) - 2 errors
    // 102 (from expr_with_type_var: func type return, expr type) - 2 errors
    // 103 (from struct_with_type_var: field type) - 1 error
    // 104 (from enum_with_type_var: variant tuple type) - 1 error
    // Error node - 1 error
    // Total unique type variables: 100, 101, 102, 103, 104 (5 unique indices)
    // Each of these will be reported once.
    // The `reported` HashSet prevents duplicate errors for the same (index, span) pair.
    // So, we expect 5 errors from type variables + 1 from the error node.

    assert_eq!(errors.len(), 6); // 5 unique type vars + 1 error node

    let error_messages: HashSet<String> = errors.iter().map(|e| e.2.clone()).collect();
    assert!(error_messages.contains("unresolved type variable"));
    assert!(error_messages.contains("Error node found in AST"));

    // Verify file path is correctly added
    for error in &errors {
        assert_eq!(error.0, file_path);
    }
}
