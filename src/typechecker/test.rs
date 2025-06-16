use super::*;
use crate::ast::ASTNode;
use crate::ast::TypedASTNode;
use crate::ast::{BinOp, Expr, Type, TypedExpr, TypedExprKind};
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
