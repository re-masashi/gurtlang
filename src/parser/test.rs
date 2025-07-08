use super::*;
use crate::ast::{AssignOp, BinOp, Expr, Extern, Function, Struct, TypeAnnot, UnOp};
use logos::Logos;

// Helper function to parse input without file I/O
fn parse_str(input: &str) -> (Vec<(ASTNode, Range<usize>)>, Parser) {
    // let tokens = Token::lexer(input).spanned().peekable();

    let mut lexer = Token::lexer(input).spanned();
    let mut has_lex_errors = false;
    while let Some((token, _)) = lexer.next() {
        if token.is_err() {
            has_lex_errors = true;
            let span = lexer.span();
            println!(
                "Lexer error at {}: Invalid token '{}'",
                // filepath,
                span.start,
                &input[span.clone()]
            );
        }
    }

    if has_lex_errors {
        panic!("Cannot continue due to lexer errors");
    }

    let lexer = Token::lexer(&input).spanned().peekable();

    let mut parser = Parser::new(lexer, "test".to_string());
    let ast = parser.parse_program();
    (ast, parser)
}

#[test]
fn test_parse_raw_string_basic() {
    let input = r#""""hello world""""#;
    let (ast, parser) = parse_str(input);
    assert!(parser.errors.is_empty());
    // println!("{:?}", ast);
    assert_eq!(ast.len(), 1);
    if let (ASTNode::Expr((Expr::String(s), _)), _) = &ast[0] {
        assert_eq!(s, "hello world");
    } else {
        panic!("Expected String expression for raw string");
    }
}

#[test]
fn test_parse_type_alias_missing_assign() {
    let input = "type MyAlias int"; // Missing '='
    let (ast, _parser) = parse_str(input);
    assert!(matches!(&ast[0].0, ASTNode::Error));
}

#[test]
fn test_parse_enum_eof_before_end() {
    let input = "enum MyEnum Variant"; // Missing 'end' keyword
    let (ast, _parser) = parse_str(input);
    assert!(matches!(&ast[0].0, ASTNode::Enum(..)));
}

#[test]
fn test_parse_enum_invalid_name() {
    let input = "enum 123 end";
    let (ast, parser) = parse_str(input);
    assert!(!parser.errors.is_empty());
    assert!(matches!(&ast[0].0, ASTNode::Error));
}

#[test]
fn test_parse_struct_field_unimplemented_token() {
    let input = "struct MyStruct 42"; // Unexpected token (Int) inside struct fields
    let (_ast, parser) = parse_str(input);
    assert!(parser.errors.len() == 1);
}

#[test]
fn test_parse_struct_invalid_name() {
    let input = "struct 123 end";
    let (ast, parser) = parse_str(input);
    assert!(!parser.errors.is_empty());
    assert!(matches!(&ast[0].0, ASTNode::Error));
}

#[test]
#[should_panic(expected = "STRUCT NEEDS A NAME")]
fn test_parse_struct_eof_after_keyword() {
    let input = "struct";
    let (ast, parser) = parse_str(input);
    assert!(!parser.errors.is_empty());
    assert!(matches!(&ast[0].0, ASTNode::Error));
}

#[test]
fn test_parse_program_empty_semicolon() {
    let input = ";";
    let (ast, parser) = parse_str(input);
    assert!(parser.errors.is_empty());
    assert!(ast.is_empty()); // Empty semicolon should result in no AST nodes
}

#[test]
fn test_parse_program_type_alias_declaration() {
    let input = "type MyAlias = int";
    let (ast, parser) = parse_str(input);
    assert!(parser.errors.is_empty());
    assert_eq!(ast.len(), 1);
    assert!(matches!(&ast[0].0, ASTNode::TypeAlias(..)));
}

#[test]
#[should_panic]
fn test_parse_program_illegal_token() {
    let input = "$"; // An illegal token
    let (_ast, _parser) = parse_str(input);
}

#[test]
fn test_parse_type_annotation_trait_invalid_name() {
    let input = "def my_func(a: trait 42) body 1 end";
    let (ast, parser) = parse_str(input);
    assert!(!parser.errors.is_empty());
    assert!(matches!(&ast[0].0, ASTNode::Error));
}

#[test]
fn test_parse_type_annotation_invalid_token_after_colon() {
    let input = "def my_func(a: 42)";
    let (ast, parser) = parse_str(input);
    assert!(!parser.errors.is_empty());
    assert!(matches!(&ast[0].0, ASTNode::Error));
}

#[test]
fn test_parse_function_eof_after_name() {
    let input = "def my_func";
    let (ast, parser) = parse_str(input);
    assert!(!parser.errors.is_empty());
    assert!(matches!(&ast[0].0, ASTNode::Error));
}

#[test]
fn test_parse_function_invalid_name() {
    let input = "def 123";
    let (ast, parser) = parse_str(input);
    assert!(!parser.errors.is_empty());
    assert!(matches!(&ast[0].0, ASTNode::Error));
}

#[test]
fn test_parse_function_eof_after_def() {
    let input = "def";
    let (ast, parser) = parse_str(input);
    assert!(!parser.errors.is_empty());
    assert!(matches!(&ast[0].0, ASTNode::Error));
}

#[test]
fn test_function_syntax_string() {
    let syntax = crate::parser::function::function_syntax();
    assert!(syntax.contains("The syntax for declaring a function is:"));
    // assert!(syntax.contains("def function_name(arg1: type1, arg2: type2, ...) -> return_type"));
}

#[test]
fn test_parse_type_annotation_multiple_traits() {
    let input = "def my_func(a: trait MyTrait + AnotherTrait) -> int do 1 end";
    let (ast, parser) = parse_str(input);
    assert!(parser.errors.is_empty());
    // println!("{:?}", parser.errors);
    assert_eq!(ast.len(), 1);
    if let (ASTNode::Function(func), _) = &ast[0] {
        if let Some(TypeAnnot::Trait(traits)) = &func.args[0].1 {
            assert_eq!(
                traits,
                &vec!["MyTrait".to_string(), "AnotherTrait".to_string()]
            );
        } else {
            panic!("Expected Trait annotation");
        }
    } else {
        panic!("Expected Function AST node");
    }
}

#[test]
fn test_parse_tuple_invalid_token_after_first_element() {
    let input = "(1 {";
    let (ast, parser) = parse_str(input);
    assert!(!parser.errors.is_empty());
    assert!(matches!(&ast[0].0, ASTNode::Expr((Expr::Error, _))));
}

#[test]
fn test_parse_tuple_eof_after_first_element() {
    let input = "(1";
    let (ast, parser) = parse_str(input);
    assert!(!parser.errors.is_empty());
    assert!(matches!(&ast[0].0, ASTNode::Expr((Expr::Error, _))));
}

#[test]
#[should_panic]
fn test_parse_enum_variant_invalid_token_after_access() {
    let input = "MyEnum::42"; // 42 is an Int token, not a Variable
    let (_ast, _parser) = parse_str(input);
}

#[test]
fn test_parse_empty_array() {
    let input = "[]";
    let (ast, parser) = parse_str(input);
    assert!(parser.errors.is_empty());
    assert_eq!(ast.len(), 1);
    if let (ASTNode::Expr((Expr::Array { elements }, _)), _) = &ast[0] {
        assert!(elements.is_empty());
    } else {
        panic!("Expected Array expression for empty array");
    }
}

#[test]
fn test_parse_array_missing_comma_or_rbracket() {
    let input = "[1 2]"; // missing comma
    let (_ast, parser) = parse_str(input);
    assert!(!parser.errors.is_empty());
}

#[test]
fn test_parse_array_eof_after_comma() {
    let input = "[1,";
    let (ast, parser) = parse_str(input);
    assert!(!parser.errors.is_empty());
    assert!(matches!(&ast[0].0, ASTNode::Expr((Expr::Error, _))));
}

#[test]
#[should_panic(expected = "EOF ERROR IN STRUCT GENERICS")]
fn test_parse_struct_generics_eof_after_less() {
    let input = "struct MyStruct<";
    let (_ast, _parser) = parse_str(input);
}

#[test]
#[should_panic(expected = "EOF ERROR IN STRUCT GENERICS")]
fn test_parse_struct_generics_eof_after_generic() {
    let input = "struct MyStruct<T,";
    let (_ast, _parser) = parse_str(input);
}

#[test]
fn test_parse_basic_expressions() {
    let (ast, parser) = parse_str("5 true 3.14 \"hello\"");
    assert!(parser.errors.is_empty());
    assert_eq!(ast.len(), 4);

    if let ASTNode::Expr((Expr::Int(5), _)) = ast[0].0 {}
    if let ASTNode::Expr((Expr::Bool(true), _)) = ast[1].0 {}
    if let ASTNode::Expr((Expr::Float(3.14), _)) = ast[2].0 {}
    if let ASTNode::Expr((Expr::String(s), _)) = &ast[3].0 {
        assert_eq!(s, "hello");
    }
}

#[test]
fn test_parse_variable() {
    let (ast, parser) = parse_str("my_var");
    assert!(parser.errors.is_empty());

    if let ASTNode::Expr((Expr::Variable(name), _)) = &ast[0].0 {
        assert_eq!(name, "my_var");
    }
}

#[test]
fn test_parse_array() {
    let (ast, parser) = parse_str("[1, 2, 3]");
    // println!("{:#?}", ast);
    // println!("{:#?}", parser.errors);

    assert!(parser.errors.is_empty());

    if let ASTNode::Expr((Expr::Array { elements }, _)) = &ast[0].0 {
        assert_eq!(elements.len(), 3);
        if let Expr::Int(1) = elements[0].0 {}
        if let Expr::Int(2) = elements[1].0 {}
        if let Expr::Int(3) = elements[2].0 {}
    }
}

#[test]
fn test_parse_index() {
    let (ast, parser) = parse_str("arr[5]");
    assert!(parser.errors.is_empty());

    if let ASTNode::Expr((Expr::Index { array, index }, _)) = &ast[0].0 {
        if let Expr::Variable(name) = &array.0 {
            assert_eq!(name, "arr");
        }
        if let Expr::Int(5) = index.0 {}
    }
}

#[test]
fn test_parse_function_call() {
    let (ast, parser) = parse_str("foo(1, 2, 3)");
    // println!("{:#?}", ast);
    // println!("{:#?}", parser.errors);
    assert!(parser.errors.is_empty());

    if let ASTNode::Expr((Expr::Call { function, args }, _)) = &ast[0].0 {
        if let Expr::Variable(name) = &function.0 {
            assert_eq!(name, "foo");
        }

        assert_eq!(args.len(), 3);
        assert!(matches!(args[0].0, Expr::Int(1)));
        assert_eq!(args[0].1, 4..5);
        assert!(matches!(args[1].0, Expr::Int(2)));
        assert_eq!(args[1].1, 7..8);
        assert!(matches!(args[2].0, Expr::Int(3)));
        assert_eq!(args[2].1, 10..11);
    } else {
        panic!("Expected function call");
    }
}

#[test]
fn test_parse_struct_access() {
    let (ast, parser) = parse_str("point.x");
    assert!(parser.errors.is_empty());

    if let ASTNode::Expr((
        Expr::StructAccess {
            struct_val,
            field_name,
        },
        _,
    )) = &ast[0].0
    {
        if let Expr::Variable(name) = &struct_val.0 {
            assert_eq!(name, "point");
        }
        assert_eq!(field_name, "x");
    }
}

#[test]
fn test_parse_method_call() {
    let (ast, parser) = parse_str("vec.push(42, 32, 32)");
    assert!(parser.errors.is_empty());

    if let ASTNode::Expr((
        Expr::MethodCall {
            struct_val,
            method_name,
            args,
        },
        _,
    )) = &ast[0].0
    {
        if let Expr::Variable(name) = &struct_val.0 {
            assert_eq!(name, "vec");
        }
        assert_eq!(method_name, "push");
        assert_eq!(args.len(), 3);
        assert!(matches!(args[0].0, Expr::Int(42)));
        assert!(matches!(args[1].0, Expr::Int(32)));
        assert!(matches!(args[2].0, Expr::Int(32)))
    }
}

#[test]
fn test_parse_binops() {
    let (ast, parser) = parse_str("1 + 2 * 3 4 == 5 true and false");
    assert!(parser.errors.is_empty());
    assert_eq!(ast.len(), 3);

    if let ASTNode::Expr((
        Expr::BinOp {
            operator,
            l_value,
            r_value,
        },
        _,
    )) = &ast[0].0
    {
        assert!(matches!(operator, BinOp::Add));
        if let Expr::Int(1) = &l_value.0 {}
        if let Expr::BinOp {
            operator: inner_op, ..
        } = &r_value.0
        {
            assert!(matches!(inner_op, BinOp::Mul));
        }
    }

    if let ASTNode::Expr((Expr::BinOp { operator, .. }, _)) = &ast[1].0 {
        assert!(matches!(operator, BinOp::Eq));
    }

    if let ASTNode::Expr((Expr::BinOp { operator, .. }, _)) = &ast[2].0 {
        assert!(matches!(operator, BinOp::And));
    }
}

#[test]
fn test_parse_unops() {
    let (ast, parser) = parse_str("-5");
    assert!(parser.errors.is_empty());
    assert_eq!(ast.len(), 1);

    if let ASTNode::Expr((Expr::UnOp { unop, expression }, _)) = &ast[0].0 {
        assert!(matches!(unop, UnOp::Minus));
        if let Expr::Int(5) = expression.0 {}
    }
}

#[test]
fn test_parse_assignments() {
    let (ast, parser) = parse_str("x = 5 y += 3");
    assert!(parser.errors.is_empty());
    assert_eq!(ast.len(), 2);

    if let ASTNode::Expr((
        Expr::Assign {
            l_value,
            r_value,
            assign_op,
        },
        _,
    )) = &ast[0].0
    {
        assert!(matches!(assign_op, AssignOp::Assign));
        if let Expr::Variable(name) = &l_value.0 {
            assert_eq!(name, "x");
        }
        if let Expr::Int(5) = &r_value.0 {}
    }

    if let ASTNode::Expr((Expr::Assign { assign_op, .. }, _)) = &ast[1].0 {
        assert!(matches!(assign_op, AssignOp::AddAssign));
    }
}

#[test]
fn test_return() {
    let (ast, parser) = parse_str("return return 0");
    assert!(parser.errors.is_empty());
    assert_eq!(ast.len(), 1);

    assert!(matches!(ast[0].0, ASTNode::Expr((Expr::Return(_), _)),))
}

#[test]
fn test_parse_do_block() {
    let (ast, parser) = parse_str("do 1+1 2; 3 end");
    // println!("{:#?}", parser.errors);
    assert!(parser.errors.is_empty());

    if let ASTNode::Expr((Expr::Do { expressions }, _)) = &ast[0].0 {
        assert_eq!(expressions.len(), 3);
    }
}

#[test]
fn test_parse_let_statement() {
    // NO NEED TO PUT '=' IF YOU ARE USING A TYPE ANNOTATION. THIS IS A FEATURE NOW
    let (ast, parser) = parse_str("let x: int 5 let y = 10");
    // println!("{:#?}", ast);
    assert!(parser.errors.is_empty());
    assert_eq!(ast.len(), 2);

    if let ASTNode::Expr((
        Expr::Let {
            var,
            type_annot,
            value,
        },
        _,
    )) = &ast[0].0
    {
        assert_eq!(var, "x");
        if let Some((TypeAnnot::Int, _)) = type_annot {}
        if let Expr::Int(5) = &value.0 {}
    }

    if let ASTNode::Expr((
        Expr::Let {
            var, type_annot, ..
        },
        _,
    )) = &ast[1].0
    {
        assert_eq!(var, "y");
        assert!(type_annot.is_none());
    }
}

#[test]
fn test_parse_if_else() {
    let (ast, parser) = parse_str("if x then 1 else 2 if y then 3");
    assert!(parser.errors.is_empty());
    assert_eq!(ast.len(), 2);

    if let ASTNode::Expr((
        Expr::IfElse {
            condition,
            if_branch,
            else_branch,
        },
        _,
    )) = &ast[0].0
    {
        if let Expr::Variable(name) = &condition.0 {
            assert_eq!(name, "x");
        }
        if let Expr::Int(1) = if_branch.0 {}
        assert!(else_branch.is_some());
    }

    if let ASTNode::Expr((Expr::IfElse { else_branch, .. }, _)) = &ast[1].0 {
        assert!(else_branch.is_none());
    }
}

#[test]
fn test_parse_tuple() {
    let (ast, parser) = parse_str("(1, true, \"three\");");
    println!("{:?}", parser.errors);
    assert!(parser.errors.is_empty());

    if let ASTNode::Expr((Expr::Tuple(elements), _)) = &ast[0].0 {
        assert_eq!(elements.len(), 3);
        if let Expr::Int(1) = elements[0].0 {}
        if let Expr::Bool(true) = elements[1].0 {}
        if let Expr::String(s) = &elements[2].0 {
            assert_eq!(s, "three");
        }
    }
}

#[test]
fn test_parse_function() {
    let (ast, parser) = parse_str("def add(a: int, b: int) -> int a + b ");
    assert!(parser.errors.is_empty());

    if let ASTNode::Function(Function {
        name,
        args,
        return_type,
        ..
    }) = &ast[0].0
    {
        assert_eq!(name, "add");
        assert_eq!(args.len(), 2);
        assert_eq!(args[0].0, "a");
        if let Some(TypeAnnot::Int) = args[0].1 {}
        assert_eq!(args[1].0, "b");
        if let Some(TypeAnnot::Int) = args[1].1 {}
        if let Some((TypeAnnot::Int, _)) = return_type {}
    }
}

#[test]
fn test_parse_extern() {
    let (ast, parser) = parse_str("extern puts(string) -> int");
    println!("{:?}", parser.errors);
    assert!(parser.errors.is_empty());

    if let ASTNode::Extern(Extern {
        name,
        args,
        return_type,
        ..
    }) = &ast[0].0
    {
        assert_eq!(name, "puts");
        assert_eq!(args.len(), 1);
        if let TypeAnnot::Int = args[0].0 {}
        if let (TypeAnnot::Int, _) = return_type {}
    }
}

#[test]
fn test_parse_generic_struct() {
    let (ast, parser) = parse_str(
        "
            struct Box<T> 
                val T
            end  
            struct Pair<A, B>
                first A 
                second B 
            end
        ",
    );
    assert!(parser.errors.is_empty());
    assert_eq!(ast.len(), 2);

    if let ASTNode::Struct(Struct {
        name,
        generics,
        fields,
    }) = &ast[0].0
    {
        assert_eq!(name.0, "Box");
        assert_eq!(generics.len(), 1);
        assert_eq!(generics[0].0, "T");
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].0, "val");
        if let TypeAnnot::Boring(t) = &fields[0].1 {
            assert_eq!(t, "T");
        }
    }

    if let ASTNode::Struct(Struct {
        name,
        generics,
        fields,
    }) = &ast[1].0
    {
        assert_eq!(name.0, "Pair");
        assert_eq!(generics.len(), 2);
        assert_eq!(fields.len(), 2);
    }
}

#[test]
fn test_parse_complex_type_annotations() {
    let (ast, parser) = parse_str("let a: List<int> 123");
    assert!(parser.errors.is_empty());
    assert_eq!(ast.len(), 1);

    if let ASTNode::Expr((Expr::Let { type_annot, .. }, _)) = &ast[0].0 {
        if let Some((TypeAnnot::Generic(name, generics), _)) = type_annot {
            assert_eq!(name, "List");
            assert_eq!(generics.len(), 1);
            if let TypeAnnot::Int = generics[0] {}
        }
    }
}

#[test]
#[should_panic]
fn test_error_recovery() {
    let (ast, parser) = parse_str("let @ = 5 42 struct 2");
    assert!(parser.errors.is_empty());
    assert_eq!(ast.len(), 3);

    // First node should be error
    if let ASTNode::Error = ast[0].0 {}

    // Next nodes should still parse
    if let ASTNode::Expr((Expr::Int(42), _)) = ast[1].0 {}
    if let ASTNode::Struct { .. } = ast[2].0 {}
}

#[test]
fn test_parse_match() {
    let (ast, parser) = parse_str(
        "
        match abcd 
            A::Pat1(a, _) if a == 2 | _ => print(a),
            A::Pat1(a, _) | _ => print(1),
            (_, b) => println(-1),
            _=>print(0),
        end
    ",
    );
    // println!("{:#?}", parser.errors);

    // println!("{:#?}", ast);
    assert!(parser.errors.is_empty());
    assert_eq!(ast.len(), 1);
}

#[test]
fn test_parse_enum() {
    let (ast, parser) = parse_str(
        "
        enum Fren
            Cat,
            Doggo,
            Human(int),
            Laptop{a: int},
        end
    ",
    );
    // println!("{:#?}", parser.errors);

    // println!("{:#?}", ast);
    assert!(parser.errors.is_empty());
    assert_eq!(ast.len(), 1);
}

#[test]
fn test_type_alias() {
    let (ast, parser) = parse_str(
        "
        type howmanybananas = int
    ",
    );
    // println!("{:#?}", parser.errors);

    // println!("{:#?}", ast);
    assert!(parser.errors.is_empty());
    assert_eq!(ast.len(), 1);
}

#[test]
fn test_strings() {
    let input = r#"
    """helloworl1"""
    "helloworld";
    "#;
    // println!("34th {}", input.chars().nth(34).unwrap());

    // println!("35th {}", input.chars().nth(35).unwrap());
    let (ast, parser) = parse_str(input);
    assert!(parser.errors.is_empty());
    assert_eq!(ast.len(), 2);
}

#[test]
fn test_parse_lambda_basic() {
    let input = "fn(a: int, b) a + b";
    let (ast, _parser) = parse_str(input);

    assert_eq!(ast.len(), 1);

    if let (ASTNode::Expr((Expr::Lambda { args, expression }, _)), _) = &ast[0] {
        assert_eq!(args.len(), 2);

        // First argument
        assert_eq!(args[0].0, "a");
        assert!(matches!(args[0].1, Some(TypeAnnot::Int)));

        // Second argument
        assert_eq!(args[1].0, "b");
        assert!(args[1].1.is_none());

        // Body expression
        let (body, _) = &**expression;
        assert!(matches!(body, Expr::BinOp { .. }));
    } else {
        panic!("Expected Lambda expression");
    }
}

#[test]
fn test_parse_lambda_no_args() {
    let input = "fn() 42";
    let (ast, _parser) = parse_str(input);

    assert_eq!(ast.len(), 1);

    if let (ASTNode::Expr((Expr::Lambda { args, expression }, _)), _) = &ast[0] {
        assert_eq!(args.len(), 0);

        // Body expression
        let (body, _) = &**expression;
        assert!(matches!(body, Expr::Int(42)));
    } else {
        panic!("Expected Lambda expression");
    }
}

#[test]
fn test_parse_lambda_nested() {
    let input = "fn(a) fn(b) a + b";
    let (ast, _parser) = parse_str(input);

    assert_eq!(ast.len(), 1);

    if let (ASTNode::Expr((Expr::Lambda { args, expression }, _)), _) = &ast[0] {
        assert_eq!(args.len(), 1);
        assert_eq!(args[0].0, "a");

        // Inner lambda
        let (body, _) = &**expression;
        assert!(matches!(body, Expr::Lambda { .. }));
    } else {
        panic!("Expected Lambda expression");
    }
}

#[test]
fn test_parse_lambda_complex_body() {
    let input = "fn(x: float) do let y = x * 2.0; y + 1.0 end";
    let (ast, _parser) = parse_str(input);

    assert_eq!(ast.len(), 1);

    if let (ASTNode::Expr((Expr::Lambda { args, expression }, _)), _) = &ast[0] {
        assert_eq!(args.len(), 1);
        assert_eq!(args[0].0, "x");
        assert!(matches!(args[0].1, Some(TypeAnnot::Float)));

        // Body expression
        let (body, _) = &**expression;
        assert!(matches!(body, Expr::Do { .. }));
    } else {
        panic!("Expected Lambda expression");
    }
}
