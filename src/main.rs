use gurtlang::lexer::Token;
use gurtlang::parser::Parser;
use gurtlang::typechecker::TypeEnv;

use logos::Logos;

use std::fs;

fn main() {
    let filepath = "examples/5.gurt".to_string();

    let contents =
        fs::read_to_string(&filepath).expect("Should have been able to read the file :/");

    let lexer = Token::lexer(&contents).spanned().peekable();
    let mut parser = Parser::new(lexer, filepath.clone());

    let ast = parser.parse_program();
    // println!("{:#?}", ast);

    if parser.report_errors() {
        panic!("cant continue");
    };

    let mut type_env = TypeEnv::new(filepath);
    let typed_ast = type_env.ast_to_typed_ast(ast);

    if type_env.report_errors() {
        panic!("cant continue. type errors");
    };

    let resolved_ast = type_env.resolve_all(typed_ast);
    let mono_ast = type_env.monomorphize_ast(resolved_ast);

    println!("{:#?}", mono_ast);

    println!("Me: Yogurt");
    println!("Gurt: Yo");
}
