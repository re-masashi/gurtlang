use ariadne::ColorGenerator;
use ariadne::Label;
use ariadne::Report;
use ariadne::ReportKind;
use ariadne::Source;

use gurtlang::lexer::Token;
use gurtlang::parser::Parser;
use gurtlang::typechecker::TypeEnv;
use gurtlang::validation;

use std::ops::Range;

use logos::Logos;

use std::fs;

fn main() {
    let filepath = "examples/3.gurt".to_string();

    let contents =
        fs::read_to_string(&filepath).expect("Should have been able to read the file :/");

    let mut lexer = Token::lexer(&contents).spanned();
    let mut has_lex_errors = false;
    while let Some((token, _)) = lexer.next() {
        if token.is_err() {
            has_lex_errors = true;
            let span = lexer.span();
            println!(
                "Lexer error at {}:{}: Invalid token '{}'",
                filepath,
                span.start,
                &contents[span.clone()]
            );
        }
    }

    if has_lex_errors {
        panic!("Cannot continue due to lexer errors");
    }

    let lexer = Token::lexer(&contents).spanned().peekable();
    let mut parser = Parser::new(lexer, filepath.clone());

    let ast = parser.parse_program();

    if parser.report_errors() {
        panic!("cant continue");
    };

    let mut type_env = TypeEnv::new(filepath.clone());
    let typed_ast = type_env.ast_to_typed_ast(ast);

    if type_env.report_errors() {
        panic!("cant continue. type errors");
    };

    let resolved_ast = type_env.resolve_all(typed_ast);
    let mono_ast = type_env.monomorphize_ast(resolved_ast);

    println!("{:#?}", mono_ast);

    let validation_errors = validation::validate_ast(mono_ast, filepath.clone());
    for (_, span, _error) in validation_errors {
        let error: Report<'_, (String, Range<usize>)> =
            Report::build(ReportKind::Error, (filepath.clone(), span.clone()))
                .with_code("Unresolved type")
                .with_label(
                    Label::new((filepath.clone(), span.clone()))
                        .with_message(
                            "type signature of this could expression could not be inferred.",
                        )
                        .with_color(ColorGenerator::new().next()),
                )
                .with_note("please specify a type or remove or comment this expression.")
                .with_message("Cannot proceed with typevars.")
                .finish();

        let source = Source::from(contents.clone());

        error.print((filepath.clone(), source.clone())).unwrap();
    }

    println!("Me: Yogurt");
    println!("Gurt: Yo");
}
