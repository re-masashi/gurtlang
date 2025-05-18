use gurtlang::lexer::Token;
use gurtlang::parser::Parser;
use logos::Logos;

use std::fs;

fn main() {
    let filepath = "examples/4.gurt".to_string();

    let contents =
        fs::read_to_string(&filepath).expect("Should have been able to read the file :/");

    let lexer = Token::lexer(&contents).spanned().peekable();
    let mut parser = Parser::new(lexer, filepath);

    println!("{:#?}", parser.parse_program());

    parser.report_errors();

    println!("Me: Yogurt");
    println!("Gurt: Yo");
}
