use super::*;
use logos::Logos;

#[test]
fn test_basic_tokens() {
    let input = "
    let x = 5
    struct Box<T> val: T end
    ";
    let mut lexer = Token::lexer(input);

    assert_eq!(lexer.next(), Some(Ok(Token::KeywordLet)));
    assert_eq!(lexer.next(), Some(Ok(Token::Variable("x".to_string()))));
    assert_eq!(lexer.next(), Some(Ok(Token::Assign)));
    assert_eq!(lexer.next(), Some(Ok(Token::Int(5))));
    assert_eq!(lexer.next(), Some(Ok(Token::KeywordStruct)));
    assert_eq!(lexer.next(), Some(Ok(Token::Variable("Box".to_string()))));
    assert_eq!(lexer.next(), Some(Ok(Token::Less)));
}

#[test]
fn test_string_token() {
    let input = r#""hello\nworld""#;
    let mut lexer = Token::lexer(input);
    assert_eq!(
        lexer.next(),
        Some(Ok(Token::String("hello\nworld".to_string())))
    );
}

#[test]
fn test_error_recovery() {
    let input = "let @ x = 5;";
    let mut lexer = Token::lexer(input);

    assert_eq!(lexer.next(), Some(Ok(Token::KeywordLet)));
    assert!(lexer.next().unwrap().is_err()); // Invalid token '@'
    assert_eq!(lexer.next(), Some(Ok(Token::Variable("x".to_string()))));
}


#[test]
fn test_raw_string_token() {
    let input = r#" """hello\nworld""" "#;
    let mut lexer = Token::lexer(input);
    assert_eq!(
        lexer.next(),
        Some(Ok(Token::RawString("hello\nworld".to_string())))
    );
}

#[test]
fn test_numeric_literals() {
    let input = "123 45.67 0.1 true false";
    let mut lexer = Token::lexer(input);

    assert_eq!(lexer.next(), Some(Ok(Token::Int(123))));
    assert_eq!(lexer.next(), Some(Ok(Token::Float(45.67))));
    assert_eq!(lexer.next(), Some(Ok(Token::Float(0.1))));
    assert_eq!(lexer.next(), Some(Ok(Token::Bool(true))));
    assert_eq!(lexer.next(), Some(Ok(Token::Bool(false))));
}

#[test]
fn test_operators() {
    let input = "+ - * / % ** == != < > <= >= and or xor nor not";
    let mut lexer = Token::lexer(input);

    assert_eq!(lexer.next(), Some(Ok(Token::Plus)));
    assert_eq!(lexer.next(), Some(Ok(Token::Minus)));
    assert_eq!(lexer.next(), Some(Ok(Token::Mul)));
    assert_eq!(lexer.next(), Some(Ok(Token::Div)));
    assert_eq!(lexer.next(), Some(Ok(Token::Mod)));
    assert_eq!(lexer.next(), Some(Ok(Token::Power)));
    assert_eq!(lexer.next(), Some(Ok(Token::Eq)));
    assert_eq!(lexer.next(), Some(Ok(Token::NotEq)));
    assert_eq!(lexer.next(), Some(Ok(Token::Less)));
    assert_eq!(lexer.next(), Some(Ok(Token::Greater)));
    assert_eq!(lexer.next(), Some(Ok(Token::LessEq)));
    assert_eq!(lexer.next(), Some(Ok(Token::GreaterEq)));
    assert_eq!(lexer.next(), Some(Ok(Token::And)));
    assert_eq!(lexer.next(), Some(Ok(Token::Or)));
    assert_eq!(lexer.next(), Some(Ok(Token::Xor)));
    assert_eq!(lexer.next(), Some(Ok(Token::Nor)));
    assert_eq!(lexer.next(), Some(Ok(Token::Not)));
}

#[test]
fn test_punctuations() {
    let input = "() [] , ; : -> . :: {} | = += -= *= /= %=";
    let mut lexer = Token::lexer(input);

    assert_eq!(lexer.next(), Some(Ok(Token::LParen)));
    assert_eq!(lexer.next(), Some(Ok(Token::RParen)));
    assert_eq!(lexer.next(), Some(Ok(Token::LBracket)));
    assert_eq!(lexer.next(), Some(Ok(Token::RBracket)));
    assert_eq!(lexer.next(), Some(Ok(Token::Comma)));
    assert_eq!(lexer.next(), Some(Ok(Token::Semicolon)));
    assert_eq!(lexer.next(), Some(Ok(Token::Colon)));
    assert_eq!(lexer.next(), Some(Ok(Token::Arrow)));
    assert_eq!(lexer.next(), Some(Ok(Token::Dot)));
    assert_eq!(lexer.next(), Some(Ok(Token::Access)));
    assert_eq!(lexer.next(), Some(Ok(Token::LBrace)));
    assert_eq!(lexer.next(), Some(Ok(Token::RBrace)));
    assert_eq!(lexer.next(), Some(Ok(Token::Union)));
    assert_eq!(lexer.next(), Some(Ok(Token::Assign)));
    assert_eq!(lexer.next(), Some(Ok(Token::AddAssign)));
    assert_eq!(lexer.next(), Some(Ok(Token::SubAssign)));
    assert_eq!(lexer.next(), Some(Ok(Token::MulAssign)));
    assert_eq!(lexer.next(), Some(Ok(Token::DivAssign)));
    assert_eq!(lexer.next(), Some(Ok(Token::ModAssign)));
}

#[test]
fn test_keywords() {
    let input = "if else def fn struct do end enum type match return";
    let mut lexer = Token::lexer(input);

    assert_eq!(lexer.next(), Some(Ok(Token::KeywordIf)));
    assert_eq!(lexer.next(), Some(Ok(Token::KeywordElse)));
    assert_eq!(lexer.next(), Some(Ok(Token::KeywordDef)));
    assert_eq!(lexer.next(), Some(Ok(Token::KeywordFn)));
    assert_eq!(lexer.next(), Some(Ok(Token::KeywordStruct)));
    assert_eq!(lexer.next(), Some(Ok(Token::KeywordDo)));
    assert_eq!(lexer.next(), Some(Ok(Token::KeywordEnd)));
    assert_eq!(lexer.next(), Some(Ok(Token::KeywordEnum)));
    assert_eq!(lexer.next(), Some(Ok(Token::KeywordType)));
    assert_eq!(lexer.next(), Some(Ok(Token::KeywordMatch)));
    assert_eq!(lexer.next(), Some(Ok(Token::KeywordReturn)));
}

#[test]
fn test_pattern_matching_tokens() {
    let input = "=> _";
    let mut lexer = Token::lexer(input);

    assert_eq!(lexer.next(), Some(Ok(Token::FatArrow)));
    assert_eq!(lexer.next(), Some(Ok(Token::Underscore)));
}

#[test]
fn test_complex_expression() {
    let input = "let result = (10 + 5) * my_var / 2.0;";
    let mut lexer = Token::lexer(input);

    assert_eq!(lexer.next(), Some(Ok(Token::KeywordLet)));
    assert_eq!(lexer.next(), Some(Ok(Token::Variable("result".to_string()))));
    assert_eq!(lexer.next(), Some(Ok(Token::Assign)));
    assert_eq!(lexer.next(), Some(Ok(Token::LParen)));
    assert_eq!(lexer.next(), Some(Ok(Token::Int(10))));
    assert_eq!(lexer.next(), Some(Ok(Token::Plus)));
    assert_eq!(lexer.next(), Some(Ok(Token::Int(5))));
    assert_eq!(lexer.next(), Some(Ok(Token::RParen)));
    assert_eq!(lexer.next(), Some(Ok(Token::Mul)));
    assert_eq!(lexer.next(), Some(Ok(Token::Variable("my_var".to_string()))));
    assert_eq!(lexer.next(), Some(Ok(Token::Div)));
    assert_eq!(lexer.next(), Some(Ok(Token::Float(2.0))));
    assert_eq!(lexer.next(), Some(Ok(Token::Semicolon)));
}

#[test]
fn test_comments() {
    let input = "
    # This is a comment
    let x = 1 # Another comment
    ";
    let mut lexer = Token::lexer(input);

    assert_eq!(lexer.next(), Some(Ok(Token::KeywordLet)));
    assert_eq!(lexer.next(), Some(Ok(Token::Variable("x".to_string()))));
    assert_eq!(lexer.next(), Some(Ok(Token::Assign)));
    assert_eq!(lexer.next(), Some(Ok(Token::Int(1))));
    assert_eq!(lexer.next(), None); // Ensure comments are skipped entirely
}

#[test]
fn test_empty_input() {
    let input = "";
    let mut lexer = Token::lexer(input);
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_whitespace_only_input() {
    let input = "   \n\t  \r";
    let mut lexer = Token::lexer(input);
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_unmatched_quote_string() {
    let input = r#""unclosed string"#;
    let mut lexer = Token::lexer(input);
    // Logos will stop at the end of the input or the first invalid character.
    // In this case, it won't produce a 'String' token as the regex won't match the unclosed string.
    assert!(lexer.next().unwrap().is_err());
}
