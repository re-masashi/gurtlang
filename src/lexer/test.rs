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
