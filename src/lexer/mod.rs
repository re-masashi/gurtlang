use logos::Logos;

#[cfg(test)]
pub mod test;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \n\r\t\f]+")] // Ignore this regex pattern between tokens
#[logos(skip r"#(.*)\n")] // Ignore this regex pattern between tokens
#[derive(Clone)]
pub enum Token {
    #[regex(r"true|false", |lex| {
        lex.slice().parse::<bool>().unwrap()
    })]
    Bool(bool),

    #[regex(r"0|[1-9][0-9]*", |lex| {
        lex.slice().parse::<i64>().unwrap()
    }, priority = 3)]
    Int(i64),

    #[regex(r"([0-9]*\.[0-9]+|[0-9]+\.?)", |lex| {
        lex.slice().parse::<f64>().unwrap()
    })]
    Float(f64),

    #[regex(r#""([^"\\]*(\\.[^"\\]*)*)""#, |lex| {
        let s = lex.slice();
        s[1..s.len()-1]
            .replace("\\\"", "\"")
            .replace("\\\\", "\\")
            .replace("\\n", "\n")
            .replace("\\r", "\r")
            .replace("\\t", "\t")
        // Removes quotes and handle escape sequences
    })]
    String(String),

    #[regex(r#"r#?"([^"\\]|\\.)*"#, |lex| {
        let s = lex.slice();
        s[3..s.len()-1]
            .replace("\\\"", "\"")
            .replace("\\\\", "\\")
            .replace("\\\n", "\n")
            .replace("\\\r", "\r")
            .replace("\\\t", "\t")
        // Removes r# and quotes and escape sequences
    })]
    RawString(String),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex|{
        lex.slice().to_string()
    })]
    Variable(String),

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Mul,

    #[token("/")]
    Div,

    #[token("%")]
    Mod,

    #[token("**", priority = 3)]
    Power,

    #[token("==")]
    Eq,

    #[token("!=")]
    NotEq,

    #[token("<")]
    Less,

    #[token(">")]
    Greater,

    #[token("<=")]
    LessEq,

    #[token(">=")]
    GreaterEq,

    #[token("and")]
    And,

    #[token("or")]
    Or,

    #[token("xor")]
    Xor,

    #[token("nor")]
    Nor,

    #[token("not")]
    Not,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[token(",")]
    Comma,

    #[token(";")]
    Semicolon,

    #[token(":")]
    Colon,

    #[token("->")]
    Arrow,

    #[token(".")]
    Dot,

    #[token("let")]
    KeywordLet,

    #[token("if")]
    KeywordIf,

    #[token("else")]
    KeywordElse,

    #[token("def")]
    KeywordDef,

    #[token("fn")]
    KeywordFn,

    #[token("struct")]
    KeywordStruct,

    #[token("do")]
    KeywordDo,

    #[token("end")]
    KeywordEnd,

    #[token("|")]
    Union,

    #[token("=")]
    Assign,

    #[token("+=")]
    AddAssign,

    #[token("-=")]
    SubAssign,

    #[token("*=")]
    MulAssign,

    #[token("/=")]
    DivAssign,

    #[token("%=")]
    ModAssign,
}
