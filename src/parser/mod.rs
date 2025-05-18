pub mod expression;
pub mod function;
pub mod struct_;

use crate::ast::ASTNode;
use crate::lexer::Token;

use ariadne::{Report, Source};
use logos::SpannedIter;

use std::fs;
use std::iter::Peekable;
use std::ops::Range;

type TokenIter<'a> = Peekable<SpannedIter<'a, Token>>;

// #[derive(Debug)]
pub struct Parser<'a> {
    // lastspan: Range<usize>,
    tokens: TokenIter<'a>,
    file: String,
    errors: Vec<Report<'a, (String, Range<usize>)>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: TokenIter<'a>, file: String) -> Self {
        Parser {
            // lastspan: 0..1,
            tokens,
            file,
            errors: vec![],
        }
    }

    pub fn parse_program(&mut self) -> Vec<(ASTNode, Range<usize>)> {
        let mut tree = vec![];
        while let Some((token, span)) = self.tokens.peek() {
            if token.is_err() {
                panic!("error at {:?}. unexpected token", span);
            }
            match token.as_ref().unwrap() {
                Token::KeywordStruct => tree.push(self.parse_struct()),
                Token::KeywordDef => tree.push(self.parse_function()),
                _ => {
                    let (expr, span) = self.parse_expression();
                    tree.push((ASTNode::Expr((expr, span.clone())), span))
                },
            }
        }
        return tree;
    }

    pub fn report_errors(&self) {
        let contents =
            fs::read_to_string(&self.file).expect("Should have been able to read the file :/");
        let source = Source::from(contents);

        for error in &self.errors {
            error.print((self.file.clone(), source.clone())).unwrap();
        }
    }
}
