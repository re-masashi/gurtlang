pub mod expression;
pub mod function;
pub mod struct_;

#[cfg(test)]
pub mod test;

use crate::ast::ASTNode;
use crate::lexer::Token;
use ariadne::ColorGenerator;
use ariadne::Label;

use ariadne::{Report, ReportKind, Source};
use logos::SpannedIter;

use std::fs;
use std::iter::Peekable;
use std::ops::Range;

type TokenIter<'a> = Peekable<SpannedIter<'a, Token>>;
type Error<'a> = (ReportKind<'a>, Report<'a, (String, Range<usize>)>);

// #[derive(Debug)]
pub struct Parser<'a> {
    // lastspan: Range<usize>,
    tokens: TokenIter<'a>,
    file: String,
    pub errors: Vec<Error<'a>>,
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
                self.errors.push((
                    ReportKind::Error,
                    Report::build(ReportKind::Error, (self.file.clone(), span.clone()))
                        .with_code("IllegalToken")
                        .with_label(
                            Label::new((self.file.clone(), span.clone()))
                                .with_message("illegal token")
                                .with_color(ColorGenerator::new().next()),
                        )
                        .with_message("illegal token found.")
                        .finish(),
                ));
                tree.push((ASTNode::Error, span.clone()));
            }
            match token.as_ref().unwrap() {
                Token::KeywordStruct => tree.push(self.parse_struct()),
                Token::KeywordDef => tree.push(self.parse_function()),
                Token::KeywordExtern => tree.push(self.parse_extern()),
                Token::KeywordEnum => tree.push(self.parse_enum()),
                Token::KeywordType => tree.push(self.parse_type_alias()),
                Token::Semicolon => {
                    // Consume empty semicolons
                    self.tokens.next();
                    continue;
                }
                _ => {
                    let (expr, span) = self.parse_expression();
                    tree.push((ASTNode::Expr((expr, span.clone())), span))
                }
            }
        }
        tree
    }

    pub fn report_errors(&self) -> bool {
        let contents =
            fs::read_to_string(&self.file).expect("Should have been able to read the file :/");
        let source = Source::from(contents);

        let mut failed = false;

        for (kind, error) in &self.errors {
            if *kind == ReportKind::Error {
                failed = true;
            }
            error.print((self.file.clone(), source.clone())).unwrap();
        }
        failed
    }
}
