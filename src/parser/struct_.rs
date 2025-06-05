use crate::ast::{ASTNode, Struct};
use crate::lexer::Token;
use crate::parser::Parser;
use ariadne::ColorGenerator;
use ariadne::Label;
use ariadne::Report;
use ariadne::ReportKind;

use std::ops::Range;

impl Parser<'_> {
    pub fn parse_struct(&mut self) -> (ASTNode, Range<usize>) {
        let Some((_token, span_struct)) = self.tokens.next() else {
            unreachable!()
        };
        let mut generics = vec![];
        let mut fields = vec![];

        let Some((token, span_name)) = self.tokens.next() else {
            unreachable!()
        };

        let name = if let Token::Variable(var) = token.unwrap() {
            var.to_string()
        } else {
            self.errors.push((
                ReportKind::Error,
                Report::build(ReportKind::Error, (self.file.clone(), span_name.clone()))
                    .with_code("EOF")
                    .with_label(
                        Label::new((self.file.clone(), span_name.clone()))
                            .with_message("unexpected end of file.")
                            .with_color(ColorGenerator::new().next()),
                    )
                    .with_message("unexpected EOF in struct declaration")
                    .finish(),
            ));
            return (ASTNode::Error, span_name.clone());
        };

        let Some((token, _span_tok)) = self.tokens.peek() else {
            // EOF error
            unreachable!()
        };

        if let Token::Less = token.clone().unwrap() {
            let Some((_token, _span_tok)) = self.tokens.next() else {
                unreachable!()
            };

            loop {
                let Some((token, _span_tok)) = self.tokens.peek() else {
                    panic!("EOR ERROR IN STRUCT GENERICS")
                };
                match token.as_ref().unwrap() {
                    Token::Greater => {
                        let Some((_token, _span_tok)) = self.tokens.next() else {
                            unreachable!()
                        };

                        break;
                    }
                    Token::Variable(_) => {
                        let Some((Ok(Token::Variable(a)), span_identifier)) = self.tokens.next()
                        else {
                            unreachable!()
                        };
                        generics.push((a.clone(), span_identifier));
                        let Some((tok, _)) = self.tokens.peek() else {
                            panic!("EOF IN STRUCT GENERICS")
                        };
                        if tok.as_ref().unwrap() == &Token::Comma {
                            let Some((_tok, _)) = self.tokens.next() else {
                                unreachable!()
                            };
                        }
                    }
                    _ => todo!(),
                }
            }

            // self.errors.push((
            //     ReportKind::Error,
            //     Report::build(ReportKind::Error, (self.file.clone(), span_name.clone()))
            //         .with_code("NOT IMPLEMENTED")
            //         .with_label(
            //             Label::new((self.file.clone(), span_name.clone()))
            //                 .with_message("STRUCT GENERICS ARE NOT IMPLEMENTED YET")
            //                 .with_color(ColorGenerator::new().next()),
            //         )
            //         .with_message("TODO: STRUCT GENERICS")
            //         .finish(),
            // ));
            // return (ASTNode::Error, span_name.clone());
        };

        loop {
            let Some((token, span)) = self.tokens.next() else {
                self.errors.push((
                    ReportKind::Error,
                    Report::build(ReportKind::Error, (self.file.clone(), span_name.clone()))
                        .with_code("EOF")
                        .with_label(
                            Label::new((self.file.clone(), span_name.clone()))
                                .with_message("unexpected end of file.")
                                .with_color(ColorGenerator::new().next()),
                        )
                        .with_message("unexpected EOF in struct declaration")
                        .finish(),
                ));
                return (ASTNode::Error, span_name.clone());
            };
            let token = token.unwrap();
            match token {
                Token::Variable(ref var) => {
                    fields.push((
                        var.to_string(),
                        self.parse_type_annotation(span.clone()).unwrap(),
                        span,
                    ));
                }
                Token::KeywordEnd => {
                    self.tokens.next();
                    break;
                }
                x => unimplemented!("{:?}", x),
            }
        }

        (
            ASTNode::Struct(Struct {
                name: (name, span_name.clone()),
                fields,
                generics,
            }),
            span_struct.start..span_name.end + 1,
        )
    }
}
