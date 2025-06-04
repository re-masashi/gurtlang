use crate::ast::{ASTNode, Struct};
use crate::lexer::Token;
use crate::parser::Parser;

use std::ops::Range;

impl Parser<'_> {
    pub fn parse_struct(&mut self) -> (ASTNode, Range<usize>) {
        let Some((_token, span_struct)) = self.tokens.next() else {
            unreachable!()
        };
        let mut fields = vec![];

        let Some((token, span_name)) = self.tokens.next() else {
            unreachable!()
        };

        let name = if let Token::Variable(var) = token.unwrap() {
            var.to_string()
        } else {
            panic!("expected struct name after struct keyword");
        };

        let Some((token, _span_tok)) = self.tokens.peek() else {
            // EOF error
            unreachable!()
        };

        if let Token::Less = token.clone().unwrap() {
            let Some((_token, _span_tok)) = self.tokens.peek() else {
                unreachable!()
            };

            todo!("parsing generics not implemented yet")
        };

        loop {
            let Some((token, span)) = self.tokens.next() else {
                panic!("expected end or struct field. reached end of file");
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
                _ => todo!(),
            }
        }

        let Some((token, _span)) = self.tokens.peek() else {
            return (
                ASTNode::Struct(Struct {
                    name: (name, span_name.clone()),
                    fields,
                    generics: vec![],
                }),
                span_struct.start..span_name.end + 1,
            );
            // todo!()
        };
        let token = token.as_ref().unwrap();

        match token {
            Token::Less => {
                let Some((_, span)) = self.tokens.next() else {
                    unreachable!()
                }; // eat '<'
                let mut generics = vec![];
                loop {
                    let Some((token, span)) = self.tokens.peek() else {
                        panic!(
                            "invalid token. no errors cuz im lazy {} {:?}",
                            self.file, span
                        )
                    };
                    let _span = span.clone();
                    match token.as_ref().unwrap() {
                        Token::Greater => {
                            self.tokens.next();
                            break;
                        }
                        Token::Comma => {
                            self.tokens.next();
                            continue;
                        }
                        _ => {
                            let Some((token, _span)) = self.tokens.peek() else {
                                unreachable!()
                            };
                            if let Token::Variable(_v) = token.as_ref().unwrap() {
                                let Some((Ok(Token::Variable(v)), var_span)) = self.tokens.next()
                                else {
                                    panic!("invalid geneirc");
                                };
                                generics.push((v.to_string(), var_span))
                            } else {
                                panic!("invalid geneirc");
                            }
                        }
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
            _ => (
                ASTNode::Struct(Struct {
                    name: (name, span_name.clone()),
                    fields,
                    generics: vec![],
                }),
                span_struct.start..span_name.end + 1,
            ),
        }

        // todo!()
    }
}
