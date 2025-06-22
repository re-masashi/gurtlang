use crate::ast::Enum;
use crate::ast::EnumVariant;
use crate::ast::EnumVariantKind;
use crate::ast::TypeAlias;
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
            panic!("STRUCT NEEDS A NAME")
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
                    // end already 'eaten'
                    // println!("{:?}", self.tokens.next());
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

    pub fn parse_enum(&mut self) -> (ASTNode, Range<usize>) {
        let Some((_token, start_span)) = self.tokens.next() else {
            unreachable!()
        };
        // Parse enum name
        let (name, name_span) = match self.tokens.next() {
            Some((Ok(Token::Variable(name)), span)) => (name, span.clone()),
            _ => {
                // Error handling
                return (ASTNode::Error, start_span);
            }
        };

        // Parse generics
        let mut generics = vec![];
        if let Some((Ok(Token::Less), _)) = self.tokens.peek() {
            self.tokens.next(); // Consume '<'
            while let Some((Ok(Token::Variable(generic)), span)) = self.tokens.next() {
                generics.push((generic, span.clone()));
                match self.tokens.peek() {
                    Some((Ok(Token::Comma), _)) => self.tokens.next(),
                    Some((Ok(Token::Greater), _)) => {
                        self.tokens.next(); // Consume '>'
                        break;
                    }
                    _ => break,
                };
            }
        }

        // Parse variants
        let mut variants = vec![];
        while let Some((token, _)) = self.tokens.peek() {
            if let Token::KeywordEnd = token.as_ref().unwrap() {
                break;
            }

            let (variant_name, variant_span) = match self.tokens.next() {
                Some((Ok(Token::Variable(name)), span)) => (name, span.clone()),
                _ => continue, // Skip to next variant on error
            };

            let kind = if let Some((Ok(Token::LParen), _)) = self.tokens.peek() {
                let Some((_, paren_span)) = self.tokens.next() else {
                    unreachable!()
                }; // Consume '('
                let mut types = vec![];
                while let Some((token, _span)) = self.tokens.peek() {
                    if let Token::RParen = token.as_ref().unwrap() {
                        self.tokens.next(); // Consume ')'
                        break;
                    }

                    let type_annot = self.parse_type_annotation(paren_span.clone()).unwrap();
                    types.push((type_annot, paren_span.clone()));

                    if let Some((Ok(Token::Comma), _)) = self.tokens.peek() {
                        self.tokens.next();
                    }
                }
                EnumVariantKind::Tuple(types)
            } else if let Some((Ok(Token::LBrace), _)) = self.tokens.peek() {
                self.tokens.next(); // Consume '{'
                let mut fields = vec![];
                while let Some((token, _)) = self.tokens.peek() {
                    if let Token::RBrace = token.as_ref().unwrap() {
                        self.tokens.next(); // Consume '}'
                        break;
                    }

                    let (field_name, field_span) = match self.tokens.next() {
                        Some((Ok(Token::Variable(name)), span)) => (name, span.clone()),
                        _ => continue,
                    };

                    if let Some((Ok(Token::Colon), _span)) = self.tokens.peek() {
                        let Some((_, span)) = self.tokens.next() else {
                            unreachable!()
                        };
                        let type_annot = self.parse_type_annotation(span.clone()).unwrap();
                        fields.push((field_name, type_annot, field_span));
                    }

                    if let Some((Ok(Token::Comma), _)) = self.tokens.peek() {
                        self.tokens.next();
                    }
                }
                EnumVariantKind::Struct(fields)
            } else {
                EnumVariantKind::Unit
            };

            variants.push(EnumVariant {
                name: (variant_name, name_span.clone()),
                kind,
                range: variant_span,
            });

            if let Some((Ok(Token::Comma), _)) = self.tokens.peek() {
                self.tokens.next();
            }
        }

        // Consume 'end'
        let end_span = if let Some((_, span)) = self.tokens.next() {
            span.clone()
        } else {
            start_span.end..start_span.end
        };

        let full_span = start_span.start..end_span.end;
        (
            ASTNode::Enum(
                Enum {
                    name: (name, name_span),
                    generics,
                    variants,
                },
                full_span.clone(),
            ),
            full_span,
        )
    }

    pub fn parse_type_alias(&mut self) -> (ASTNode, Range<usize>) {
        let Some((_token, start_span)) = self.tokens.next() else {
            unreachable!()
        };
        // Parse alias name
        let (name, name_span) = match self.tokens.next() {
            Some((Ok(Token::Variable(name)), span)) => (name, span.clone()),
            _ => return (ASTNode::Error, start_span),
        };

        // Parse generics
        let mut generics = vec![];
        if let Some((Ok(Token::Less), _)) = self.tokens.peek() {
            self.tokens.next(); // Consume '<'
            while let Some((Ok(Token::Variable(generic)), span)) = self.tokens.next() {
                generics.push((generic, span.clone()));
                match self.tokens.peek() {
                    Some((Ok(Token::Comma), _)) => self.tokens.next(),
                    Some((Ok(Token::Greater), _)) => {
                        self.tokens.next(); // Consume '>'
                        break;
                    }
                    _ => break,
                };
            }
        }

        // Parse '='
        if let Some((Ok(Token::Assign), span)) = self.tokens.next() {
            let aliased_type = self.parse_type_annotation(span.clone()).unwrap();
            let end_span = span.end..span.end;
            let full_span = start_span.start..end_span.end;

            (
                ASTNode::TypeAlias(
                    TypeAlias {
                        name: (name, name_span),
                        generics,
                        aliased_type: Box::new(aliased_type),
                        range: full_span.clone(),
                    },
                    full_span.clone(),
                ),
                full_span,
            )
        } else {
            // Error handling
            (ASTNode::Error, start_span)
        }
    }
}
