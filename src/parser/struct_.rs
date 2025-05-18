use crate::ast::{ASTNode, Struct};
use crate::parser::{Parser};
use crate::lexer::Token;

use std::ops::Range;

impl<'a> Parser<'a> {
    pub fn parse_struct(&mut self) -> (ASTNode, Range<usize>) {
        let Some((_token, span_struct)) = self.tokens.next() else {unreachable!()};
        let mut fields = vec![];

        let Some((token, span_name)) = self.tokens.next() else {unreachable!()};

        let name = if let Token::Variable(var) = token.unwrap() {
            var.to_string()
        }else{
            panic!("expected struct name after struct keyword");
        };

        loop {
            let Some((token, span)) = self.tokens.next() else {
                panic!("expected end or struct field. reached end of file");
            };
            let token = token.unwrap();
            match token {
                Token::Variable(ref var)=>{
                    fields.push((var.to_string(), self.parse_type_annotation(span.clone()).unwrap(), span));
                }
                Token::KeywordEnd => {
                    self.tokens.next();
                    break
                }
                _=>todo!()
            }
        }

        (ASTNode::Struct(Struct {
            name,
            fields
        }), span_struct.start..span_name.end+1)

        // todo!()   
    }
}
