use yansi::Paint;

use crate::ast::{ASTNode, TypeAnnot, Function};
use crate::parser::Parser;
use crate::lexer::Token;

use std::ops::Range;

use ariadne::{ColorGenerator, Label, Report, ReportKind, Color, Fmt};

 pub fn function_syntax() -> String {
        format!(
            "\
            The syntax for declaring a function is: 
            {} function_name({}: {}, {}: {}, ...) -> return_type
                expression
            ",
            Fmt::fg("def", Color::Yellow).bold(),
            Fmt::fg("arg1", Color::Rgb(150, 200, 100)).bold(),
            Fmt::fg("type1", Color::Rgb(205, 150, 100)).bold(),
            Fmt::fg("arg2", Color::Rgb(150, 200, 100)).bold(),
            Fmt::fg("type2", Color::Rgb(205, 150, 100)).bold(),
        )
    }



impl<'a> Parser<'a> {
    pub fn parse_function(&mut self) -> (ASTNode, Range<usize>) {
        let Some((_token, span_function)) = self.tokens.next() else {
            unreachable!() // never happens
        };

        let Some((token, _span_name)) = self.tokens.next() else {
            self.errors.push(
                Report::build(
                    ReportKind::Error,
                    (self.file.clone(), span_function.clone()),
                )
                .with_code("EOF")
                .with_label(
                    Label::new((self.file.clone(), span_function.clone()))
                        .with_message(format!(
                            "expected function name after '{}' but reached end of file",
                            Fmt::fg("def", Color::Yellow).bold(),
                        ))
                        .with_color(ColorGenerator::new().next()),
                )
                .with_note(function_syntax())
                .with_message("reached end of file while trying to parse a function definition.")
                .finish(),
            );
            return (ASTNode::Error, span_function);
        };

        let token = token.unwrap();

        let fun_name = if let Token::Variable(fun_name) = token {
            fun_name
        } else {
            self.errors.push(
                Report::build(
                    ReportKind::Error,
                    (self.file.clone(), span_function.clone()),
                )
                .with_code("EOF")
                .with_label(
                    Label::new((self.file.clone(), span_function.clone()))
                        .with_message(format!(
                            "expected function name after '{}'. got {} instead",
                            Fmt::fg("def", Color::Yellow).bold(),
                            Fmt::fg(format!("{:?}", token), Color::BrightRed).bold(),

                        ))
                        .with_color(ColorGenerator::new().next()),
                )
                .with_note(function_syntax())
                .with_message("found unexpected token after 'def'. expected a valid identifier.")
                .finish(),
            );
            return (ASTNode::Error, span_function);
        };

        let Some((token, span_starting_paren)) = self.tokens.next() else {
            self.errors.push(
                Report::build(
                    ReportKind::Error,
                    (self.file.clone(), span_function.clone()),
                )
                .with_code("EOF")
                .with_label(
                    Label::new((self.file.clone(), span_function.clone()))
                        .with_message(format!(
                            "expected '(' name after fucntion name but reached end of file",
                        ))
                        .with_color(ColorGenerator::new().next()),
                )
                .with_note(function_syntax())
                .with_message("reached end of file while trying to parse a function definition.")
                .finish(),
            );
            return (ASTNode::Error, span_function);
        };

        let token = token.unwrap();

        // println!("{:?}", self.tokens.peek());

        if let Token::LParen = token {
            // do nothing
        } else {
            self.errors.push(
                Report::build(
                    ReportKind::Error,
                    (self.file.clone(), span_function.clone()),
                )
                .with_code("EOF")
                .with_label(
                    Label::new((self.file.clone(), span_function.clone()))
                        .with_message(format!(
                            "expected '{}' after function name. got {} instead",
                            Fmt::fg('(', Color::BrightRed).bold(),
                            Fmt::fg(format!("{:?}", token), Color::BrightRed).bold(),
                        ))
                        .with_color(ColorGenerator::new().next()),
                )
                .with_note(function_syntax())
                .with_message(format!(
                    "found unexpected token after the name of function `{}`. expected a valid identifier.",
                    Fmt::fg(fun_name, Color::Blue).bold(),
                ))
                .finish(),
            );
            return (ASTNode::Error, span_function);
        };

        let mut args = vec![];

        loop {
            let Some((token, span)) = self.tokens.peek() else {
                panic!("invalid token. no errors cuz im lazy {} {:?}", self.file, span_starting_paren)
            };
            let span = span.clone();
            match token.as_ref().unwrap() {
                Token::RParen => {
                    self.tokens.next();
                    break
                }
                Token::Comma => {
                    self.tokens.next();
                    continue
                }
                _ => {
                    args.push(self.parse_function_arg(span).unwrap());
                },
            }
        }

        let Some((token, _span)) = self.tokens.peek() else {
            return (ASTNode::Function(
                Function {
                    name: fun_name,
                    args,
                    body: Box::new(self.parse_expression()),
                    return_type: None
                }
            ), span_function)
        };

        match token.as_ref().unwrap() {
            Token::Arrow => {
                let Some((_, span)) = self.tokens.next() else {unreachable!()};
                let return_type = self.parse_type_annotation(span.clone()).unwrap();
                return (ASTNode::Function(
                    Function {
                        name: fun_name,
                        args,
                        body: Box::new(self.parse_expression()),
                        return_type: Some(return_type)
                    }
                ), span_function)
            },
            _=>{
                return (ASTNode::Function(
                    Function {
                        name: fun_name,
                        args,
                        body: Box::new(self.parse_expression()),
                        return_type: None
                    }
                ), span_function)
            }
        }
    }

    pub fn parse_function_arg(&mut self, _span: Range<usize>) -> Result<(String, Option<TypeAnnot>), Report<'a, (String, Range<usize>)>> {
        let Some((token, span)) = self.tokens.next() else {
            self.errors.push(
                Report::build(
                    ReportKind::Error,
                    (self.file.clone(), _span.clone()),
                )
                .with_code("EOF")
                .with_label(
                    Label::new((self.file.clone(), _span))
                        .with_message(format!(
                            "expected a valid identifier as argument name but reached end of file",
                        ))
                        .with_color(ColorGenerator::new().next()),
                )
                .with_note(function_syntax())
                .with_message("reached end of file while trying to parse a function definition.")
                .finish(),
            );
            todo!()
            // return Err();
        };

        let token = token.unwrap();

        let arg_name  = if let Token::Variable(ref arg_name) = token {
            arg_name
        } else {
            return Err(
                Report::build(
                    ReportKind::Error,
                    (self.file.clone(), span.clone()),
                )
                .with_code("EOF")
                .with_label(
                    Label::new((self.file.clone(), span))
                        .with_message(format!(
                            "expected a valid identifier as argument name but got {} instead",
                            Fmt::fg(format!("{:?}", token), Color::BrightRed).bold(),
                        ))
                        .with_color(ColorGenerator::new().next()),
                )
                .with_note(function_syntax())
                .with_message("found unexpected token. expected a valid identifier.")
                .finish(),
            );
        };

        let Some((token, _span)) = self.tokens.peek() else {
            return Ok((arg_name.to_string(), None))
        };

        if let Token::Colon = token.as_ref().unwrap() {
            let Some((_, span)) = self.tokens.next() else {unreachable!()};

            let type_annot = self.parse_type_annotation(span.clone())?;

            self.errors.push(
            Report::build(
                ReportKind::Warning,
                (self.file.clone(), span.clone()),
            )
            .with_code("Unimplemented")
            .with_label(
                Label::new((self.file.clone(), span.clone()))
                    .with_message(
                        "type annotations cannot be parsed yet.",
                    )
                    .with_color(ColorGenerator::new().next()),
            )
            .with_note(function_syntax())
            .with_message("found an argument with a type annotation in function definition. type annotations are not fully supported yet")
            .finish(),
        );
             return Ok((arg_name.to_string(), Some(type_annot)))
            

        } else {
            return Ok((arg_name.to_string(), None))
        } 

        // panic!("FUNCTION ARGS PARSING YET TO BE DONE")
    }

    pub fn parse_type_annotation(&mut self, _span: Range<usize>) -> Result<TypeAnnot, Report<'a, (String, Range<usize>)>> {
        let Some((token, span)) = self.tokens.next() else {
            return Err(
                Report::build(
                    ReportKind::Error,
                    (self.file.clone(), _span.clone()),
                )
                .with_code("EOF")
                .with_label(
                    Label::new((self.file.clone(), _span))
                        .with_message(format!(
                            "expected a valid type after colon but reached end of file",
                        ))
                        .with_color(ColorGenerator::new().next()),
                )
                .with_note(function_syntax())
                .with_message("reached end of file while trying to parse a type annotation")
                .finish(),
            );
            // todo!()
        };
        let token = token.unwrap();

        let type_annot = match token {
            Token::KeywordFn => todo!(),
            Token::LParen => todo!(), // tuple
            Token::Variable(ref var) if var == "int" => Ok(TypeAnnot::Int),
            Token::Variable(ref var) if var == "bool" => Ok(TypeAnnot::Bool),
            Token::Variable(ref var) if var == "string" => Ok(TypeAnnot::String),
            Token::Variable(ref var) if var == "float" => Ok(TypeAnnot::Float),
            Token::Variable(ref var) if var == "trait" => {
                let Some((token, span)) = self.tokens.next() else {
                    return Err(
                        Report::build(
                            ReportKind::Error,
                            (self.file.clone(), span.clone()),
                        )
                        .with_code("EOF")
                        .with_label(
                            Label::new((self.file.clone(), span))
                                .with_message(format!(
                                    "expected a valid type after colon but reached end of file",
                                ))
                                .with_color(ColorGenerator::new().next()),
                        )
                        .with_note(function_syntax())
                        .with_message("reached end of file while trying to parse a type annotation")
                        .finish(),
                    );
                };
                let token = token.unwrap();
                match token {
                    Token::Variable(ref var) => Ok(TypeAnnot::Trait(var.clone())),
                    _ => Err(
                        Report::build(
                            ReportKind::Error,
                            (self.file.clone(), span.clone()),
                        )
                        .with_code("EOF")
                        .with_label(
                            Label::new((self.file.clone(), span))
                                .with_message(format!(
                                    "expected a valid type after 'trait' but got {} instead",
                                    Fmt::fg(format!("{:?}", token), Color::BrightRed).bold(),
                                ))
                                .with_color(ColorGenerator::new().next()),
                        )
                        .with_note(function_syntax())
                        .with_message("found unexpected token. expected a valid type.")
                        .finish(),
                    ),
                }
            },
            Token::Variable(ref var) => {
                let ret = TypeAnnot::Boring(var.clone());
                let Some((token, _span)) = self.tokens.peek() else {
                    return Ok(ret)
                    // todo!()
                };
                let token = token.as_ref().unwrap();

                match token {
                    Token::Less => {
                        let Some((_, span)) = self.tokens.next() else {unreachable!()}; // eat '<'
                        let mut generics = vec![];
                        loop {
                            let Some((token, span)) = self.tokens.peek() else {
                                panic!("invalid token. no errors cuz im lazy {} {:?}", self.file, span)
                            };
                            let span = span.clone();
                            match token.as_ref().unwrap() {
                                Token::Greater => {
                                    self.tokens.next();
                                    break
                                }
                                Token::Comma => {
                                    self.tokens.next();
                                    continue
                                }
                                _ => {
                                    generics.push(self.parse_type_annotation(span).unwrap());
                                },
                            }
                        };
                        Ok(TypeAnnot::Generic(var.to_string(), generics))
                    },
                    _=>Ok(ret)
                }
            },
            _ => Err(
                Report::build(
                    ReportKind::Error,
                    (self.file.clone(), span.clone()),
                )
                .with_code("EOF")
                .with_label(
                    Label::new((self.file.clone(), span))
                        .with_message(format!(
                            "expected a valid type after colon but got {} instead",
                            Fmt::fg(format!("{:?}", token), Color::BrightRed).bold(),
                        ))
                        .with_color(ColorGenerator::new().next()),
                )
                .with_note(function_syntax())
                .with_message("found unexpected token. expected a valid type.")
                .finish(),
            ),
        };

        type_annot
    }
}
