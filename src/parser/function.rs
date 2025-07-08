use yansi::Paint;

use crate::ast::{ASTNode, Extern, Function, TypeAnnot};
use crate::lexer::Token;
use crate::parser::Parser;

use std::ops::Range;

use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind};

type Arg = (String, Option<TypeAnnot>, Range<usize>);
type ErrReport = (String, Range<usize>);

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

        let Some((token, span_name)) = self.tokens.next() else {
            self.errors.push((
                ReportKind::Error,
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
            ));
            return (ASTNode::Error, span_function);
        };

        let token = token.unwrap();

        let fun_name = if let Token::Variable(fun_name) = token {
            fun_name
        } else {
            self.errors.push((
                ReportKind::Error,
                Report::build(ReportKind::Error, (self.file.clone(), span_name.clone()))
                    .with_code("EOF")
                    .with_label(
                        Label::new((self.file.clone(), span_name.clone()))
                            .with_message(format!(
                                "expected function name after '{}'. got {} instead",
                                Fmt::fg("def", Color::Yellow).bold(),
                                Fmt::fg(format!("{:?}", token), Color::BrightRed).bold(),
                            ))
                            .with_color(ColorGenerator::new().next()),
                    )
                    .with_note(function_syntax())
                    .with_message(
                        "found unexpected token after 'def'. expected a valid identifier.",
                    )
                    .finish(),
            ));
            return (ASTNode::Error, span_name);
        };

        let Some((token, _span_starting_paren)) = self.tokens.next() else {
            self.errors.push((
                ReportKind::Error,
                Report::build(
                    ReportKind::Error,
                    (self.file.clone(), span_function.clone()),
                )
                .with_code("EOF")
                .with_label(
                    Label::new((self.file.clone(), span_function.clone()))
                        .with_message(
                            "expected '(' name after fucntion name but reached end of file"
                                .to_string(),
                        )
                        .with_color(ColorGenerator::new().next()),
                )
                .with_note(function_syntax())
                .with_message("reached end of file while trying to parse a function definition.")
                .finish(),
            ));
            return (ASTNode::Error, span_function);
        };

        let token = token.unwrap();

        // println!("{:?}", self.tokens.peek());

        if let Token::LParen = token {
            // do nothing
        } else {
            self.errors.push((
                ReportKind::Error,
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
            ));
            return (ASTNode::Error, span_name);
        };

        let mut args = vec![];

        loop {
            let Some((token, span)) = self.tokens.peek() else {
                self.errors.push((
                    ReportKind::Error,
                    Report::build(
                        ReportKind::Error,
                        (self.file.clone(), span_function.clone()),
                    )
                    .with_code("EOF")
                    .with_label(
                        Label::new((self.file.clone(), span_function.clone()))
                            .with_message("unexpected EOF")
                            .with_color(ColorGenerator::new().next()),
                    )
                    .with_message("unexpected eof while parsing function")
                    .finish(),
                ));
                return (ASTNode::Error, span_function.clone());
            };
            let span = span.clone();
            match token.as_ref().unwrap() {
                Token::RParen => {
                    self.tokens.next();
                    break;
                }
                Token::Comma => {
                    self.tokens.next();
                    continue;
                }
                _ => {
                    let arg = match self.parse_function_arg(span.clone(), false) {
                        Ok(arg) => arg,
                        Err(e) => {
                            self.errors.push(*e);
                            return (ASTNode::Error, span);
                        }
                    };
                    args.push(arg);
                }
            }
        }

        let Some((token, _span)) = self.tokens.peek() else {
            return (
                ASTNode::Function(Function {
                    name: fun_name,
                    args,
                    body: Box::new(self.parse_expression()),
                    return_type: None,
                }),
                span_name,
            );
        };

        match token.as_ref().unwrap() {
            Token::Arrow => {
                let Some((_, span)) = self.tokens.next() else {
                    unreachable!()
                };
                let return_type = match self.parse_type_annotation(span.clone()) {
                    Ok(result) => result,
                    Err(e) => {
                        self.errors.push(*e);
                        return (ASTNode::Error, span.clone());
                    }
                };
                (
                    ASTNode::Function(Function {
                        name: fun_name,
                        args,
                        body: Box::new(self.parse_expression()),
                        return_type: Some((return_type, span)),
                    }),
                    span_name,
                )
            }
            _ => (
                ASTNode::Function(Function {
                    name: fun_name,
                    args,
                    body: Box::new(self.parse_expression()),
                    return_type: None,
                }),
                span_name,
            ),
        }
    }

    pub fn parse_extern(&mut self) -> (ASTNode, Range<usize>) {
        let Some((_token, span_function)) = self.tokens.next() else {
            unreachable!() // never happens
        };

        let Some((token, span_name)) = self.tokens.next() else {
            self.errors.push((
                ReportKind::Error,
                Report::build(
                    ReportKind::Error,
                    (self.file.clone(), span_function.clone()),
                )
                .with_code("EOF")
                .with_label(
                    Label::new((self.file.clone(), span_function.clone()))
                        .with_message(format!(
                            "expected externa function name after '{}' but reached end of file",
                            Fmt::fg("extern", Color::Yellow).bold(),
                        ))
                        .with_color(ColorGenerator::new().next()),
                )
                .with_note(function_syntax())
                .with_message(
                    "reached end of file while trying to parse an external function definition.",
                )
                .finish(),
            ));
            return (ASTNode::Error, span_function);
        };

        let token = token.unwrap();

        let fun_name = if let Token::Variable(fun_name) = token {
            fun_name
        } else {
            self.errors.push((
                ReportKind::Error,
                Report::build(ReportKind::Error, (self.file.clone(), span_name.clone()))
                    .with_code("EOF")
                    .with_label(
                        Label::new((self.file.clone(), span_name.clone()))
                            .with_message(format!(
                                "expected externa function name after '{}'. got {} instead",
                                Fmt::fg("extern", Color::Yellow).bold(),
                                Fmt::fg(format!("{:?}", token), Color::BrightRed).bold(),
                            ))
                            .with_color(ColorGenerator::new().next()),
                    )
                    .with_note(function_syntax())
                    .with_message(
                        "found unexpected token after 'extern'. expected a valid identifier.",
                    )
                    .finish(),
            ));
            return (ASTNode::Error, span_name);
        };

        let Some((token, _span_starting_paren)) = self.tokens.next() else {
            self.errors.push((
                ReportKind::Error,
                Report::build(
                    ReportKind::Error,
                    (self.file.clone(), span_function.clone()),
                )
                .with_code("EOF")
                .with_label(
                    Label::new((self.file.clone(), span_function.clone()))
                        .with_message(
                            "expected '(' name after external function name but reached end of file"
                                .to_string(),
                        )
                        .with_color(ColorGenerator::new().next()),
                )
                .with_note(function_syntax())
                .with_message("reached end of file while trying to parse an external function definition.")
                .finish(),
            ));
            return (ASTNode::Error, span_function);
        };

        let token = token.unwrap();

        // println!("{:?}", self.tokens.peek());

        if let Token::LParen = token {
            // do nothing
        } else {
            self.errors.push((
                ReportKind::Error,
                Report::build(
                    ReportKind::Error,
                    (self.file.clone(), span_function.clone()),
                )
                .with_code("EOF")
                .with_label(
                    Label::new((self.file.clone(), span_function.clone()))
                        .with_message(format!(
                            "expected '{}' after external function name. got {} instead",
                            Fmt::fg('(', Color::BrightRed).bold(),
                            Fmt::fg(format!("{:?}", token), Color::BrightRed).bold(),
                        ))
                        .with_color(ColorGenerator::new().next()),
                )
                .with_note(function_syntax())
                .with_message(format!(
                    "found unexpected token after the name of external function `{}`.",
                    Fmt::fg(fun_name, Color::Blue).bold(),
                ))
                .finish(),
            ));
            return (ASTNode::Error, span_name);
        };

        let mut args = vec![];

        loop {
            let Some((token, span)) = self.tokens.peek() else {
                self.errors.push((
                    ReportKind::Error,
                    Report::build(
                        ReportKind::Error,
                        (self.file.clone(), span_function.clone()),
                    )
                    .with_code("EOF")
                    .with_label(
                        Label::new((self.file.clone(), span_function.clone()))
                            .with_message("unexpected EOF")
                            .with_color(ColorGenerator::new().next()),
                    )
                    .with_message("unexpected eof while parsing function")
                    .finish(),
                ));
                return (ASTNode::Error, span_function.clone());
            };
            let span = span.clone();
            match token.as_ref().unwrap() {
                Token::RParen => {
                    self.tokens.next();
                    break;
                }
                Token::Comma => {
                    self.tokens.next();
                    continue;
                }
                _ => {
                    let arg = match self.parse_type_annotation(span.clone()) {
                        Ok(arg) => (arg, span),
                        Err(e) => {
                            self.errors.push(*e);
                            return (ASTNode::Error, span);
                        }
                    };
                    args.push(arg);
                }
            }
        }

        let Some((token, _span)) = self.tokens.peek() else {
            panic!("RETURN TYPE NEEDED FOR EXTERN");
        };

        match token.as_ref().unwrap() {
            Token::Arrow => {
                let Some((_, span)) = self.tokens.next() else {
                    unreachable!()
                };
                let return_type = match self.parse_type_annotation(span.clone()) {
                    Ok(result) => result,
                    Err(e) => {
                        self.errors.push(*e);
                        return (ASTNode::Error, span.clone());
                    }
                };
                (
                    ASTNode::Extern(Extern {
                        name: fun_name,
                        args,
                        return_type: (return_type, span),
                    }),
                    span_name,
                )
            }
            _ => panic!("return type needed"),
        }
    }

    pub fn parse_function_arg(
        &mut self,
        _span: Range<usize>,
        needs_annot: bool,
    ) -> Result<Arg, Box<(ReportKind<'a>, Report<'a, ErrReport>)>> {
        let Some((token, span)) = self.tokens.next() else {
            return Err(Box::new(
                (
                    ReportKind::Error,
                    Report::build(
                        ReportKind::Error,
                        (self.file.clone(), _span.clone()),
                    )
                    .with_code("EOF")
                    .with_label(
                        Label::new((self.file.clone(), _span))
                            .with_message("expected a valid identifier as argument name but reached end of file".to_string())
                            .with_color(ColorGenerator::new().next()),
                    )
                    .with_note(function_syntax())
                    .with_message("reached end of file while trying to parse a function definition.")
                        .finish(),
                    )
            ));
        };

        let token = token.unwrap();

        let arg_name = if let Token::Variable(ref arg_name) = token {
            arg_name
        } else {
            return Err(Box::new((
                ReportKind::Error,
                Report::build(ReportKind::Error, (self.file.clone(), span.clone()))
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
            )));
        };

        let Some((token, _span)) = self.tokens.peek() else {
            return Ok((arg_name.to_string(), None, span));
        };

        if let Token::Colon = token.as_ref().unwrap() {
            let Some((_, span)) = self.tokens.next() else {
                unreachable!()
            };

            let type_annot = self.parse_type_annotation(span.clone())?;

            Ok((arg_name.to_string(), Some(type_annot), span))
        } else if needs_annot {
            return Err(Box::new((
                ReportKind::Error,
                Report::build(ReportKind::Error, (self.file.clone(), span.clone()))
                    .with_code("Syntax Error")
                    .with_label(
                        Label::new((self.file.clone(), span))
                            .with_message(format!(
                                "expected a valid type annotation after argument name but got {} instead",
                                Fmt::fg(format!("{:?}", token), Color::BrightRed).bold(),
                            ))
                            .with_color(ColorGenerator::new().next()),
                    )
                    .with_note(function_syntax())
                    .with_message("found unexpected token. expected a valid identifier.")
                    .finish(),
            )));
        } else {
            Ok((arg_name.to_string(), None, span))
        }
    }

    pub fn parse_type_annotation(
        &mut self,
        _span: Range<usize>,
    ) -> Result<TypeAnnot, Box<(ReportKind<'a>, Report<'a, ErrReport>)>> {
        let Some((token, span)) = self.tokens.next() else {
            return Err(Box::new((
                ReportKind::Error,
                Report::build(ReportKind::Error, (self.file.clone(), _span.clone()))
                    .with_code("EOF")
                    .with_label(
                        Label::new((self.file.clone(), _span))
                            .with_message(
                                "expected a valid type after colon but reached end of file"
                                    .to_string(),
                            )
                            .with_color(ColorGenerator::new().next()),
                    )
                    .with_note(function_syntax())
                    .with_message("reached end of file while trying to parse a type annotation")
                    .finish(),
            )));
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
                    return Err(Box::new((
                        ReportKind::Error,
                        Report::build(ReportKind::Error, (self.file.clone(), span.clone()))
                            .with_code("EOF")
                            .with_label(
                                Label::new((self.file.clone(), span))
                                    .with_message(
                                        "expected a valid type after colon but reached end of file"
                                            .to_string(),
                                    )
                                    .with_color(ColorGenerator::new().next()),
                            )
                            .with_note(function_syntax())
                            .with_message(
                                "reached end of file while trying to parse a type annotation",
                            )
                            .finish(),
                    )));
                };
                let token = token.unwrap();
                match token {
                    Token::Variable(ref var) => {
                        let mut traits = vec![var.clone()];
                        // let r = Ok(TypeAnnot::Trait(vec!(var.clone())));
                        if matches!(self.tokens.peek(), Some((_, _))) {
                            if let Some((Ok(Token::Plus), _span)) = self.tokens.peek() {
                                self.tokens.next();
                            }
                            while let Some((Ok(Token::Variable(_)), _span)) = self.tokens.peek() {
                                let Some((Ok(Token::Variable(trait_name)), _span)) =
                                    self.tokens.next()
                                else {
                                    unreachable!()
                                };
                                traits.push(trait_name);
                            }
                        }
                        Ok(TypeAnnot::Trait(traits))
                    }
                    _ => Err((
                        ReportKind::Error,
                        Report::build(ReportKind::Error, (self.file.clone(), span.clone()))
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
                    )),
                }
            }
            Token::Variable(ref var) => {
                let ret = TypeAnnot::Boring(var.clone());
                let Some((token, _span)) = self.tokens.peek() else {
                    return Ok(ret);
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
                                return Err(Box::new((
                                    ReportKind::Error,
                                    Report::build(
                                        ReportKind::Error,
                                        (self.file.clone(), span.clone()),
                                    )
                                    .with_code("EOF")
                                    .with_label(
                                        Label::new((self.file.clone(), span.clone()))
                                            .with_message(
                                                "unexpected end of line while parsing generics",
                                            )
                                            .with_color(ColorGenerator::new().next()),
                                    )
                                    .with_message("unexpected end of file")
                                    .finish(),
                                )));
                            };
                            let span = span.clone();
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
                                    generics.push(self.parse_type_annotation(span).unwrap());
                                }
                            }
                        }
                        Ok(TypeAnnot::Generic(var.to_string(), generics))
                    }
                    _ => Ok(ret),
                }
            }
            _ => Err((
                ReportKind::Error,
                Report::build(ReportKind::Error, (self.file.clone(), span.clone()))
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
            )),
        };

        Ok(type_annot?)
    }
}
