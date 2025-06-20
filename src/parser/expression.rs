use crate::ast::BinOp;
use crate::ast::{AssignOp, Expr, UnOp};
use crate::lexer::Token;
use crate::parser::Parser;

use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind};
use yansi::Paint;

use std::ops::Range;

impl Parser<'_> {
    pub fn parse_expression(&mut self) -> (Expr, Range<usize>) {
        let Some((token, span_expression)) = self.tokens.next() else {
            self.errors.push((
                ReportKind::Error,
                Report::build(
                    ReportKind::Error,
                    (self.file.clone(), self.file.len()..self.file.len()),
                )
                .with_code("EOF")
                .with_label(
                    Label::new((self.file.clone(), self.file.len()..self.file.len()))
                        .with_message("expected a valid expression but reached end of file")
                        .with_color(ColorGenerator::new().next()),
                )
                .with_message("expected a valid expression but reached end of file while parsing.")
                .finish(),
            ));
            return (Expr::Error, self.file.len()..self.file.len());
        };

        let span_expression = span_expression.clone();

        let mut l_expr = match token.unwrap() {
            // literals
            Token::Int(i) => (Expr::Int(i), span_expression.clone()),
            Token::Float(f) => (Expr::Float(f), span_expression.clone()),
            Token::String(s) => (Expr::String(s), span_expression.clone()),
            Token::Bool(b) => (Expr::Bool(b), span_expression.clone()),

            Token::Variable(i) => (Expr::Variable(i), span_expression.clone()),

            Token::LParen => {
                let expr = self.parse_expression();
                let Some((token, span)) = self.tokens.next() else {
                    self.errors.push((
                        ReportKind::Error,
                        Report::build(
                            ReportKind::Error,
                            (self.file.clone(), span_expression.clone()),
                        )
                        .with_code("EOF")
                        .with_label(
                            Label::new((self.file.clone(), span_expression.clone()))
                                .with_message(
                                    "unclosed parenthesis. expected ')' but reached end of file",
                                )
                                .with_color(ColorGenerator::new().next()),
                        )
                        .with_message("reached end of file while parsing expression")
                        .finish(),
                    ));
                    return (Expr::Error, span_expression);
                };
                match token.unwrap() {
                    Token::RParen => expr,
                    Token::Comma => {
                        let mut exprs = vec![expr];
                        loop {
                            match self.tokens.next().unwrap().0.unwrap() {
                                Token::RParen => break,
                                _ => {
                                    exprs.push(self.parse_expression());
                                    match self.tokens.peek().unwrap().0.as_ref().unwrap() {
                                        Token::Comma => self.tokens.next(),
                                        Token::RParen => continue,

                                        _ => {
                                            self.errors.push((
                                                ReportKind::Error,
                                                Report::build(ReportKind::Error, (self.file.clone(), span.clone()))
                                                    .with_code("Syntax Error")
                                                    .with_label(
                                                        Label::new((self.file.clone(), span.clone()))
                                                            .with_message("invalid token. expected ',' or ')' in function call")
                                                            .with_color(ColorGenerator::new().next()),
                                                    )
                                                    .with_message("unexpected token in function call")
                                                    .finish(),
                                            ));
                                            return (Expr::Error, span.clone());
                                        }
                                    };
                                }
                            }
                        }
                        (Expr::Tuple(exprs), span)
                    }
                    _ => {
                        self.errors.push((
                            ReportKind::Error,
                            Report::build(ReportKind::Error, (self.file.clone(), span.clone()))
                                .with_code("Syntax Error")
                                .with_label(
                                    Label::new((self.file.clone(), span.clone()))
                                        .with_message("unclosed parenthesis")
                                        .with_color(ColorGenerator::new().next()),
                                )
                                .with_label(
                                    Label::new((self.file.clone(), span_expression.clone()))
                                        .with_message("expected ')' after expression")
                                        .with_color(ColorGenerator::new().next()),
                                )
                                .with_message("unclosed parenthesis")
                                .finish(),
                        ));
                        (Expr::Error, span)
                    }
                }
            }

            // keywords
            Token::KeywordLet => return self.parse_let(span_expression),
            Token::KeywordDo => return self.parse_do(span_expression),
            Token::KeywordIf => return self.parse_if(span_expression),

            // unary ops
            Token::Not => (
                Expr::UnOp {
                    expression: Box::new(self.parse_expression()),
                    unop: UnOp::Not,
                },
                span_expression.clone(),
            ),
            Token::Plus => (
                Expr::UnOp {
                    expression: Box::new(self.parse_expression()),
                    unop: UnOp::Plus,
                },
                span_expression.clone(),
            ),
            Token::Minus => (
                Expr::UnOp {
                    expression: Box::new(self.parse_expression()),
                    unop: UnOp::Minus,
                },
                span_expression.clone(),
            ),

            Token::LBracket => {
                let mut elements = vec![];
                let start_span = span_expression.clone();
                let mut end_span = span_expression.end..span_expression.end;

                // Check for empty array
                if let Some((token, _span)) = self.tokens.peek() {
                    if let Token::RBracket = token.as_ref().unwrap() {
                        let (_, span) = self.tokens.next().unwrap();
                        return (Expr::Array { elements }, start_span.start..span.end);
                    }
                }

                loop {
                    let expr = self.parse_expression();
                    elements.push((expr.0, expr.1.clone()));

                    // Check for comma or closing bracket
                    let Some((token, span)) = self.tokens.next() else {
                        self.errors.push((
                            ReportKind::Error,
                            Report::build(
                                ReportKind::Error,
                                (self.file.clone(), start_span.clone()),
                            )
                            .with_code("EOF")
                            .with_label(
                                Label::new((self.file.clone(), start_span.clone()))
                                    .with_message("unclosed array, expected ']'")
                                    .with_color(ColorGenerator::new().next()),
                            )
                            .with_message("unexpected end of file in array literal")
                            .finish(),
                        ));
                        return (Expr::Error, start_span);
                    };

                    match token.unwrap() {
                        Token::Comma => {
                            // Continue to next element
                        }
                        Token::RBracket => {
                            end_span = span;
                            break;
                        }
                        other => {
                            self.errors.push((
                                ReportKind::Error,
                                Report::build(ReportKind::Error, (self.file.clone(), span.clone()))
                                    .with_code("SyntaxError")
                                    .with_label(
                                        Label::new((self.file.clone(), span.clone()))
                                            .with_message(format!(
                                                "expected ',' or ']' after array element, found {}",
                                                Fmt::fg(format!("{:?}", other), Color::Red).bold()
                                            ))
                                            .with_color(ColorGenerator::new().next()),
                                    )
                                    .with_message("unexpected token in array literal")
                                    .finish(),
                            ));
                            return (Expr::Error, span);
                        }
                    }
                }

                (Expr::Array { elements }, start_span.start..end_span.end)
            }

            _x => {
                self.errors.push((
                    ReportKind::Error,
                    Report::build(
                        ReportKind::Error,
                        (self.file.clone(), span_expression.clone()),
                    )
                    .with_code("Syntax Error")
                    .with_label(
                        Label::new((self.file.clone(), span_expression.clone()))
                            .with_message("invalid token")
                            .with_color(ColorGenerator::new().next()),
                    )
                    .with_message("unexpected token while parsing expressions")
                    .finish(),
                ));
                return (Expr::Error, span_expression.clone());
            }
        };

        loop {
            // println!("next {:?}", self.tokens.peek());
            let Some((token, span_start)) = self.tokens.peek() else {
                return l_expr;
            };

            let span_start = span_start.clone();

            match token.as_ref().unwrap() {
                Token::LBracket => {
                    let Some((_, span_brack)) = self.tokens.next() else {
                        unreachable!()
                    };
                    let index = self.parse_expression();
                    let Some((token, span_expression)) = self.tokens.next() else {
                        self.errors.push((
                            ReportKind::Error,
                                Report::build(ReportKind::Error, (self.file.clone(), span_start.clone()))
                                    .with_code("EOF")
                                    .with_label(
                                        Label::new((self.file.clone(), span_start.clone()))
                                            .with_message("expected a closing bracket ']' after index. reached end of file")
                                            .with_color(ColorGenerator::new().next()),
                                    )
                                    .with_note(format!(
                                        "the syntax for indexing is 'your_expression{}your_index{}'",
                                        Fmt::fg("[", Color::Yellow).bold(),
                                        Fmt::fg("]", Color::Yellow).bold(),
                                    ))
                                    .with_message("Missing closing bracket. reached end of file while parsing value index.")
                                    .finish(),
                            ));
                        return (Expr::Error, span_brack.clone());
                    };
                    if let Token::RBracket = token.as_ref().unwrap() {
                        l_expr = (
                            Expr::Index {
                                array: Box::new(l_expr),
                                index: Box::new(index),
                            },
                            span_expression,
                        )
                    } else {
                        self.errors.push((
                            ReportKind::Error,
                            Report::build(
                                ReportKind::Error,
                                (self.file.clone(), span_expression.clone()),
                            )
                            .with_code("Syntax Error")
                            .with_label(
                                Label::new((self.file.clone(), span_expression.clone()))
                                    .with_message("expected a closing bracket ']' after index")
                                    .with_color(ColorGenerator::new().next()),
                            )
                            .with_label(
                                Label::new((self.file.clone(), span_start.clone()))
                                    .with_message("found opened bracket here")
                                    .with_color(ColorGenerator::new().next()),
                            )
                            .with_note(format!(
                                "the syntax for indexing is 'your_expression{}your_index{}'",
                                Fmt::fg("[", Color::Yellow).bold(),
                                Fmt::fg("]", Color::Yellow).bold(),
                            ))
                            .with_message("Unclosed bracket")
                            .finish(),
                        ));
                        return (Expr::Error, span_expression);
                    }
                } // index
                Token::LParen => {
                    self.tokens.next();
                    let mut args = vec![];
                    loop {
                        let Some((token, _span_expression)) = self.tokens.peek() else {
                            self.errors.push((
                                 ReportKind::Error,   Report::build(ReportKind::Error, (self.file.clone(), span_start.clone()))
                                        .with_code("EOF")
                                        .with_label(
                                            Label::new((self.file.clone(), span_start))
                                                .with_message("expected a closing parenthesis ')' or some arguments in function call. reached end of file")
                                                .with_color(ColorGenerator::new().next()),
                                        )
                                        .with_note(format!(
                                            "the syntax for calling a function is 'your_function{}your_arg1, your_arg1, ...{}'",
                                            Fmt::fg("(", Color::Yellow).bold(),
                                            Fmt::fg(")", Color::Yellow).bold(),
                                        ))
                                        .with_message("Missing closing parenthesis or expressions. reached end of file while parsing function call.")
                                        .finish(),
                                ));
                            return (Expr::Error, span_expression);
                        };
                        if token.clone().unwrap() == Token::RParen {
                            self.tokens.next();
                            break;
                        }
                        args.push(self.parse_expression());
                        let Some((token, span_expression)) = self.tokens.next() else {
                            self.errors.push((
                                ReportKind::Error,
                                    Report::build(ReportKind::Error, (self.file.clone(), span_start.clone()))
                                        .with_code("EOF")
                                        .with_label(
                                            Label::new((self.file.clone(), span_start))
                                                .with_message("expected a closing parenthesis ')' or some arguments in function call. reached end of file")
                                                .with_color(ColorGenerator::new().next()),
                                        )
                                        .with_note(format!(
                                            "the syntax for calling a function is 'your_function{}your_arg1, your_arg1, ...{}'",
                                            Fmt::fg("(", Color::Yellow).bold(),
                                            Fmt::fg(")", Color::Yellow).bold(),
                                        ))
                                        .with_message("Missing closing parenthesis or expressions. reached end of file while parsing function call.")
                                        .finish(),
                                ));
                            return (Expr::Error, span_expression);
                        };
                        match token.unwrap() {
                            Token::RParen => {
                                break;
                            }
                            Token::Comma => {
                                // self.tokens.next();
                            }
                            x => {
                                self.errors.push((
                                    ReportKind::Error,
                                        Report::build(ReportKind::Error, (self.file.clone(), span_start.clone()))
                                            .with_code("EOF")
                                            .with_label(
                                                Label::new((self.file.clone(), span_start))
                                                    .with_message("expected a closing parenthesis ')' or some arguments in function call. reached end of file")
                                                    .with_color(ColorGenerator::new().next()),
                                            )
                                            .with_note(format!(
                                                "the syntax for calling a function is 'your_function{}your_arg1, your_arg1, ...{}'",
                                                Fmt::fg("(", Color::Yellow).bold(),
                                                Fmt::fg(")", Color::Yellow).bold(),
                                            ))
                                            .with_message(format!("unexpected token. expected ')', found {:?}. ", x))
                                            .finish(),
                                    ));
                                return (Expr::Error, span_expression);
                            }
                        }
                    }
                    l_expr = (
                        Expr::Call {
                            function: Box::new(l_expr),
                            args,
                        },
                        span_expression.clone(),
                    );
                } // call

                Token::Dot => {
                    let Some((_, span_dot)) = self.tokens.next() else {
                        unreachable!()
                    }; // eat the dot
                    let Some((token, span)) = self.tokens.next() else {
                        self.errors.push((
                            ReportKind::Error,
                            Report::build(
                                ReportKind::Error,
                                (self.file.clone(), span_start.clone()),
                            )
                            .with_code("EOF")
                            .with_label(
                                Label::new((self.file.clone(), span_dot.clone()))
                                    .with_message("expected field name after '.'")
                                    .with_color(ColorGenerator::new().next()),
                            )
                            .with_message("unexpected end of file in field access")
                            .finish(),
                        ));
                        break;
                    };

                    // Destructure the current expression
                    let (expr_kind, expr_span) = l_expr;

                    if let Token::Variable(field_name) = token.unwrap() {
                        l_expr = (
                            Expr::StructAccess {
                                struct_val: Box::new((expr_kind, expr_span.clone())),
                                field_name,
                            },
                            // Span from the start of the struct expression to the end of the field name
                            expr_span.start..span.end,
                        );
                    } else {
                        self.errors.push((
                            ReportKind::Error,
                            Report::build(ReportKind::Error, (self.file.clone(), span.clone()))
                                .with_code("SyntaxError")
                                .with_label(
                                    Label::new((self.file.clone(), span.clone()))
                                        .with_message("expected field name after '.'")
                                        .with_color(ColorGenerator::new().next()),
                                )
                                .with_message("invalid field access")
                                .finish(),
                        ));
                        l_expr = (Expr::Error, span);
                    }
                }
                // assignments
                Token::Assign => {
                    let Some((_, span_op)) = self.tokens.next() else {
                        unreachable!()
                    };
                    l_expr = (
                        Expr::Assign {
                            assign_op: AssignOp::Assign,
                            l_value: Box::new(l_expr),
                            r_value: Box::new(self.parse_expression()),
                        },
                        span_op,
                    )
                }
                Token::AddAssign => {
                    let Some((_, span_op)) = self.tokens.next() else {
                        unreachable!()
                    };
                    l_expr = (
                        Expr::Assign {
                            assign_op: AssignOp::AddAssign,
                            l_value: Box::new(l_expr),
                            r_value: Box::new(self.parse_expression()),
                        },
                        span_op,
                    )
                }
                Token::SubAssign => {
                    let Some((_, span_op)) = self.tokens.next() else {
                        unreachable!()
                    };
                    l_expr = (
                        Expr::Assign {
                            assign_op: AssignOp::SubAssign,
                            l_value: Box::new(l_expr),
                            r_value: Box::new(self.parse_expression()),
                        },
                        span_op,
                    )
                }
                Token::MulAssign => {
                    let Some((_, span_op)) = self.tokens.next() else {
                        unreachable!()
                    };
                    l_expr = (
                        Expr::Assign {
                            assign_op: AssignOp::MulAssign,
                            l_value: Box::new(l_expr),
                            r_value: Box::new(self.parse_expression()),
                        },
                        span_op,
                    )
                }
                Token::DivAssign => {
                    let Some((_, span_op)) = self.tokens.next() else {
                        unreachable!()
                    };
                    l_expr = (
                        Expr::Assign {
                            assign_op: AssignOp::DivAssign,
                            l_value: Box::new(l_expr),
                            r_value: Box::new(self.parse_expression()),
                        },
                        span_op,
                    )
                }
                Token::ModAssign => {
                    let Some((_, span_op)) = self.tokens.next() else {
                        unreachable!()
                    };
                    l_expr = (
                        Expr::Assign {
                            assign_op: AssignOp::ModAssign,
                            l_value: Box::new(l_expr),
                            r_value: Box::new(self.parse_expression()),
                        },
                        span_op,
                    )
                }

                _ => {
                    break;
                }
            }
        }

        loop {
            let Some((token, _span_expression)) = self.tokens.peek() else {
                return l_expr;
            };

            match token.as_ref().unwrap() {
                Token::Plus => {
                    let Some((_, span_op)) = self.tokens.next() else {
                        unreachable!()
                    };
                    l_expr = (
                        Expr::BinOp {
                            l_value: Box::new(l_expr),
                            r_value: Box::new(self.parse_expression()),
                            operator: BinOp::Add,
                        },
                        span_op,
                    )
                }
                Token::Minus => {
                    let Some((_, span_op)) = self.tokens.next() else {
                        unreachable!()
                    };
                    l_expr = (
                        Expr::BinOp {
                            l_value: Box::new(l_expr),
                            r_value: Box::new(self.parse_expression()),
                            operator: BinOp::Sub,
                        },
                        span_op,
                    )
                }
                Token::Mul => {
                    let Some((_, span_op)) = self.tokens.next() else {
                        unreachable!()
                    };
                    l_expr = (
                        Expr::BinOp {
                            l_value: Box::new(l_expr),
                            r_value: Box::new(self.parse_expression()),
                            operator: BinOp::Mul,
                        },
                        span_op,
                    )
                }
                Token::Div => {
                    let Some((_, span_op)) = self.tokens.next() else {
                        unreachable!()
                    };
                    l_expr = (
                        Expr::BinOp {
                            l_value: Box::new(l_expr),
                            r_value: Box::new(self.parse_expression()),
                            operator: BinOp::Div,
                        },
                        span_op,
                    )
                }

                Token::Eq => {
                    let Some((_, span_op)) = self.tokens.next() else {
                        unreachable!()
                    };
                    l_expr = (
                        Expr::BinOp {
                            l_value: Box::new(l_expr),
                            r_value: Box::new(self.parse_expression()),
                            operator: BinOp::Eq,
                        },
                        span_op,
                    )
                }
                Token::NotEq => {
                    let Some((_, span_op)) = self.tokens.next() else {
                        unreachable!()
                    };
                    l_expr = (
                        Expr::BinOp {
                            l_value: Box::new(l_expr),
                            r_value: Box::new(self.parse_expression()),
                            operator: BinOp::Add,
                        },
                        span_op,
                    )
                }
                Token::GreaterEq => {
                    let Some((_, span_op)) = self.tokens.next() else {
                        unreachable!()
                    };
                    l_expr = (
                        Expr::BinOp {
                            l_value: Box::new(l_expr),
                            r_value: Box::new(self.parse_expression()),
                            operator: BinOp::GreaterEq,
                        },
                        span_op,
                    )
                }
                Token::LessEq => {
                    let Some((_, span_op)) = self.tokens.next() else {
                        unreachable!()
                    };
                    l_expr = (
                        Expr::BinOp {
                            l_value: Box::new(l_expr),
                            r_value: Box::new(self.parse_expression()),
                            operator: BinOp::LessEq,
                        },
                        span_op,
                    )
                }

                Token::Or => {
                    let Some((_, span_op)) = self.tokens.next() else {
                        unreachable!()
                    };
                    l_expr = (
                        Expr::BinOp {
                            l_value: Box::new(l_expr),
                            r_value: Box::new(self.parse_expression()),
                            operator: BinOp::Or,
                        },
                        span_op,
                    )
                }
                Token::Xor => {
                    let Some((_, span_op)) = self.tokens.next() else {
                        unreachable!()
                    };
                    l_expr = (
                        Expr::BinOp {
                            l_value: Box::new(l_expr),
                            r_value: Box::new(self.parse_expression()),
                            operator: BinOp::Xor,
                        },
                        span_op,
                    )
                }
                Token::And => {
                    let Some((_, span_op)) = self.tokens.next() else {
                        unreachable!()
                    };
                    l_expr = (
                        Expr::BinOp {
                            l_value: Box::new(l_expr),
                            r_value: Box::new(self.parse_expression()),
                            operator: BinOp::And,
                        },
                        span_op,
                    )
                }
                Token::Nor => {
                    let Some((_, span_op)) = self.tokens.next() else {
                        unreachable!()
                    };
                    l_expr = (
                        Expr::BinOp {
                            l_value: Box::new(l_expr),
                            r_value: Box::new(self.parse_expression()),
                            operator: BinOp::Nor,
                        },
                        span_op,
                    )
                }

                _x => break,
            }
        }

        l_expr
    }

    pub fn parse_let(&mut self, span: Range<usize>) -> (Expr, Range<usize>) {
        // `let` has already been "eaten"

        let Some((token, var_span)) = self.tokens.next() else {
            self.errors.push((
                ReportKind::Error,
                Report::build(ReportKind::Error, (self.file.clone(), span.clone()))
                    .with_code("EOF")
                    .with_label(
                        Label::new((self.file.clone(), span.clone()))
                            .with_message("expected a variable after 'let'. reached end of file")
                            .with_color(ColorGenerator::new().next()),
                    )
                    .with_note(format!(
                        "the syntax for 'let' is '{} your_variable = your_expression'",
                        Fmt::fg("let", Color::Yellow).bold()
                    ))
                    .with_message("reached end of file while parsing `let` expression")
                    .finish(),
            ));
            return (Expr::Error, span.clone());
        };
        if let Token::Variable(identifier) = token.unwrap() {
            let Some((token, span)) = self.tokens.next() else {
                self.errors.push((
                    ReportKind::Error,
                    Report::build(ReportKind::Error, (self.file.clone(), span.clone()))
                        .with_code("EOF")
                        .with_label(
                            Label::new((self.file.clone(), span.clone()))
                                .with_message("expected ':' or =' after variable in let expression. reached unexpected end of file.")
                                .with_color(ColorGenerator::new().next()),
                        )
                        .with_note(format!(
                            "the syntax for 'let' is '{} your_variable = your_expression'",
                            Fmt::fg("let", Color::Yellow).bold()
                        ))
                        .with_message("reached end of file while parsing `let` expression")
                        .finish(),
                ));
                return (Expr::Error, span.clone());
            }; // eat '='
            let mut type_annot = None;
            match token.unwrap() {
                Token::Colon => {
                    type_annot = Some((self.parse_type_annotation(span.clone()).unwrap(), span))
                }
                Token::Assign => {}
                _ => {
                    self.errors.push((
                        ReportKind::Error,
                        Report::build(ReportKind::Error, (self.file.clone(), span.clone()))
                            .with_code("Syntax Error")
                            .with_label(
                                Label::new((self.file.clone(), span.clone()))
                                    .with_message(
                                        "invalid token. expected ':' or '=' in let expression",
                                    )
                                    .with_color(ColorGenerator::new().next()),
                            )
                            .with_message("unexpected token in let expression")
                            .finish(),
                    ));
                    return (Expr::Error, span.clone());
                }
            }
            let (expr, expr_span) = self.parse_expression();
            (
                Expr::Let {
                    var: identifier,
                    type_annot,
                    value: Box::new((expr, expr_span.clone())),
                },
                var_span.start..expr_span.end,
            )
        } else {
            let mut colors = ColorGenerator::new();
            let a = colors.next();
            let b = colors.next();
            let _c = colors.next();

            self.errors.push((
                ReportKind::Error,
                Report::build(ReportKind::Error, (self.file.clone(), span.clone()))
                    .with_code("Syntax Error")
                    .with_label(
                        Label::new((self.file.clone(), var_span))
                            .with_message("unexpected token.")
                            .with_color(a),
                    )
                    .with_label(
                        Label::new((self.file.clone(), span.clone()))
                            .with_message("expected variable name after 'let'")
                            .with_color(b),
                    )
                    .with_note(format!(
                        "the syntax for 'let' is '{} your_variable = your_expression'",
                        Fmt::fg("let", Color::Yellow).bold()
                    ))
                    .with_message("unexpected token after 'let'. expected variable name")
                    .finish(),
            ));
            (Expr::Error, span)
        }
    }

    pub fn parse_do(&mut self, span: Range<usize>) -> (Expr, Range<usize>) {
        // `do` has already been 'eaten'
        // let mut has_ended = false;
        let mut expressions = vec![];

        let Some((token, span_expression)) = self.tokens.peek() else {
            self.errors.push((
                ReportKind::Error,
                Report::build(ReportKind::Error, (self.file.clone(), span.clone()))
                    .with_code("EOF")
                    .with_label(
                        Label::new((self.file.clone(), span.clone()))
                            .with_message("expected a valid expression but reached end of file")
                            .with_color(ColorGenerator::new().next()),
                    )
                    .with_message("reached end of file while parsing `do` expression")
                    .finish(),
            ));
            return (Expr::Error, span);
        };

        if let Token::KeywordEnd = token.as_ref().unwrap() {
            self.errors.push((
                ReportKind::Error,
                Report::build(ReportKind::Error, (self.file.clone(), span.clone()))
                    .with_code("Syntax Error")
                    .with_label(
                        Label::new((self.file.clone(), span.clone()))
                            .with_message("Empty `do` expression found")
                            .with_color(ColorGenerator::new().next()),
                    )
                    .with_label(
                        Label::new((self.file.clone(), span_expression.clone()))
                            .with_message(
                                "Found `end` expression here with no expressions in between",
                            )
                            .with_color(ColorGenerator::new().next()),
                    )
                    .with_message("Empty `do` expressions are not allowed")
                    .finish(),
            ));
            let Some((_, span)) = self.tokens.next() else {
                self.errors.push((
                    ReportKind::Error,
                    Report::build(ReportKind::Error, (self.file.clone(), span.clone()))
                        .with_code("EOF")
                        .with_label(
                            Label::new((self.file.clone(), span.clone()))
                                .with_message(
                                    "expected 'end' after expressions in 'do'. reached end of file",
                                )
                                .with_color(ColorGenerator::new().next()),
                        )
                        .with_message("reached end of file while parsing `do` expression")
                        .finish(),
                ));
                return (Expr::Error, span.clone());
            }; // eat 'end'
            return (Expr::Error, span);
        }

        let mut span_expression = span_expression.clone();

        loop {
            let Some((token, span_expression_inside)) = self.tokens.peek() else {
                self.errors.push((
                    ReportKind::Error,
                    Report::build(
                        ReportKind::Error,
                        (self.file.clone(), span_expression.clone()),
                    )
                    .with_code("EOF")
                    .with_label(
                        Label::new((self.file.clone(), span_expression.clone()))
                            .with_message("expected a valid expression but reached end of file")
                            .with_color(ColorGenerator::new().next()),
                    )
                    .with_message("reached end of file while parsing expression")
                    .finish(),
                ));
                return (Expr::Error, span_expression);
            };

            span_expression = span_expression_inside.clone();
            // println!("{:?}", span_expression);

            match token.clone().unwrap() {
                Token::Semicolon => {
                    // Consume empty semicolons
                    self.tokens.next();
                    continue;
                }
                Token::KeywordEnd => {
                    return {
                        self.tokens.next();

                        (Expr::Do { expressions }, span_expression)
                    };
                }
                _ => expressions.push(self.parse_expression()),
            }
        }
    }

    pub fn parse_if(&mut self, span: Range<usize>) -> (Expr, Range<usize>) {
        let condition = Box::new(self.parse_expression());
        let Some((_token, then_span)) = self.tokens.next()
        // eat 'then'
        else {
            self.errors.push((
                    ReportKind::Error,
                    Report::build(ReportKind::Error, (self.file.clone(), span.clone()))
                        .with_code("EOF")
                        .with_label(
                            Label::new((self.file.clone(), span.clone()))
                                .with_message("expected 'then' after 'if' condition. reached end of file")
                                .with_color(ColorGenerator::new().next()),
                        )
                        .with_note(format!(
                            "the syntax for 'if' is '{} condition {} your_expression'",
                            Fmt::fg("if", Color::Yellow).bold(),
                            Fmt::fg("then", Color::Yellow).bold(),

                        ))
                        .with_note(format!(
                            "the syntax for 'if-else' is '{} condition {} your_if_expression {} your_else_expression'",
                            Fmt::fg("if", Color::Yellow).bold(),
                            Fmt::fg("then", Color::Yellow).bold(),
                            Fmt::fg("else", Color::Yellow).bold(),
                        ))
                        .with_message("reached end of file while parsing `if` expression. Expected 'then'")
                        .finish(),
                ));
            return (Expr::Error, span);
        };
        let if_branch = Box::new(self.parse_expression());
        let Some((token, _then_span)) = self.tokens.peek()
        // eat 'else'
        else {
            return (
                Expr::IfElse {
                    condition,
                    if_branch,
                    else_branch: None,
                },
                then_span,
            );
        };
        if let Token::KeywordElse = token.as_ref().unwrap() {
            let Some((_, span)) = self.tokens.next() else {
                unreachable!()
            };
            (
                Expr::IfElse {
                    condition,
                    if_branch,
                    else_branch: Some(Box::new(self.parse_expression())),
                },
                span.clone(),
            )
        } else {
            (
                Expr::IfElse {
                    condition,
                    if_branch,
                    else_branch: None,
                },
                span.clone(),
            )
        }
    }
}
