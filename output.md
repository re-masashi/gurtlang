```rs
// lib.rs
pub mod ast;
pub mod lexer;
pub mod parser;
pub mod typechecker;

```

```rs
// main.rs
use gurtlang::lexer::Token;
use gurtlang::parser::Parser;
use gurtlang::typechecker::TypeEnv;

use logos::Logos;

use std::fs;

fn main() {
    let filepath = "examples/5.gurt".to_string();

    let contents =
        fs::read_to_string(&filepath).expect("Should have been able to read the file :/");

    let lexer = Token::lexer(&contents).spanned().peekable();
    let mut parser = Parser::new(lexer, filepath.clone());

    let ast = parser.parse_program();
    // println!("{:#?}", ast);

    if parser.report_errors() {
        panic!("cant continue");
    };

    let mut type_env = TypeEnv::new(filepath);
    let typed_ast = type_env.ast_to_typed_ast(ast);

    if type_env.report_errors() {
        panic!("cant continue. type errors");
    };

    let resolved_ast = type_env.resolve_all(typed_ast);
    let mono_ast = type_env.monomorphize_ast(resolved_ast);

    println!("{:#?}", mono_ast);

    println!("Me: Yogurt");
    println!("Gurt: Yo");
}

```

```rs
// ast/mod.rs
use std::ops::Range;
use std::sync::Arc;
use std::vec::Vec;

#[derive(Debug)]
pub enum ASTNode {
    Expr((Expr, Range<usize>)),
    Function(Function),
    Struct(Struct),
    Error, // dummy node for error recovery
}

#[derive(Debug)]
pub enum Expr {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),

    Variable(String),

    Array {
        elements: Vec<(Expr, Range<usize>)>,
    },

    Index {
        array: Box<(Expr, Range<usize>)>,
        index: Box<(Expr, Range<usize>)>,
    },

    Call {
        function: Box<(Expr, Range<usize>)>,
        args: Vec<(Expr, Range<usize>)>,
    },

    StructAccess {
        struct_val: Box<(Expr, Range<usize>)>,
        field_name: String,
    },

    MethodCall {
        struct_val: Box<(Expr, Range<usize>)>,
        method_name: String,
        args: Vec<(Expr, Range<usize>)>,
    },

    BinOp {
        operator: BinOp,
        l_value: Box<(Expr, Range<usize>)>,
        r_value: Box<(Expr, Range<usize>)>,
    },

    Assign {
        l_value: Box<(Expr, Range<usize>)>,
        r_value: Box<(Expr, Range<usize>)>,
        assign_op: AssignOp,
    },

    UnOp {
        unop: UnOp,
        expression: Box<(Expr, Range<usize>)>,
    },

    Do {
        expressions: Vec<(Expr, Range<usize>)>,
    },

    Let {
        var: String,
        type_annot: Option<(TypeAnnot, Range<usize>)>,
        value: Box<(Expr, Range<usize>)>,
    },

    IfElse {
        condition: Box<(Expr, Range<usize>)>,
        if_branch: Box<(Expr, Range<usize>)>,
        else_branch: Option<Box<(Expr, Range<usize>)>>,
    },

    Tuple(Vec<(Expr, Range<usize>)>),

    Error, // dummy node for error recovery
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Power,

    Eq,
    NotEq,
    Less,
    Greater,
    LessEq,
    GreaterEq,

    And,
    Or,
    Xor,
    Nor,
}

#[derive(Debug, Clone)]
pub enum UnOp {
    Plus,
    Minus,
    Not,
}

#[derive(Debug, Clone)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub args: Vec<(String, Option<TypeAnnot>, Range<usize>)>,
    pub body: Box<(Expr, Range<usize>)>,
    pub return_type: Option<(TypeAnnot, Range<usize>)>,
}

#[derive(Debug)]
pub struct Struct {
    pub name: (String, Range<usize>),
    pub generics: Vec<(String, Range<usize>)>, // only boring types?
    pub fields: Vec<(String, TypeAnnot, Range<usize>)>,
}

#[derive(Debug)]
pub enum TypeAnnot {
    Bool,
    Int,
    Float,
    String,
    Boring(String),
    Generic(String, Vec<TypeAnnot>),
    Union(Vec<TypeAnnot>),
    Function {
        params: Vec<TypeAnnot>,
        return_type: Box<TypeAnnot>,
    },
    Tuple(Vec<TypeAnnot>),
    Trait(String),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Constructor {
        name: String,
        generics: Vec<Arc<Type>>,
        traits: Vec<String>,
    },
    Variable(usize),
    Trait(String),
    Function {
        params: Vec<Arc<Type>>,
        return_type: Arc<Type>,
    },
    Tuple(Vec<Arc<Type>>),
    Union(Vec<Arc<Type>>),
    // GenericParam(String), // JUST FOR STRUCTS AND MAYBE ENUMS LATER
    Unit,
}

#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub kind: TypedExprKind,
    pub ty: Arc<Type>,
    pub range: Range<usize>,
}

#[derive(Debug, Clone)]
pub enum TypedExprKind {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),

    Variable(String),

    Array {
        elements: Vec<TypedExpr>,
    },

    Index {
        array: Box<TypedExpr>,
        index: Box<TypedExpr>,
    },

    Call {
        function: Box<TypedExpr>,
        args: Vec<TypedExpr>,
    },

    StructAccess {
        struct_val: Box<TypedExpr>,
        field_name: String,
    },

    MethodCall {
        struct_val: Box<TypedExpr>,
        method_name: String,
        args: Vec<TypedExpr>,
    },

    BinOp {
        operator: BinOp,
        l_value: Box<TypedExpr>,
        r_value: Box<TypedExpr>,
    },
    Assign {
        l_value: Box<TypedExpr>,
        r_value: Box<TypedExpr>,
        assign_op: AssignOp,
    },

    UnOp {
        unop: UnOp,
        expression: Box<TypedExpr>,
    },

    Do {
        expressions: Vec<TypedExpr>,
    },

    Let {
        var: String,
        value: Box<TypedExpr>,
    },

    IfElse {
        condition: Box<TypedExpr>,
        if_branch: Box<TypedExpr>,
        else_branch: Option<Box<TypedExpr>>,
    },

    Tuple(Vec<TypedExpr>),

    Error, // dummy node for error recovery
}

#[derive(Debug)]
pub enum TypedASTNode {
    Expr((TypedExpr, Range<usize>)),
    Function((TypedFunction, Range<usize>)),
    Struct((TypedStruct, Range<usize>)),
    Error, // dummy node for error recovery
}

#[derive(Debug, Clone)]
pub struct TypedFunction {
    pub name: String,
    pub args: Vec<(String, Arc<Type>, Range<usize>)>,
    pub body: Box<(TypedExpr, Range<usize>)>,
    pub return_type: (Arc<Type>, Range<usize>),
}

#[derive(Debug)]
pub struct TypedStruct {
    pub name: String,
    pub generics: Vec<(String, Range<usize>)>, // only boring types?
    pub fields: Vec<(String, Arc<Type>, Range<usize>)>,
}

impl TypedFunction {
    /// Checks if a function is generic (contains type variables)
    pub fn is_generic(&self) -> bool {
        self.args
            .iter()
            .any(|(_, ty, _)| matches!(&**ty, Type::Variable(_)))
            || matches!(&*self.return_type.0, Type::Variable(_))
    }
}

```

```rs
// typechecker/mod.rs
pub mod expression;
pub mod function;
pub mod monomorphize;
pub mod resolve;
pub mod struct_;
pub mod unify;

use ariadne::{Report, ReportKind, Source};

use std::collections::HashMap;
use std::fs;
use std::ops::Range;
use std::sync::Arc;

use crate::ast::{Type, TypeAnnot};
use crate::tvar;

type Error<'a> = (ReportKind<'a>, Report<'a, (String, Range<usize>)>);

#[derive(Debug)]
pub struct StructTy {
    pub name: String,
    pub generics: Vec<String>,
    pub fields: Vec<(String, Arc<Type>)>,
}

#[derive(Debug)]
pub struct TypeEnv<'a> {
    pub variables: HashMap<String, Arc<Type>>,
    pub structs: HashMap<String, Arc<StructTy>>,
    pub tvar_count: usize,
    substitutions: HashMap<usize, Arc<Type>>,
    errors: Vec<Error<'a>>,
    file: String,
}

impl TypeEnv<'_> {
    pub fn new(file: String) -> Self {
        TypeEnv {
            variables: HashMap::new(),
            structs: HashMap::new(),
            errors: vec![],
            substitutions: HashMap::new(),
            tvar_count: 0,
            file,
        }
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

    pub fn new_typevar(&mut self) -> Arc<Type> {
        self.tvar_count += 1;
        tvar!(self.tvar_count)
    }

    pub fn insert_var(&mut self, var: String, ty: Arc<Type>) {
        self.variables.insert(var, ty);
    }

    pub fn get_var(&mut self, var: &String) -> Option<Arc<Type>> {
        self.variables.get(var).cloned()
    }

    pub fn var_exists(&mut self, var: &String) -> bool {
        self.variables.contains_key(var)
    }

    pub fn insert_struct(&mut self, struct_name: String, struct_: Arc<StructTy>) {
        self.structs.insert(struct_name, struct_);
    }

    pub fn get_struct(&mut self, struct_: &String) -> Option<Arc<StructTy>> {
        self.structs.get(struct_).cloned()
    }

    pub fn struct_exists(&mut self, struct_: &String) -> bool {
        self.structs.contains_key(struct_)
    }
}

#[macro_export]
macro_rules! tvar {
    ($i:expr) => {
        Arc::new(Type::Variable($i))
    };
}

#[macro_export]
macro_rules! t_unit {
    () => {
        Arc::new(Type::Unit)
    };
}

#[macro_export]
macro_rules! t_int {
    () => {
        Arc::new(Type::Constructor {
            name: "int".to_string(),
            generics: vec![],
            traits: vec![
                "Number".to_string(),
                "Printable".to_string(),
                "Simple".to_string(),
            ],
        })
    };
}

#[macro_export]
macro_rules! t_bool {
    () => {
        Arc::new(Type::Constructor {
            name: "bool".to_string(),
            generics: vec![],
            traits: vec![
                "Number".to_string(),
                "Printable".to_string(),
                "Simple".to_string(),
            ],
        })
    };
}

#[macro_export]
macro_rules! t_float {
    () => {
        Arc::new(Type::Constructor {
            name: "float".to_string(),
            generics: vec![],
            traits: vec![
                "Number".to_string(),
                "Printable".to_string(),
                "Simple".to_string(),
            ],
        })
    };
}

#[macro_export]
macro_rules! t_string {
    () => {
        Arc::new(Type::Constructor {
            name: "string".to_string(),
            generics: vec![],
            traits: vec!["Printable".to_string(), "Simple".to_string()],
        })
    };
}

#[macro_export]
macro_rules! t_list {
    ($t: expr) => {
        Arc::new(Type::Constructor {
            name: "List".to_string(),
            generics: vec![$t],
            traits: vec!["Iterable".to_string()],
        })
    };
}

fn type_string(ty: &Type) -> String {
    match ty {
        Type::Variable(i) => format!("?T{}", i),
        Type::Constructor { name, generics, .. } if generics.is_empty() => name.clone(),
        Type::Constructor { name, generics, .. } => {
            let generics = generics
                .iter()
                .map(|t| type_string(t))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}<{}>", name, generics)
        }
        Type::Function {
            params,
            return_type,
        } => {
            let params = params
                .iter()
                .map(|t| type_string(t))
                .collect::<Vec<_>>()
                .join(", ");
            format!("({}) -> {}", params, type_string(return_type))
        }
        _ => format!("{:?}", ty),
    }
}

pub fn type_annot_to_type(type_annot: &TypeAnnot) -> Arc<Type> {
    match type_annot {
        TypeAnnot::Bool => t_bool!(),
        TypeAnnot::Int => t_int!(),
        TypeAnnot::Float => t_float!(),
        TypeAnnot::String => t_string!(),
        TypeAnnot::Boring(name) => Arc::new(Type::Constructor {
            name: name.to_string(),
            generics: vec![],
            traits: vec![],
        }),
        TypeAnnot::Generic(name, generics) => {
            let generics = generics.iter().map(type_annot_to_type).collect::<Vec<_>>();
            Arc::new(Type::Constructor {
                name: name.clone(),
                generics: generics.clone(),
                traits: vec![],
            })
        }
        TypeAnnot::Union(unions) => {
            let unions = unions.iter().map(type_annot_to_type).collect::<Vec<_>>();
            Arc::new(Type::Union(unions))
        }
        TypeAnnot::Function {
            params,
            return_type,
        } => {
            let params = params.iter().map(type_annot_to_type).collect::<Vec<_>>();
            let return_type = type_annot_to_type(return_type);
            Arc::new(Type::Function {
                params,
                return_type,
            })
        }
        TypeAnnot::Tuple(tuple) => {
            let tuple = tuple.iter().map(type_annot_to_type).collect::<Vec<_>>();
            Arc::new(Type::Tuple(tuple))
        }
        TypeAnnot::Trait(name) => Arc::new(Type::Trait(name.clone())),
    }
}

```

```rs
// typechecker/function.rs
#![allow(unused_imports)]

use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    ASTNode, AssignOp, BinOp, Expr, Function, Struct, Type, TypeAnnot, TypedASTNode, TypedExpr,
    TypedExprKind, TypedFunction, TypedStruct,
};
use crate::typechecker::{TypeEnv, type_annot_to_type};
use crate::{t_bool, t_float, t_int, t_list, t_string, t_unit, tvar};

impl TypeEnv<'_> {
    pub fn function_to_typed_function(
        &mut self,
        function: (&Function, &Range<usize>),
    ) -> (TypedFunction, Range<usize>) {
        let (function, fun_span) = function;
        let Function {
            name,
            args,
            body,
            return_type,
        } = function;
        let typed_args = args
            .iter()
            .map(|(argname, typeannot, range)| match typeannot {
                Some(ty) => (argname.to_string(), type_annot_to_type(ty), range.clone()),
                None => {
                    let new_typevar = self.new_typevar();
                    self.insert_var(argname.clone(), new_typevar.clone());
                    (argname.to_string(), new_typevar, range.clone())
                }
            })
            .collect::<Vec<_>>();
        let (body, body_span) = &**body;
        let typed_body = (
            self.expr_to_typed_expr((body, body_span)),
            body_span.clone(),
        );

        let typed_return_type = match return_type {
            Some((return_type, span)) => (type_annot_to_type(return_type), span.clone()),
            None => (typed_body.0.ty.clone(), typed_body.0.range.clone()),
        };
        let function_type = Arc::new(Type::Function {
            params: typed_args.iter().map(|(_, ty, _)| ty.clone()).collect(),
            return_type: typed_return_type.0.clone(),
        });
        self.insert_var(name.clone(), function_type.clone());
        (
            TypedFunction {
                args: typed_args,
                body: Box::new(typed_body),
                name: name.to_string(),
                return_type: typed_return_type,
            },
            fun_span.clone(),
        )
    }
}

```

```rs
// typechecker/struct_.rs
#![allow(unused_imports)]

use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    ASTNode, AssignOp, BinOp, Expr, Function, Struct, Type, TypeAnnot, TypedASTNode, TypedExpr,
    TypedExprKind, TypedFunction, TypedStruct,
};
use crate::typechecker::{StructTy, TypeEnv, type_annot_to_type};
use crate::{t_bool, t_float, t_int, t_list, t_string, t_unit, tvar};

impl TypeEnv<'_> {
    pub fn struct_to_typed_struct(
        &mut self,
        struct_: (&Struct, &Range<usize>),
    ) -> (TypedStruct, Range<usize>) {
        let (struct_, _span) = struct_;
        let Struct {
            name,
            generics,
            fields,
        } = struct_;

        let last_field_span = match fields.last() {
            Some(f) => &f.2,
            None => &name.1,
        };

        // Create a mapping of generic parameter names
        let generic_names: Vec<_> = generics.iter().map(|(name, _)| name.clone()).collect();

        let typed_fields = fields
            .iter()
            .map(|(field_name, type_annot, range)| {
                // Use special conversion that handles generic params
                let ty = self.convert_type_annot(type_annot, &generic_names);
                (field_name.to_string(), ty, range.clone())
            })
            .collect::<Vec<_>>();

        let struct_ty = Arc::new(StructTy {
            name: name.0.clone(),
            generics: generic_names.clone(),
            fields: typed_fields
                .iter()
                .map(|(name, ty, _)| (name.clone(), ty.clone()))
                .collect(),
        });

        self.insert_struct(name.0.clone(), struct_ty);

        // Create constructor function type
        let return_type = Arc::new(Type::Constructor {
            name: name.0.clone(),
            generics: generics.iter().map(|_| self.new_typevar()).collect(),
            traits: vec![],
        });

        let function_type = Arc::new(Type::Function {
            params: typed_fields.iter().map(|(_, ty, _)| ty.clone()).collect(),
            return_type: (return_type),
        });

        self.insert_var(name.0.clone(), function_type.clone()); // constructor

        (
            TypedStruct {
                name: name.0.clone(),
                generics: generics.clone(),
                fields: typed_fields,
            },
            name.1.start..last_field_span.end,
        )
    }

    fn convert_type_annot(&self, type_annot: &TypeAnnot, _generic_names: &[String]) -> Arc<Type> {
        match type_annot {
            _ => type_annot_to_type(type_annot),
        }
    }
}

```

```rs
// typechecker/expression.rs
#![allow(unused_imports)]

use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    ASTNode, AssignOp, BinOp, Expr, Function, Struct, Type, TypeAnnot, TypedASTNode, TypedExpr,
    TypedExprKind, TypedFunction, TypedStruct, UnOp,
};
use crate::typechecker::{TypeEnv, type_annot_to_type};
use crate::{t_bool, t_float, t_int, t_list, t_string, t_unit, tvar};

impl TypeEnv<'_> {
    pub fn ast_to_typed_ast(&mut self, ast: Vec<(ASTNode, Range<usize>)>) -> Vec<TypedASTNode> {
        ast.into_iter()
            .map(|(node, span)| self.node_to_typed_node(node, &span))
            .collect()
    }

    pub fn node_to_typed_node(&mut self, node: ASTNode, span: &Range<usize>) -> TypedASTNode {
        match node {
            ASTNode::Error => unreachable!(),
            ASTNode::Expr(expr) => {
                let (expr, span) = expr;
                let typed_expr = self.expr_to_typed_expr((&expr, &span));
                TypedASTNode::Expr((typed_expr, span))
            }
            ASTNode::Function(func) => {
                let (typed_fun, span) = self.function_to_typed_function((&func, span));
                TypedASTNode::Function((typed_fun, span))
            }
            ASTNode::Struct(struct_) => {
                let dummy_span = 0..0; // Dummy span since structs don't have spans
                let (typed_struct, span) = self.struct_to_typed_struct((&struct_, &dummy_span));
                TypedASTNode::Struct((typed_struct, span))
            }
        }
    }

    pub fn expr_to_typed_expr(&mut self, expr: (&Expr, &Range<usize>)) -> TypedExpr {
        let (expr, span) = expr;
        let (exprkind, ty) = match expr {
            Expr::Bool(b) => (TypedExprKind::Bool(*b), t_bool!()),
            Expr::Int(i) => (TypedExprKind::Int(*i), t_int!()),
            Expr::Float(f) => (TypedExprKind::Float(*f), t_float!()),
            Expr::String(s) => (TypedExprKind::String(s.to_string()), t_string!()),
            Expr::Variable(name) => {
                let ty = if self.var_exists(name) {
                    self.get_var(name).unwrap()
                } else {
                    let new_typevar = self.new_typevar();
                    self.insert_var(name.clone(), new_typevar.clone());
                    new_typevar
                };
                (TypedExprKind::Variable(name.to_string()), ty)
            }
            Expr::Array { elements } => {
                if elements.is_empty() {
                    let new_typevar = self.new_typevar();

                    (
                        TypedExprKind::Array { elements: vec![] },
                        t_list!(new_typevar),
                    )
                } else if elements.len() == 1 {
                    let (elem, span) = &elements[0];
                    let new_elem = self.expr_to_typed_expr((elem, span));
                    let ty = new_elem.ty.clone();
                    (
                        TypedExprKind::Array {
                            elements: vec![new_elem],
                        },
                        ty.clone(),
                    )
                } else {
                    let (first_elem, first_span) = &elements[0];
                    let new_first_elem = self.expr_to_typed_expr((first_elem, first_span));

                    let val_ty = new_first_elem.ty.clone();
                    let val_span = new_first_elem.range.clone();
                    let mut new_elems = vec![new_first_elem];

                    if let Some(elem) = elements.get(1) {
                        let (elem, span) = elem;
                        let typed = self.expr_to_typed_expr((elem, span));
                        let _ =
                            self.unify(val_ty.clone(), typed.ty.clone(), &val_span, &typed.range)
                                || todo!("AAAA! INVALID ARRAY ELEM");
                        new_elems.push(typed);

                        for (elem, span) in elements.iter().skip(2) {
                            // let (elem, span) = &elements[i];
                            let typed = self.expr_to_typed_expr((elem, span));
                            let _ = self.unify(
                                val_ty.clone(),
                                typed.ty.clone(),
                                &val_span,
                                &typed.range,
                            ) || todo!("AAAA! INVALID ARRAY ELEM");
                            new_elems.push(typed);
                        }
                    }

                    let ty = &new_elems[0].ty.clone();
                    (
                        TypedExprKind::Array {
                            elements: new_elems,
                        },
                        ty.clone(),
                    )
                }
            }
            Expr::Index { array, index } => {
                // TODO: add custom indexing through traits
                let (expr, _span) = &**array;
                let (index, span) = &**index;

                let typed_array = self.expr_to_typed_expr((expr, span));
                let typed_index = self.expr_to_typed_expr((index, span));

                match &*typed_array.ty.clone() {
                    Type::Constructor {
                        name,
                        generics,
                        traits: _,
                    } if name == "List" => (
                        TypedExprKind::Index {
                            array: Box::new(typed_array),
                            index: Box::new(typed_index),
                        },
                        generics[0].clone(),
                    ),
                    // Type::Constructor { name, .. } if name == "string" => {
                    //     (t_string!())
                    // }
                    _ => todo!("bs. who would even index sumn that's not an array."),
                }
            }
            Expr::Call { function, args } => {
                let (fun, span) = &**function;
                let typed_fun = self.expr_to_typed_expr((fun, span));

                match &*typed_fun.ty.clone() {
                    Type::Variable(_) => {
                        let new_args = args
                            .iter()
                            .map(|arg| {
                                let (arg, span) = arg;
                                self.expr_to_typed_expr((arg, span))
                            })
                            .collect::<Vec<_>>();
                        let ty = typed_fun.ty.clone();
                        (
                            TypedExprKind::Call {
                                function: Box::new(typed_fun),
                                args: new_args,
                            },
                            ty,
                        )
                    }
                    Type::Function {
                        params: _,
                        return_type,
                    } => {
                        let new_args = args
                            .iter()
                            .map(|arg| {
                                let (arg, span) = arg;
                                self.expr_to_typed_expr((arg, span))
                            })
                            .collect::<Vec<_>>();
                        (
                            TypedExprKind::Call {
                                function: Box::new(typed_fun),
                                args: new_args,
                            },
                            return_type.clone(),
                        )
                    }
                    _ => todo!(),
                }
            }
            Expr::StructAccess {
                struct_val: _,
                field_name: _,
            } => {
                todo!()
            }
            Expr::MethodCall { .. } => todo!(), // needs structs implemented
            Expr::BinOp {
                operator,
                l_value,
                r_value,
            } => {
                let (l_expr, l_span) = &**l_value;
                let l_value_typed = self.expr_to_typed_expr((l_expr, l_span));

                let (r_expr, r_span) = &**r_value;
                let r_value_typed = self.expr_to_typed_expr((r_expr, r_span));

                let ty = match (operator, &*l_value_typed.ty, &*l_value_typed.ty) {
                    (
                        BinOp::Add,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "int" && name_r == "int" => {
                        t_int!()
                    }
                    (
                        BinOp::Add,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "float" && name_r == "int" => {
                        t_float!()
                    }
                    (
                        BinOp::Add,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "int" && name_r == "float" => {
                        t_float!()
                    }
                    (
                        BinOp::Add,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "string" || name_r == "string" => {
                        t_string!()
                    }

                    (
                        BinOp::Sub,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "int" && name_r == "int" => {
                        t_int!()
                    }
                    (
                        BinOp::Sub,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "float" && name_r == "int" => {
                        t_float!()
                    }
                    (
                        BinOp::Sub,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "int" && name_r == "float" => {
                        t_float!()
                    }

                    (
                        BinOp::Mul,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "int" && name_r == "int" => {
                        t_int!()
                    }
                    (
                        BinOp::Mul,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "float" && name_r == "int" => {
                        t_float!()
                    }
                    (
                        BinOp::Mul,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "int" && name_r == "float" => {
                        t_float!()
                    }
                    (
                        BinOp::Mul,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if (name_l == "string" && name_r == "int")
                        || (name_l == "int" && name_r == "string") =>
                    {
                        t_string!()
                    }

                    (
                        BinOp::Div,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "int" && name_r == "int" => {
                        t_float!()
                    }
                    (
                        BinOp::Div,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "float" && name_r == "int" => {
                        t_float!()
                    }
                    (
                        BinOp::Div,
                        Type::Constructor { name: name_l, .. },
                        Type::Constructor { name: name_r, .. },
                    ) if name_l == "int" && name_r == "float" => {
                        t_float!()
                    }
                    (BinOp::Eq, ..) => {
                        t_bool!()
                    }
                    (BinOp::LessEq, ..) => {
                        t_bool!()
                    }
                    (BinOp::GreaterEq, ..) => {
                        t_bool!()
                    }
                    (BinOp::NotEq, ..) => {
                        t_bool!()
                    }

                    _ => {
                        let _ = self.unify(
                            l_value_typed.ty.clone(),
                            r_value_typed.ty.clone(),
                            l_span,
                            r_span,
                        ) || todo!("AAAAA! INVALID BinOp TYPES");
                        l_value_typed.ty.clone()
                    }
                };

                (
                    TypedExprKind::BinOp {
                        operator: operator.clone(),
                        l_value: Box::new(l_value_typed),
                        r_value: Box::new(r_value_typed),
                    },
                    ty.clone(),
                )
            }
            Expr::UnOp { unop, expression } => {
                let (expr, span) = &**expression;
                let typed_expr = self.expr_to_typed_expr((expr, span));
                let ty = typed_expr.ty.clone();
                if matches!(unop, UnOp::Not) {
                    let _ = self.unify(ty.clone(), t_bool!(), span, span)
                        || todo!("NOT OPERAND MUST BE A BOOL");
                }
                (
                    TypedExprKind::UnOp {
                        unop: unop.clone(),
                        expression: Box::new(typed_expr),
                    },
                    ty,
                )
            }
            Expr::Assign {
                l_value,
                r_value,
                assign_op,
            } => {
                let (l_expr, l_span) = &**l_value;
                let l_value_typed = self.expr_to_typed_expr((l_expr, l_span));

                let is_valid = matches!(
                    l_value_typed.kind,
                    TypedExprKind::Variable(_)
                        | TypedExprKind::Index { .. }
                        | TypedExprKind::StructAccess { .. }
                );

                if !is_valid {
                    todo!("Invalid assignment target");
                }

                let (r_expr, r_span) = &**r_value;
                let r_value_typed = self.expr_to_typed_expr((r_expr, r_span));

                // let _ty = match (assign_op, &*l_value_typed.ty, &*l_value_typed.ty) {
                //     (AssignOp::Assign, Type::Variable(_), Type::Variable(_)) => {

                //     }
                //     _=>todo!()
                // };

                let ty = r_value_typed.ty.clone();

                let _ = self.unify(
                    l_value_typed.ty.clone(),
                    r_value_typed.ty.clone(),
                    l_span,
                    r_span,
                ) || todo!("AAAA! INVALID ASSIGNMENT TYPES");

                (
                    TypedExprKind::Assign {
                        assign_op: assign_op.clone(),
                        l_value: Box::new(l_value_typed),
                        r_value: Box::new(r_value_typed),
                    },
                    ty.clone(),
                )
            }
            Expr::Do { expressions } => {
                let mut typed_exprs = vec![];
                for expression in expressions {
                    let (expr, span) = expression;
                    typed_exprs.push(self.expr_to_typed_expr((expr, span)));
                }
                let Some(ex) = typed_exprs.last() else {
                    todo!()
                };
                let ty = ex.ty.clone();
                (
                    TypedExprKind::Do {
                        expressions: typed_exprs,
                    },
                    ty,
                )
            }
            Expr::Let {
                var,
                type_annot,
                value,
            } => {
                let (val_expr, val_span) = &**value;
                let typed_val = self.expr_to_typed_expr((val_expr, val_span));

                let (var_ty, var_span) = match type_annot {
                    Some((annot, span)) => (type_annot_to_type(annot), span),
                    None => (typed_val.ty.clone(), val_span),
                };

                let ty = typed_val.ty.clone();

                self.insert_var(var.clone(), var_ty.clone());

                let _ = self.unify(var_ty, ty.clone(), val_span, var_span)
                    || todo!("AAAAAAAAA INVALID LET TYPE");

                (
                    TypedExprKind::Let {
                        var: var.clone(),
                        value: Box::new(typed_val),
                    },
                    ty,
                )
            }
            Expr::IfElse {
                condition,
                if_branch,
                else_branch,
            } => {
                let (condition, condition_span) = &**condition;
                let (if_branch, if_branch_span) = &**if_branch;

                let typed_condition = self.expr_to_typed_expr((condition, condition_span));
                let typed_if_branch = self.expr_to_typed_expr((if_branch, if_branch_span));

                let _ = self.unify(
                    typed_condition.ty.clone(),
                    t_bool!(),
                    condition_span,
                    condition_span,
                ) || todo!("AAAAAA! IF CONDITION MUST BE A BOOL");

                match else_branch {
                    Some(else_branch) => {
                        let ty = typed_if_branch.ty.clone();
                        let (else_branch, else_branch_span) = &**else_branch;
                        let typed_else_branch =
                            self.expr_to_typed_expr((else_branch, else_branch_span));
                        let _ = self.unify(
                            typed_if_branch.ty.clone(),
                            typed_else_branch.ty.clone(),
                            if_branch_span,
                            else_branch_span,
                        ) || todo!("AAA! IF AND ELSE BRANCH HAVE DIFF TYPES");
                        (
                            TypedExprKind::IfElse {
                                condition: Box::new(typed_condition),
                                if_branch: Box::new(typed_if_branch),
                                else_branch: Some(Box::new(typed_else_branch)),
                            },
                            ty,
                        )
                    }
                    None => (
                        TypedExprKind::IfElse {
                            condition: Box::new(typed_condition),
                            if_branch: Box::new(typed_if_branch),
                            else_branch: None,
                        },
                        t_unit!(),
                    ),
                }
            }
            Expr::Tuple(expressions) => {
                let mut typed_exprs = vec![];
                let mut types = vec![];

                for expression in expressions {
                    let (expr, span) = expression;
                    let typed = self.expr_to_typed_expr((expr, span));
                    types.push(typed.ty.clone());
                    typed_exprs.push(typed);
                }
                (
                    TypedExprKind::Tuple(typed_exprs),
                    Arc::new(Type::Tuple(types)),
                )
            }
            Expr::Error => unreachable!(),
        };
        TypedExpr {
            kind: exprkind,
            ty,
            range: span.clone(),
        }
    }
}

```

```rs
// typechecker/unify.rs
#![allow(unused_imports)]

use ariadne::ColorGenerator;
use ariadne::Fmt;
use ariadne::Label;
use ariadne::Report;
use ariadne::ReportKind;
use yansi::Color;
use yansi::Paint;

use std::collections::HashMap;
use std::collections::HashSet;
use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    ASTNode, AssignOp, BinOp, Expr, Function, Struct, Type, TypeAnnot, TypedASTNode, TypedExpr,
    TypedExprKind, TypedFunction, TypedStruct,
};
use crate::typechecker::{StructTy, TypeEnv, type_annot_to_type, type_string};
use crate::{t_bool, t_float, t_int, t_list, t_string, t_unit, tvar};

impl TypeEnv<'_> {
    pub fn unify(
        &mut self,
        t1: Arc<Type>,
        t2: Arc<Type>,
        span1: &Range<usize>,
        span2: &Range<usize>,
    ) -> bool {
        let t1 = self.resolve(t1);
        let t2 = self.resolve(t2);

        match (&*t1, &*t2) {
            // Same types
            _ if t1 == t2 => true,

            // Variable unification
            (Type::Variable(i), _) => self.bind(*i, t2, span1),
            (_, Type::Variable(i)) => self.bind(*i, t1, span2),

            // Union type unification
            (Type::Union(u1), Type::Union(u2)) => self.unify_unions(u1, u2, span1, span2),
            (Type::Union(u), _) => self.unify_with_union(u, &t2, span1, span2),
            (_, Type::Union(u)) => self.unify_with_union(u, &t1, span2, span1),

            // Function types
            (
                Type::Function {
                    params: p1,
                    return_type: r1,
                },
                Type::Function {
                    params: p2,
                    return_type: r2,
                },
            ) if p1.len() == p2.len() => {
                let mut f = true;
                for (a, b) in p1.iter().zip(p2.iter()) {
                    if !self.unify(a.clone(), b.clone(), span1, span2) {
                        f = false;
                    }
                }
                if f {
                    self.unify(r1.clone(), r2.clone(), span1, span2)
                } else {
                    f
                }
            }

            // Tuple types
            (Type::Tuple(t1), Type::Tuple(t2)) if t1.len() == t2.len() => {
                let mut f = true;
                for (a, b) in t1.iter().zip(t2.iter()) {
                    if !self.unify(a.clone(), b.clone(), span1, span2) {
                        f = false;
                    }
                }
                f
            }

            // Constructor types
            (
                Type::Constructor {
                    name: n1,
                    generics: g1,
                    ..
                },
                Type::Constructor {
                    name: n2,
                    generics: g2,
                    ..
                },
            ) if n1 == n2 && g1.len() == g2.len() => {
                let mut f = true;
                for (a, b) in g1.iter().zip(g2.iter()) {
                    if !self.unify(a.clone(), b.clone(), span1, span2) {
                        f = false;
                    }
                }
                f
            }

            // (Type::Union(u1), Type::Union(u2)) => {
            //     let mut success = false;
            //     for t1 in u1 {
            //         for t2 in u2 {
            //             if self.unify(t1.clone(), t2.clone(), span1, span2) {
            //                 success = true;
            //                 break;
            //             }
            //         }
            //         if success {
            //             break;
            //         }
            //     }
            //     success
            // }

            // Union and other types
            // (Type::Union(u), _other) => {
            //     let mut success = false;
            //     for t in u {
            //         if self.unify(t.clone(), t2.clone(), span1, span2) {
            //             success = true;
            //             break;
            //         }
            //     }
            //     success
            // }

            // (_other, Type::Union(u)) => {
            //     let mut success = false;
            //     for t in u {
            //         if self.unify(t1.clone(), t.clone(), span1, span2) {
            //             success = true;
            //             break;
            //         }
            //     }
            //     success
            // }

            // (
            //     Type::Trait(n1) ,
            //     Type::Trait(n2),
            // ) if n1 == n2 {
            //    // handled in the first case
            // }

            // Mismatched types
            _ => false,
        }
    }

    fn bind(&mut self, var: usize, ty: Arc<Type>, span: &Range<usize>) -> bool {
        if let Type::Variable(i) = *ty {
            if i == var {
                return true; // Same variable
            }
        }

        if self.occurs(var, &ty) {
            panic!(
                "Recursive type involving {} at {:?}",
                type_string(&ty),
                span
            );
        }

        self.substitutions.insert(var, ty);
        true
    }

    fn unify_with_union(
        &mut self,
        u: &[Arc<Type>],
        other: &Arc<Type>,
        span_union: &Range<usize>,
        span_other: &Range<usize>,
    ) -> bool {
        // If the other type is already in the union, succeed
        if u.iter()
            .any(|t| self.resolve(t.clone()) == self.resolve(other.clone()))
        {
            return true;
        }

        // Try to unify with each variant in the union
        for variant in u {
            if self.unify(variant.clone(), other.clone(), span_union, span_other) {
                return true;
            }
        }

        false
    }

    fn unify_unions(
        &mut self,
        u1: &[Arc<Type>],
        u2: &[Arc<Type>],
        span1: &Range<usize>,
        span2: &Range<usize>,
    ) -> bool {
        // Create sets of resolved types
        let set1: HashSet<_> = u1.iter().map(|t| self.resolve(t.clone())).collect();
        let set2: HashSet<_> = u2.iter().map(|t| self.resolve(t.clone())).collect();

        // If both sets are equal, unification succeeds
        if set1 == set2 {
            return true;
        }

        // If either set is a subset of the other, we can unify to the superset
        if set1.is_subset(&set2) {
            return self.bind_union_to_superset(u1, u2, span1, span2);
        } else if set2.is_subset(&set1) {
            return self.bind_union_to_superset(u2, u1, span2, span1);
        }

        // Try to find a common type
        for t1 in u1 {
            for t2 in u2 {
                if self.unify(t1.clone(), t2.clone(), span1, span2) {
                    return true;
                }
            }
        }

        false
    }

    fn bind_union_to_superset(
        &mut self,
        subset: &[Arc<Type>],
        superset: &[Arc<Type>],
        span_subset: &Range<usize>,
        _span_superset: &Range<usize>,
    ) -> bool {
        // Create type variables for each element in the subset
        for t in subset {
            if let Type::Variable(i) = &**t {
                self.bind(*i, Arc::new(Type::Union(superset.to_vec())), span_subset);
            }
        }
        true
    }

    fn occurs(&self, var: usize, ty: &Type) -> bool {
        match ty {
            Type::Variable(i) => {
                *i == var
                    || self
                        .substitutions
                        .get(i)
                        .is_some_and(|t| self.occurs(var, t))
            }
            Type::Constructor { generics, .. } => generics.iter().any(|t| self.occurs(var, t)),
            Type::Function {
                params,
                return_type,
            } => params.iter().any(|t| self.occurs(var, t)) || self.occurs(var, return_type),
            Type::Tuple(types) => types.iter().any(|t| self.occurs(var, t)),
            Type::Union(types) => types.iter().any(|t| self.occurs(var, t)),
            _ => false,
        }
    }

    pub fn resolve(&self, ty: Arc<Type>) -> Arc<Type> {
        match &*ty {
            Type::Variable(i) => self
                .substitutions
                .get(i)
                .map(|t| self.resolve(t.clone()))
                .unwrap_or(ty),
            Type::Union(types) => {
                // Resolve all types in the union
                let resolved: Vec<Arc<Type>> =
                    types.iter().map(|t| self.resolve(t.clone())).collect();

                // Remove duplicates
                let mut unique = Vec::new();
                for t in resolved {
                    if !unique.contains(&t) {
                        unique.push(t);
                    }
                }

                // Simplify single-type unions
                if unique.len() == 1 {
                    unique[0].clone()
                } else {
                    Arc::new(Type::Union(unique))
                }
            }
            _ => ty,
        }
    }
}

```

```rs
// typechecker/resolve.rs
#![allow(unused_imports)]

use std::collections::HashMap;
use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    ASTNode, AssignOp, BinOp, Expr, Function, Struct, Type, TypeAnnot, TypedASTNode, TypedExpr,
    TypedExprKind, TypedFunction, TypedStruct,
};
use crate::typechecker::{StructTy, TypeEnv, type_annot_to_type};
use crate::{t_bool, t_float, t_int, t_list, t_string, t_unit, tvar};

impl TypeEnv<'_> {
    // Resolve types in the entire environment and AST
    pub fn resolve_all(&mut self, ast: Vec<TypedASTNode>) -> Vec<TypedASTNode> {
        // Resolve variable types
        let mut resolved_vars = HashMap::new();
        for (name, ty) in &self.variables {
            resolved_vars.insert(name.clone(), self.resolve(ty.clone()));
        }
        self.variables = resolved_vars;

        // Resolve struct types
        let mut resolved_structs = HashMap::new();
        for (name, struct_ty) in &self.structs {
            let mut resolved_fields = Vec::new();
            for (field_name, field_ty) in &struct_ty.fields {
                resolved_fields.push((field_name.clone(), self.resolve(field_ty.clone())));
            }

            resolved_structs.insert(
                name.clone(),
                Arc::new(StructTy {
                    name: struct_ty.name.clone(),
                    generics: struct_ty.generics.clone(),
                    fields: resolved_fields,
                }),
            );
        }
        self.structs = resolved_structs;

        // Resolve AST nodes
        ast.into_iter()
            .map(|node| self.resolve_node(node))
            .collect()
    }

    fn resolve_node(&mut self, node: TypedASTNode) -> TypedASTNode {
        match node {
            TypedASTNode::Expr((expr, span)) => TypedASTNode::Expr((self.resolve_expr(expr), span)),
            TypedASTNode::Function((func, span)) => {
                TypedASTNode::Function((self.resolve_function(func), span))
            }
            TypedASTNode::Struct((strukt, span)) => {
                TypedASTNode::Struct((self.resolve_struct(strukt), span))
            }
            TypedASTNode::Error => TypedASTNode::Error,
        }
    }

    fn resolve_expr(&mut self, expr: TypedExpr) -> TypedExpr {
        // Resolve the type first
        let resolved_ty = self.resolve(expr.ty.clone());

        // Then resolve child expressions
        let kind = match expr.kind {
            TypedExprKind::Call { function, args } => {
                let resolved_function = self.resolve_expr(*function);
                let resolved_args = args.into_iter().map(|arg| self.resolve_expr(arg)).collect();
                TypedExprKind::Call {
                    function: Box::new(resolved_function),
                    args: resolved_args,
                }
            }
            TypedExprKind::BinOp {
                operator,
                l_value,
                r_value,
            } => {
                let resolved_l = self.resolve_expr(*l_value);
                let resolved_r = self.resolve_expr(*r_value);
                TypedExprKind::BinOp {
                    operator,
                    l_value: Box::new(resolved_l),
                    r_value: Box::new(resolved_r),
                }
            }
            TypedExprKind::Let { var, value } => {
                let resolved_value = self.resolve_expr(*value);
                TypedExprKind::Let {
                    var,
                    value: Box::new(resolved_value),
                }
            }
            // Add cases for other expression types as needed...
            _ => expr.kind,
        };

        TypedExpr {
            kind,
            ty: resolved_ty,
            range: expr.range,
        }
    }

    fn resolve_function(&mut self, func: TypedFunction) -> TypedFunction {
        TypedFunction {
            name: func.name,
            args: func
                .args
                .into_iter()
                .map(|(name, ty, range)| (name, self.resolve(ty), range))
                .collect(),
            body: Box::new((self.resolve_expr(func.body.0), func.body.1)),
            return_type: (self.resolve(func.return_type.0), func.return_type.1),
        }
    }

    fn resolve_struct(&mut self, strukt: TypedStruct) -> TypedStruct {
        TypedStruct {
            name: strukt.name,
            generics: strukt.generics,
            fields: strukt
                .fields
                .into_iter()
                .map(|(name, ty, range)| (name, self.resolve(ty), range))
                .collect(),
        }
    }
}

```

```rs
// typechecker/monomorphize.rs
#![allow(unused_imports)]

use std::collections::HashMap;
use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    ASTNode, AssignOp, BinOp, Expr, Function, Struct, Type, TypeAnnot, TypedASTNode, TypedExpr,
    TypedExprKind, TypedFunction, TypedStruct,
};
use crate::typechecker::{StructTy, TypeEnv, type_annot_to_type, type_string};
use crate::{t_bool, t_float, t_int, t_list, t_string, t_unit, tvar};

impl TypeEnv<'_> {
    fn substitute_type(&self, ty: &Arc<Type>, type_map: &HashMap<usize, Arc<Type>>) -> Arc<Type> {
        match &**ty {
            Type::Variable(id) => {
                type_map.get(id).cloned().unwrap_or_else(|| {
                    // If not in map, try to resolve it
                    let resolved = self.resolve(ty.clone());
                    if let Type::Variable(id) = &*resolved {
                        type_map.get(id).cloned().unwrap_or(resolved)
                    } else {
                        self.substitute_type(&resolved, type_map)
                    }
                })
            }
            Type::Constructor {
                name,
                generics,
                traits,
            } => {
                let new_generics = generics
                    .iter()
                    .map(|t| self.substitute_type(t, type_map))
                    .collect();
                Arc::new(Type::Constructor {
                    name: name.clone(),
                    generics: new_generics,
                    traits: traits.clone(),
                })
            }
            Type::Function {
                params,
                return_type,
            } => {
                let new_params = params
                    .iter()
                    .map(|t| self.substitute_type(t, type_map))
                    .collect();
                let new_return = self.substitute_type(return_type, type_map);
                Arc::new(Type::Function {
                    params: new_params,
                    return_type: (new_return),
                })
            }
            Type::Tuple(types) => {
                let new_types = types
                    .iter()
                    .map(|t| self.substitute_type(t, type_map))
                    .collect();
                Arc::new(Type::Tuple(new_types))
            }
            Type::Union(types) => {
                let new_types = types
                    .iter()
                    .map(|t| self.substitute_type(t, type_map))
                    .collect();
                Arc::new(Type::Union(new_types))
            }
            _ => ty.clone(),
        }
    }
    /// Properly monomorphizes the AST with correct order and type replacement
    pub fn monomorphize_ast(&mut self, ast: Vec<TypedASTNode>) -> Vec<TypedASTNode> {
        // First pass: collect all generic functions
        let mut generic_fns = HashMap::new();
        let mut non_generic_nodes = Vec::new();

        for node in ast {
            if let TypedASTNode::Function((ref func, ref _span)) = node {
                if func.is_generic() {
                    if let TypedASTNode::Function((func, span)) = node {
                        generic_fns.insert(func.name.clone(), (func, span));
                    }
                    continue;
                }
            }
            non_generic_nodes.push(node);
        }

        // Second pass: process nodes and specialize functions
        let mut specialized_fns = HashMap::new();
        let mut processed_nodes = Vec::new();

        for node in non_generic_nodes {
            processed_nodes.push(self.process_node(node, &generic_fns, &mut specialized_fns));
        }

        // Third pass: add specialized functions to the beginning of the AST
        let mut result = Vec::new();
        for (_, (func, span)) in specialized_fns {
            result.push(TypedASTNode::Function((func, span)));
        }
        result.extend(processed_nodes);
        result
    }

    fn process_node(
        &mut self,
        node: TypedASTNode,
        generic_fns: &HashMap<String, (TypedFunction, Range<usize>)>,
        specialized_fns: &mut HashMap<String, (TypedFunction, Range<usize>)>,
    ) -> TypedASTNode {
        match node {
            TypedASTNode::Expr((expr, span)) => {
                TypedASTNode::Expr((self.process_expr(expr, generic_fns, specialized_fns), span))
            }
            TypedASTNode::Function((func, span)) => {
                // Process function body for nested generics
                let (body_expr, body_span) = *func.body;
                let processed_body = self.process_expr(body_expr, generic_fns, specialized_fns);
                TypedASTNode::Function((
                    TypedFunction {
                        body: Box::new((processed_body, body_span)),
                        ..func
                    },
                    span,
                ))
            }
            _ => node,
        }
    }

    fn process_expr(
        &mut self,
        expr: TypedExpr,
        generic_fns: &HashMap<String, (TypedFunction, Range<usize>)>,
        specialized_fns: &mut HashMap<String, (TypedFunction, Range<usize>)>,
    ) -> TypedExpr {
        match expr.kind {
            TypedExprKind::Call { function, args } => {
                let new_function = self.process_expr(*function, generic_fns, specialized_fns);
                let new_args = args
                    .into_iter()
                    .map(|arg| self.process_expr(arg, generic_fns, specialized_fns))
                    .collect::<Vec<_>>();

                if let TypedExprKind::Variable(func_name) = &new_function.kind {
                    if let Some((generic_func, orig_span)) = generic_fns.get(func_name) {
                        // Create specialized function
                        let specialized_func =
                            self.specialize_function_at_call(generic_func, &new_args);
                        let spec_name = specialized_func.name.clone();

                        // Store specialized function
                        specialized_fns.insert(
                            spec_name.clone(),
                            (specialized_func.clone(), orig_span.clone()),
                        );

                        // Return new call with concrete type
                        return TypedExpr {
                            kind: TypedExprKind::Call {
                                function: Box::new(TypedExpr {
                                    kind: TypedExprKind::Variable(spec_name),
                                    ty: new_function.ty,
                                    range: new_function.range,
                                }),
                                args: new_args,
                            },
                            // FIXED: Use the specialized function's return type
                            ty: specialized_func.return_type.0.clone(),
                            range: expr.range,
                        };
                    }
                    else if let Some(struct_ty) = self.get_struct(func_name) {
                                            if !struct_ty.generics.is_empty() {
                                                return self.specialize_struct_constructor(
                                                    func_name,
                                                    &struct_ty,
                                                    &new_args,
                                                    expr.range,
                                                    specialized_fns,
                                                );
                                            }
                                        }
                }

                // Non-generic call
                TypedExpr {
                    kind: TypedExprKind::Call {
                        function: Box::new(new_function),
                        args: new_args,
                    },
                    ty: expr.ty,
                    range: expr.range,
                }
            }
            // ... handle other expression types ...
            _ => expr,
        }
    }

    fn specialize_struct_constructor(
            &self,
            struct_name: &str,
            struct_ty: &Arc<StructTy>,
            args: &[TypedExpr],
            call_range: Range<usize>,
            specialized_fns: &mut HashMap<String, (TypedFunction, Range<usize>)>,
        ) -> TypedExpr {
            // Create type mapping from arguments to struct's generic parameters
            let mut type_map = HashMap::new();
            for (i, (_field_name, field_ty)) in struct_ty.fields.iter().enumerate() {
                if let Type::Variable(id) = &**field_ty {
                    let resolved_arg_ty = self.resolve_deep(args[i].ty.clone());
                    type_map.insert(*id, resolved_arg_ty);
                }
            }

            // Create specialized struct name
            let generic_types: Vec<String> = struct_ty.generics.iter()
                .map(|g| {
                    println!("{:?}", g);
                    if let Some(ty) = type_map.get(&g.parse().unwrap()) {
                        type_signature_string(ty)
                    } else {
                        format!("T{}", g)
                    }
                })
                .collect();
            
            let spec_name = if generic_types.is_empty() {
                struct_name.to_string()
            } else {
                format!("{}_{}", struct_name, generic_types.join("_"))
            };

            // Create specialized return type
            let specialized_return = Arc::new(Type::Constructor {
                name: struct_name.to_string(),
                generics: struct_ty.generics.iter()
                    .filter_map(|g| g.parse().ok().and_then(|id| type_map.get(&id).cloned()))
                    .collect(),
                traits: vec![],
            });

            // Create the specialized constructor function if it doesn't exist
            if !specialized_fns.contains_key(&spec_name) {
                let constructor_func = self.create_constructor_function(struct_ty, &spec_name, &type_map);
                specialized_fns.insert(spec_name.clone(), (constructor_func, call_range.clone()));
            }

            // Return the new call expression
            TypedExpr {
                kind: TypedExprKind::Call {
                    function: Box::new(TypedExpr {
                        kind: TypedExprKind::Variable(spec_name.clone()),
                        ty: Arc::new(Type::Function {
                            params: args.iter().map(|a| a.ty.clone()).collect(),
                            return_type: specialized_return.clone(),
                        }),
                        range: call_range.clone(),
                    }),
                    args: args.to_vec(),
                },
                ty: specialized_return,
                range: call_range,
            }
        }

        fn create_constructor_function(
            &self,
            struct_ty: &StructTy,
            spec_name: &str,
            type_map: &HashMap<usize, Arc<Type>>,
        ) -> TypedFunction {
            // Create specialized field types
            let specialized_fields: Vec<(String, Arc<Type>, Range<usize>)> = struct_ty.fields.iter()
                .map(|(name, ty)| {
                    let spec_ty = self.substitute_type(ty, type_map);
                    (name.clone(), spec_ty, 0..0) // Dummy span
                })
                .collect();

            // Create specialized return type
            let specialized_return = Arc::new(Type::Constructor {
                name: spec_name.to_string(),
                generics: vec![],
                traits: vec![],
            });

            // Create dummy body (will be handled during codegen)
            let dummy_body = TypedExpr {
                kind: TypedExprKind::Error,
                ty: specialized_return.clone(),
                range: 0..0,
            };

            TypedFunction {
                name: spec_name.to_string(),
                args: specialized_fields,
                body: Box::new((dummy_body, 0..0)),
                return_type: (specialized_return, 0..0),
            }
        }

    fn specialize_function_at_call(
        &self,
        func: &TypedFunction,
        args: &[TypedExpr],
    ) -> TypedFunction {
        let mut specialized = func.clone();
        let mut type_map = HashMap::new();

        // Create type mapping from arguments
        for (i, (_, param_ty, _)) in func.args.iter().enumerate() {
            if let Type::Variable(id) = &**param_ty {
                let resolved_ty = self.resolve_deep(args[i].ty.clone());
                if let Some(existing) = type_map.get(id) {
                    if *existing != resolved_ty {
                        panic!("Type variable T{} inferred as both {:?} and {:?}", id, existing, resolved_ty);
                    }
                } else {
                    type_map.insert(*id, resolved_ty);
                }
            }
        }
        
        // Generate unique name based on argument types
        let arg_types: Vec<String> = args
            .iter()
            .map(|arg| type_signature_string(&arg.ty))
            .collect();

        // Include return type in the signature for full uniqueness
        let return_type = self.substitute_type(&func.return_type.0, &type_map);
        let return_str = type_signature_string(&return_type);

        let signature = format!("{}_{}_to_{}", func.name, arg_types.join("_"), return_str);

        // Sanitize the name for use as an identifier
        let spec_name = sanitize_identifier(&signature);
        specialized.name = spec_name;

        // Apply type mapping to function signature
        specialized.args = func
            .args
            .iter()
            .map(|(name, ty, span)| {
                let new_ty = self.substitute_type(ty, &type_map);
                (name.clone(), new_ty, span.clone())
            })
            .collect();

        specialized.return_type.0 = return_type;

        // Apply substitution to function body
        let (body_expr, body_span) = *func.body.clone();
        specialized.body = Box::new((self.substitute_in_expr(body_expr, &type_map), body_span));

        specialized
    }
    fn substitute_in_expr(
        &self,
        expr: TypedExpr,
        type_map: &HashMap<usize, Arc<Type>>,
    ) -> TypedExpr {
        let new_ty = self.substitute_type(&expr.ty, type_map);

        let new_kind = match expr.kind {
            TypedExprKind::Call { function, args } => {
                let new_function = Box::new(self.substitute_in_expr(*function, type_map));
                let new_args = args
                    .into_iter()
                    .map(|arg| self.substitute_in_expr(arg, type_map))
                    .collect();
                TypedExprKind::Call {
                    function: new_function,
                    args: new_args,
                }
            }
            TypedExprKind::Variable(name) => TypedExprKind::Variable(name),
            TypedExprKind::BinOp {
                operator,
                l_value,
                r_value,
            } => {
                let new_l = Box::new(self.substitute_in_expr(*l_value, type_map));
                let new_r = Box::new(self.substitute_in_expr(*r_value, type_map));
                TypedExprKind::BinOp {
                    operator,
                    l_value: new_l,
                    r_value: new_r,
                }
            }
            TypedExprKind::Let { var, value } => {
                let new_value = Box::new(self.substitute_in_expr(*value, type_map));
                TypedExprKind::Let {
                    var,
                    value: new_value,
                }
            }
            TypedExprKind::IfElse {
                condition,
                if_branch,
                else_branch,
            } => {
                let new_cond = Box::new(self.substitute_in_expr(*condition, type_map));
                let new_if = Box::new(self.substitute_in_expr(*if_branch, type_map));
                let new_else = else_branch.map(|b| Box::new(self.substitute_in_expr(*b, type_map)));
                TypedExprKind::IfElse {
                    condition: new_cond,
                    if_branch: new_if,
                    else_branch: new_else,
                }
            }
            // Add other expression types as needed
            kind => kind,
        };

        TypedExpr {
            kind: new_kind,
            ty: new_ty,
            range: expr.range,
        }
    }

    pub fn resolve_deep(&self, ty: Arc<Type>) -> Arc<Type> {
        let resolved = self.resolve(ty.clone());
        match &*resolved {
            // Handle function types specially for monomorphization
            Type::Function {
                params,
                return_type,
            } => {
                let concrete_params = params
                    .iter()
                    .map(|t| self.resolve_deep(t.clone()))
                    .collect();
                let concrete_return = self.resolve_deep(return_type.clone());

                Arc::new(Type::Function {
                    params: concrete_params,
                    return_type: (concrete_return),
                })
            }

            // Other types get normal resolution
            _ => match &*resolved {
                Type::Variable(_) => resolved,
                Type::Constructor {
                    name,
                    generics,
                    traits,
                } => {
                    let resolved_generics = generics
                        .iter()
                        .map(|t| self.resolve_deep(t.clone()))
                        .collect();
                    Arc::new(Type::Constructor {
                        name: name.clone(),
                        generics: resolved_generics,
                        traits: traits.clone(),
                    })
                }
                Type::Tuple(types) => {
                    let resolved_types =
                        types.iter().map(|t| self.resolve_deep(t.clone())).collect();
                    Arc::new(Type::Tuple(resolved_types))
                }
                Type::Union(types) => {
                    let resolved_types =
                        types.iter().map(|t| self.resolve_deep(t.clone())).collect();
                    Arc::new(Type::Union(resolved_types))
                }
                _ => resolved,
            },
        }
    }
}

/// Sanitizes a string to be a valid identifier
fn sanitize_identifier(name: &str) -> String {
    name.chars()
        .map(|c| {
            if c.is_alphanumeric() || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect()
}

/// Generates a unique string representation of a type for specialization names
fn type_signature_string(ty: &Arc<Type>) -> String {
    match &**ty {
        Type::Constructor { name, generics, .. } => {
            if generics.is_empty() {
                name.clone()
            } else {
                let generics_str = generics
                    .iter()
                    .map(type_signature_string)
                    .collect::<Vec<_>>()
                    .join("_");
                format!("{}<{}>", name, generics_str)
            }
        }
        Type::Variable(i) => format!("T{}", i),
        Type::Function {
            params,
            return_type,
        } => {
            let params_str = params
                .iter()
                .map(type_signature_string)
                .collect::<Vec<_>>()
                .join("_");
            let return_str = type_signature_string(return_type);
            format!("fn_{}_to_{}", params_str, return_str)
        }
        Type::Tuple(types) => {
            let types_str = types
                .iter()
                .map(type_signature_string)
                .collect::<Vec<_>>()
                .join("_");
            format!("tuple_{}", types_str)
        }
        Type::Union(types) => {
            let types_str = types
                .iter()
                .map(type_signature_string)
                .collect::<Vec<_>>()
                .join("_");
            format!("union_{}", types_str)
        }
        _ => format!("{:?}", ty).replace(" ", ""),
    }
}

```

