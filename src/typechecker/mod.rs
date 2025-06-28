pub mod enum_;
pub mod expression;
pub mod function;
pub mod monomorphize;
pub mod resolve;
pub mod struct_;
pub mod unify;

#[cfg(test)]
pub mod test;

use ariadne::{Report, ReportKind, Source};

use std::collections::HashMap;
use std::fs;
use std::ops::Range;
use std::sync::Arc;

use crate::ast::{EnumVariantKind, Type, TypeAnnot};
use crate::tvar;

type Error<'a> = (ReportKind<'a>, Report<'a, (String, Range<usize>)>);

#[derive(Debug)]
pub struct StructTy {
    pub name: String,
    pub generics: Vec<String>,
    pub fields: Vec<(String, Arc<Type>)>,
}

#[derive(Debug)]
pub struct EnumTy {
    pub name: String,
    pub generics: Vec<String>,
    pub variants: HashMap<String, EnumVariantTy>,
}

#[derive(Debug, Clone)]
pub struct EnumVariantTy {
    pub kind: EnumVariantKindTy,
    pub ty: Arc<Type>,
}

#[derive(Debug, Clone)]
pub enum EnumVariantKindTy {
    Unit,
    Tuple(Vec<Arc<Type>>),
    Struct(Vec<(String, Arc<Type>)>),
}

#[derive(Debug)]
pub struct TypeEnv<'a> {
    pub variables: HashMap<String, Arc<Type>>,
    pub structs: HashMap<String, Arc<StructTy>>,
    pub enums: HashMap<String, Arc<EnumTy>>,
    pub type_aliases: HashMap<String, (Vec<String>, Arc<Type>)>,
    pub tvar_count: usize,
    substitutions: HashMap<usize, Arc<Type>>,
    in_function: bool,
    return_depth: usize, // Track depth of return expressions
    errors: Vec<Error<'a>>,
    file: String,
}

impl TypeEnv<'_> {
    pub fn new(file: String) -> Self {
        TypeEnv {
            variables: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),
            type_aliases: HashMap::new(),
            errors: vec![],
            substitutions: HashMap::new(),
            tvar_count: 0,
            in_function: false,
            return_depth: 0,
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
        Type::Function { params, return_type } => {
            let param_str = params
                .iter()
                .map(|t| type_string(t))
                .collect::<Vec<_>>()
                .join(", ");
            format!("fn({}) -> {}", param_str, type_string(return_type))
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
