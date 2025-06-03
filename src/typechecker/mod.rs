pub mod convert;
pub mod function;
pub mod struct_;

use ariadne::{Report, Source};

use std::collections::HashMap;
use std::fs;
use std::ops::Range;
use std::sync::Arc;

use crate::ast::Type;

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
    errors: Vec<Report<'a, (String, Range<usize>)>>,
    file: String,
}

impl TypeEnv<'_> {

    pub fn new(file: String) -> Self {
        TypeEnv {
            variables: HashMap::new(),
            structs: HashMap::new(),
            errors: vec![],
            file
        }
    }

    pub fn report_errors(&self) {
        let contents =
            fs::read_to_string(&self.file).expect("Should have been able to read the file :/");
        let source = Source::from(contents);

        for error in &self.errors {
            error.print((self.file.clone(), source.clone())).unwrap();
        }
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
