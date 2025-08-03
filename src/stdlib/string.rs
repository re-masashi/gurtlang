use crate::ast::Type;
use crate::typechecker::TypeEnv;

use crate::t_bool;
use crate::t_int;
use crate::t_list;
use crate::t_string;

use std::sync::Arc;

pub fn add_string_functions(type_env: &mut TypeEnv) {
    let string_type = t_string!();

    let int_type = t_int!();

    // substring(s: string, start: int, end: int) -> string
    let substring_type = Arc::new(Type::Function {
        params: vec![string_type.clone(), int_type.clone(), int_type.clone()],
        return_type: string_type.clone(),
    });
    type_env.insert_var("substring".to_string(), substring_type);

    // to_upper(s: string) -> string
    let to_upper_type = Arc::new(Type::Function {
        params: vec![string_type.clone()],
        return_type: string_type.clone(),
    });
    type_env.insert_var("to_upper".to_string(), to_upper_type);

    // to_lower(s: string) -> string
    let to_lower_type = Arc::new(Type::Function {
        params: vec![string_type.clone()],
        return_type: string_type.clone(),
    });
    type_env.insert_var("to_lower".to_string(), to_lower_type);

    // split(s: string, delimiter: string) -> [string]
    let split_type = Arc::new(Type::Function {
        params: vec![string_type.clone(), string_type.clone()],
        return_type: t_list!(string_type.clone()),
    });
    type_env.insert_var("split".to_string(), split_type);

    // trim(s: string) -> string
    let trim_type = Arc::new(Type::Function {
        params: vec![string_type.clone()],
        return_type: string_type.clone(),
    });
    type_env.insert_var("trim".to_string(), trim_type);

    // contains(s: string, substring: string) -> bool
    let contains_type = Arc::new(Type::Function {
        params: vec![string_type.clone(), string_type.clone()],
        return_type: t_bool!(),
    });
    type_env.insert_var("contains".to_string(), contains_type);
}
