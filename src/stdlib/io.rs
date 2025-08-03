use crate::ast::Type;
use crate::typechecker::TypeEnv;

use crate::t_bool;
use crate::t_string;

use std::sync::Arc;

pub fn add_io_functions(type_env: &mut TypeEnv) {
    let string_type = t_string!();

    let bool_type = t_bool!();

    // read_file(path: string) -> string?
    // let read_file_type = Arc::new(Type::Function {
    //     params: vec![string_type.clone()],
    //     return_type: Arc::new(Type::Constructor {
    //         name: "Option".to_string(),
    //         generics: vec![string_type.clone()],
    //     }),
    // });
    // type_env.insert_var("read_file".to_string(), read_file_type);

    // write_file(path: string, content: string) -> bool
    let write_file_type = Arc::new(Type::Function {
        params: vec![string_type.clone(), string_type.clone()],
        return_type: bool_type.clone(),
    });
    type_env.insert_var("write_file".to_string(), write_file_type);

    // file_exists(path: string) -> bool
    let file_exists_type = Arc::new(Type::Function {
        params: vec![string_type.clone()],
        return_type: bool_type.clone(),
    });
    type_env.insert_var("file_exists".to_string(), file_exists_type);
}
