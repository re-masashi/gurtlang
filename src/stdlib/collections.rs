use crate::ast::Type;
use crate::typechecker::TypeEnv;

use crate::t_int;
use crate::t_list;

use std::sync::Arc;

pub fn add_collections(type_env: &mut TypeEnv) {
    let array_type = |element_type: Arc<Type>| t_list!(element_type);

    // append(array: [T], element: T) -> [T]
    let append_type = Arc::new(Type::Function {
        params: vec![
            array_type(Arc::new(Type::Variable(0))),
            Arc::new(Type::Variable(0)),
        ],
        return_type: array_type(Arc::new(Type::Variable(0))),
    });
    type_env.insert_var("append".to_string(), append_type);

    // pop(array: [T]) -> T?
    // let pop_type = Arc::new(Type::Function {
    //     params: vec![array_type(Arc::new(Type::Variable(0)))],
    //     return_type: Arc::new(Type::Constructor {
    //         name: "Option".to_string(),
    //         generics: vec![Arc::new(Type::Variable(0))],
    //     }),
    // });
    // type_env.insert_var("pop".to_string(), pop_type);

    // slice(array: [T], start: int, end: int) -> [T]
    let slice_type = Arc::new(Type::Function {
        params: vec![array_type(Arc::new(Type::Variable(0))), t_int!(), t_int!()],
        return_type: array_type(Arc::new(Type::Variable(0))),
    });
    type_env.insert_var("slice".to_string(), slice_type);

    // reverse(array: [T]) -> [T]
    let reverse_type = Arc::new(Type::Function {
        params: vec![array_type(Arc::new(Type::Variable(0)))],
        return_type: array_type(Arc::new(Type::Variable(0))),
    });
    type_env.insert_var("reverse".to_string(), reverse_type);
}
