use crate::ast::Type;
use crate::typechecker::TypeEnv;

use crate::t_float;
use crate::t_int;

use std::sync::Arc;

pub fn add_math_functions(type_env: &mut TypeEnv) {
    let int_type = t_int!();

    let float_type = t_float!();

    // abs(x: int) -> int
    let abs_int_type = Arc::new(Type::Function {
        params: vec![int_type.clone()],
        return_type: int_type.clone(),
    });
    type_env.insert_var("abs".to_string(), abs_int_type);

    // sqrt(x: float) -> float
    let sqrt_type = Arc::new(Type::Function {
        params: vec![float_type.clone()],
        return_type: float_type.clone(),
    });
    type_env.insert_var("sqrt".to_string(), sqrt_type);

    // max(a: T, b: T) -> T (generic)
    let max_type = Arc::new(Type::Function {
        params: vec![Arc::new(Type::Variable(0)), Arc::new(Type::Variable(0))],
        return_type: Arc::new(Type::Variable(0)),
    });
    type_env.insert_var("max".to_string(), max_type);

    // min(a: T, b: T) -> T (generic)
    let min_type = Arc::new(Type::Function {
        params: vec![Arc::new(Type::Variable(0)), Arc::new(Type::Variable(0))],
        return_type: Arc::new(Type::Variable(0)),
    });
    type_env.insert_var("min".to_string(), min_type);

    // pow(base: float, exp: float) -> float
    let pow_type = Arc::new(Type::Function {
        params: vec![float_type.clone(), float_type.clone()],
        return_type: float_type.clone(),
    });
    type_env.insert_var("pow".to_string(), pow_type);

    // floor(x: float) -> int
    let floor_type = Arc::new(Type::Function {
        params: vec![float_type.clone()],
        return_type: int_type.clone(),
    });
    type_env.insert_var("floor".to_string(), floor_type);

    // ceil(x: float) -> int
    let ceil_type = Arc::new(Type::Function {
        params: vec![float_type.clone()],
        return_type: int_type.clone(),
    });
    type_env.insert_var("ceil".to_string(), ceil_type);
}
