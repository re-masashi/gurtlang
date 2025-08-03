pub mod collections;
pub mod io;
pub mod math;
pub mod string;

use crate::typechecker::TypeEnv;

pub fn add_stdlib_to_env(type_env: &mut TypeEnv) {
    collections::add_collections(type_env);
    string::add_string_functions(type_env);
    math::add_math_functions(type_env);
    io::add_io_functions(type_env);
}
