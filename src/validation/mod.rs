use crate::ast::TypedASTNode;

pub mod no_typevars;

#[cfg(test)]
pub mod test;

pub fn validate_ast(
    ast: Vec<TypedASTNode>,
    file: String,
) -> Vec<(String, std::ops::Range<usize>, String)> {
    no_typevars::check_for_type_vars(&ast, &file)
}
