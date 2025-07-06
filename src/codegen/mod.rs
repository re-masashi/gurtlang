// use inkwell::OptimizationLevel;
// use inkwell::builder::Builder;
// use inkwell::context::Context;
// use inkwell::module::Module;
// use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};
// use inkwell::types::{BasicTypeEnum, StructType};
// use inkwell::values::{BasicValueEnum, FunctionValue};

// use crate::ast::{
//     Type, TypedASTNode, TypedEnum, TypedExpr, TypedExprKind, TypedFunction, TypedStruct,
// };
// use crate::t_int;

// use std::collections::HashMap;
// use std::ops::Range;
// use std::path::Path;
// use std::sync::Arc;

// // pub mod enum_;
// // pub mod function;
// // pub mod struct_;

// #[derive(Clone, Debug)]
// pub enum IRType<'ctx> {
//     Function {
//         args: Vec<(String, IRType<'ctx>)>,
//         return_type: Box<IRType<'ctx>>,
//     },
//     Struct {
//         struct_ty: StructType<'ctx>,
//         fields: Vec<(String, IRType<'ctx>)>,
//     },
//     Enum {
//         /// The overall LLVM `StructType` representing the entire enum (discriminant + payload).
//         enum_struct_ty: StructType<'ctx>,
//         /// A map from the source language variant name to its discriminant and its payload IRType.
//         /// `Option<IRType>` is `None` for variants without associated data (e.g., `None` or `Quit`).
//         variants: HashMap<String, (u64, Option<IRType<'ctx>>)>,
//         /// The LLVM `StructType` that specifically represents the union-like payload part (e.g., `[N x i8]`).
//         /// This is useful for `bitcast` operations during data access.
//         payload_struct_ty: StructType<'ctx>,
//     },
//     Simple(BasicTypeEnum<'ctx>), // int, bool, unit, etc
//     BuiltIn,                     // functions such as type().
// }

// #[derive(Clone)]
// pub enum IRValue<'ctx> {
//     Function {
//         fun_val: FunctionValue<'ctx>,
//         args: Vec<(String, IRType<'ctx>)>,
//         return_type: Box<IRType<'ctx>>,
//     }, // function, args, ret
//     Struct {
//         struct_ty: StructType<'ctx>,
//         fields: Vec<(String, IRType<'ctx>)>,
//     },
//     Enum {
//         enum_ir_type: IRType<'ctx>,
//     },
//     Simple(BasicValueEnum<'ctx>),
//     BuiltIn(String),
// }

// #[allow(dead_code)]
// pub struct IRGenerator<'ctx> {
//     context: &'ctx Context,
//     module: Module<'ctx>,
//     builder: Builder<'ctx>,
//     lambda_counter: i32,
//     variables: HashMap<String, (IRValue<'ctx>, IRType<'ctx>)>,
//     structs: HashMap<String, (StructType<'ctx>, Vec<(String, IRType<'ctx>)>)>,
//     pos: i32,
//     line_no: i32,
//     file: String,
//     builtins: Vec<IRValue<'ctx>>,
// }

// impl<'ctx> IRGenerator<'ctx> {
//     pub fn new(context: &'ctx Context, file: String) -> Self {
//         let module = context.create_module(&file);
//         let builder = context.create_builder();
//         Self {
//             context,
//             module,
//             builder,
//             lambda_counter: 0,
//             variables: HashMap::new(),
//             structs: HashMap::new(),
//             pos: 0,
//             line_no: 0,
//             file,
//             builtins: vec![
//                 IRValue::BuiltIn("type".to_string()),
//                 IRValue::BuiltIn("print".to_string()),
//                 IRValue::BuiltIn("println".to_string()),
//                 IRValue::BuiltIn("str".to_string()),
//                 IRValue::BuiltIn("array".to_string()),
//                 IRValue::BuiltIn("push".to_string()),
//                 IRValue::BuiltIn("set".to_string()),
//                 IRValue::BuiltIn("len".to_string()),
//                 IRValue::BuiltIn("size".to_string()),
//             ],
//         }
//     }

//     pub fn print_ir(&self) {
//         println!("\n{}", self.module.print_to_string().to_string());
//     }

//     pub fn gen_program(
//         &mut self,
//         nodes: Vec<&TypedASTNode>,
//         // _span: &Span,
//         optimisation_level: u8,
//     ) {
//         let default_triple = TargetMachine::get_default_triple();

//         Target::initialize_x86(&InitializationConfig::default());

//         let opt = match optimisation_level {
//             0 => OptimizationLevel::None,
//             1 => OptimizationLevel::Less,
//             2 => OptimizationLevel::Default,
//             _ => OptimizationLevel::Aggressive,
//         };

//         let reloc = RelocMode::PIC;
//         let model = CodeModel::Default;
//         let binding = self.file.clone() + ".o";
//         let _path = Path::new(&binding);
//         let target = Target::from_name("x86-64").unwrap();
//         let target_machine = target
//             .create_target_machine(&default_triple, "x86-64", "+avx2", opt, reloc, model)
//             .unwrap();

//         let data_layout = target_machine.get_target_data().get_data_layout();
//         self.module.set_data_layout(&data_layout);

//         let mut exprs = vec![];

//         for node in nodes {
//             match node {
//                 TypedASTNode::Expr((e, span)) => exprs.push((e, span)),
//                 TypedASTNode::Function((f, span)) => self.gen_function(f, span),
//                 TypedASTNode::Struct((s, span)) => self.gen_struct(s, span),
//                 TypedASTNode::Enum((e, span)) => self.gen_enum(e, span),
//                 TypedASTNode::Error => unreachable!(),
//             }
//         }

//         exprs.push((
//             &TypedExpr {
//                 kind: TypedExprKind::Int(0),
//                 ty: t_int!(),
//                 range: 0..0,
//             },
//             &(0..0),
//         ));
//     }

//     pub fn gen_function(&mut self, _node: &TypedFunction, _span: &Range<usize>) {
//         todo!()
//     }

//     pub fn gen_struct(&mut self, _node: &TypedStruct, _span: &Range<usize>) {
//         todo!()
//     }

//     pub fn gen_enum(&mut self, _node: &TypedEnum, _span: &Range<usize>) {
//         todo!()
//     }
// }
