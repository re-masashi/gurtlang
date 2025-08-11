use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module as LLVMModule;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FunctionValue};
use std::collections::HashMap;

pub mod function;
pub mod instruction;

pub struct LLVMCodegen<'ctx> {
    context: &'ctx Context,
    module: LLVMModule<'ctx>,
    builder: Builder<'ctx>,
    function_value_map: HashMap<String, FunctionValue<'ctx>>,
    value_map: HashMap<String, BasicValueEnum<'ctx>>,
    current_function: Option<FunctionValue<'ctx>>,
}

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
            function_value_map: HashMap::new(),
            value_map: HashMap::new(),
            current_function: None,
        }
    }

    fn get_or_declare_memcpy(&mut self) -> FunctionValue<'ctx> {
        if let Some(memcpy) = self.module.get_function("memcpy") {
            return memcpy;
        }

        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let memcpy_type = ptr_type.fn_type(
            &[
                ptr_type.into(),
                ptr_type.into(),
                self.context.i64_type().into(),
            ],
            false,
        );

        let memcpy_fn = self.module.add_function("memcpy", memcpy_type, None);
        self.function_value_map
            .insert("memcpy".to_string(), memcpy_fn);
        memcpy_fn
    }

    pub fn get_llvm_type(&self, ir_type: &crate::ir::IRType) -> BasicTypeEnum<'ctx> {
        use crate::ir::IRType::*;
        match ir_type {
            I1 => self.context.bool_type().as_basic_type_enum(),
            I8 => self.context.i8_type().as_basic_type_enum(),
            I32 => self.context.i32_type().as_basic_type_enum(),
            I64 => self.context.i64_type().as_basic_type_enum(),
            F64 => self.context.f64_type().as_basic_type_enum(),
            Ptr => self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .as_basic_type_enum(),
            Struct { name, fields } => {
                let struct_ty = self.module.get_struct_type(name).unwrap_or_else(|| {
                    let s = self.context.opaque_struct_type(name);
                    let field_types: Vec<_> = fields
                        .iter()
                        .map(|field| self.get_llvm_type(field))
                        .collect();
                    s.set_body(&field_types, false);
                    s
                });
                struct_ty.as_basic_type_enum()
            }
            Function { .. } => self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .as_basic_type_enum(),
            Array { element_type, size } => {
                let elem_ty = self.get_llvm_type(element_type);
                elem_ty.array_type(*size as u32).as_basic_type_enum()
            }
            Void => {
                panic!("Void type cannot be converted to BasicTypeEnum")
            }
            TaggedUnion { .. } => self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .as_basic_type_enum(),
        }
    }

    pub fn declare_function(&mut self, function: &crate::ir::Function) -> FunctionValue<'ctx> {
        if let Some(f) = self.module.get_function(&function.name) {
            self.function_value_map.insert(function.name.clone(), f);
            return f;
        }

        if function.name == "printf" {
            let printf_type = self.context.i32_type().fn_type(
                &[self
                    .context
                    .ptr_type(inkwell::AddressSpace::default())
                    .into()],
                true,
            );

            let fn_val = self.module.add_function("printf", printf_type, None);
            self.function_value_map.insert("printf".to_string(), fn_val);
            return fn_val;
        }

        // Special handling for array functions to ensure correct signatures
        if function.name == "array_get" {
            let fn_type = self.context.i64_type().fn_type(
                // Returns i64, not ptr
                &[
                    self.context
                        .ptr_type(inkwell::AddressSpace::default())
                        .into(),
                    self.context.i64_type().into(),
                ],
                false,
            );
            let fn_val = self.module.add_function("array_get", fn_type, None);
            self.function_value_map
                .insert("array_get".to_string(), fn_val);
            return fn_val;
        }

        if function.name == "array_set" {
            let fn_type = self.context.void_type().fn_type(
                &[
                    self.context
                        .ptr_type(inkwell::AddressSpace::default())
                        .into(),
                    self.context.i64_type().into(),
                    self.context.i64_type().into(), // Third param is i64, not ptr
                ],
                false,
            );
            let fn_val = self.module.add_function("array_set", fn_type, None);
            self.function_value_map
                .insert("array_set".to_string(), fn_val);
            return fn_val;
        }

        if function.name == "int_to_string" {
            let fn_type = self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .fn_type(
                    &[self.context.i64_type().into()], // Takes i64, not ptr
                    false,
                );
            let fn_val = self.module.add_function("int_to_string", fn_type, None);
            self.function_value_map
                .insert("int_to_string".to_string(), fn_val);
            return fn_val;
        }

        // Convert to BasicMetadataTypeEnum
        let param_types: Vec<BasicMetadataTypeEnum> = function
            .params
            .iter()
            .map(|(_, ty)| self.get_llvm_type(ty).into())
            .collect();

        let fn_type = if matches!(function.return_type, crate::ir::IRType::Void) {
            self.context.void_type().fn_type(&param_types, false)
        } else {
            let ret_type = self.get_llvm_type(&function.return_type);
            ret_type.fn_type(&param_types, false)
        };

        let fn_val = self.module.add_function(&function.name, fn_type, None);
        self.function_value_map
            .insert(function.name.clone(), fn_val);
        fn_val
    }

    fn store_value(&mut self, name: String, value: BasicValueEnum<'ctx>) {
        self.value_map.insert(name, value);
    }

    pub fn codegen_value(&mut self, val: &crate::ir::Value) -> BasicValueEnum<'ctx> {
        match val {
            crate::ir::Value::Constant(c) => self.codegen_constant(c),
            crate::ir::Value::Register(name) => self
                .value_map
                .get(name)
                .cloned()
                .unwrap_or_else(|| panic!("Register {} not found", name)),
            crate::ir::Value::Global(name) => {
                if let Some(global) = self.module.get_global(name) {
                    global.as_pointer_value().into()
                } else if let Some(func) = self.function_value_map.get(name) {
                    func.as_global_value().as_pointer_value().into()
                } else {
                    panic!("Global {} not found", name)
                }
            }
            crate::ir::Value::Argument(name) => self
                .value_map
                .get(name)
                .cloned()
                .unwrap_or_else(|| panic!("Function argument {} not found", name)),
        }
    }

    fn codegen_constant(&self, constant: &crate::ir::Constant) -> BasicValueEnum<'ctx> {
        match constant {
            crate::ir::Constant::Int(i) => {
                self.context.i64_type().const_int(*i as u64, true).into()
            }
            crate::ir::Constant::Bool(b) => {
                self.context.bool_type().const_int(*b as u64, false).into()
            }
            crate::ir::Constant::Float(f) => self.context.f64_type().const_float(*f).into(),
            crate::ir::Constant::String(s) => {
                let string_val = self.context.const_string(s.as_bytes(), true);
                let global = self.module.add_global(
                    string_val.get_type(),
                    None,
                    &format!("str_{}", s.len()),
                );
                global.set_initializer(&string_val);
                global.as_pointer_value().into()
            }
            crate::ir::Constant::Null => self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .const_null()
                .into(),
            crate::ir::Constant::Undef => self.context.i64_type().get_undef().into(),
            crate::ir::Constant::Void => {
                panic!("Void constant cannot be converted to BasicValueEnum")
            }
            crate::ir::Constant::I32(i) => {
                self.context.i32_type().const_int(*i as u64, true).into()
            }
        }
    }

    pub fn generate_module(&mut self, ir_module: &crate::ir::Module) {
        // Generate global string constants
        for (name, content) in &ir_module.global_strings {
            let string_val = self.context.const_string(content.as_bytes(), true);
            let global = self.module.add_global(string_val.get_type(), None, name);
            global.set_initializer(&string_val);
            global.set_linkage(inkwell::module::Linkage::Private);
            global.set_unnamed_addr(true);
        }

        // Declare all functions first
        for function in &ir_module.functions {
            self.declare_function(function);
        }

        // Generate function bodies (skip externals)
        for function in &ir_module.functions {
            if !function.is_external {
                self.codegen_function(function);
            }
        }
    }

    pub fn emit_to_file(&self, filename: &str) -> Result<(), String> {
        self.module
            .print_to_file(filename)
            .map_err(|e| format!("Failed to emit LLVM IR: {:?}", e))
    }

    pub fn get_module(&self) -> &LLVMModule<'ctx> {
        &self.module
    }
}
