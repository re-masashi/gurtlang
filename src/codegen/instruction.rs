use crate::codegen::LLVMCodegen;

use inkwell::values::BasicMetadataValueEnum;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;

use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn codegen_instruction(
        &mut self,
        instr: &crate::ir::Instruction,
        _current_function: FunctionValue<'ctx>,
    ) {
        use crate::ir::Instruction::*;
        match instr {
            // Arithmetic operations
            Add {
                dest, lhs, rhs, ty, ..
            } => {
                let lhs_val = self.codegen_value(lhs);
                let rhs_val = self.codegen_value(rhs);
                let res: BasicValueEnum = match self.get_llvm_type(ty) {
                    BasicTypeEnum::IntType(_) => self
                        .builder
                        .build_int_add(lhs_val.into_int_value(), rhs_val.into_int_value(), dest)
                        .unwrap()
                        .into(),
                    BasicTypeEnum::FloatType(_) => self
                        .builder
                        .build_float_add(
                            lhs_val.into_float_value(),
                            rhs_val.into_float_value(),
                            dest,
                        )
                        .unwrap()
                        .into(),
                    _ => panic!("Add with unsupported type {:?}", ty),
                };
                self.store_value(dest.clone(), res);
            }

            Sub {
                dest, lhs, rhs, ty, ..
            } => {
                let lhs_val = self.codegen_value(lhs);
                let rhs_val = self.codegen_value(rhs);
                let res: BasicValueEnum = match self.get_llvm_type(ty) {
                    BasicTypeEnum::IntType(_) => self
                        .builder
                        .build_int_sub(lhs_val.into_int_value(), rhs_val.into_int_value(), dest)
                        .unwrap()
                        .into(),
                    BasicTypeEnum::FloatType(_) => self
                        .builder
                        .build_float_sub(
                            lhs_val.into_float_value(),
                            rhs_val.into_float_value(),
                            dest,
                        )
                        .unwrap()
                        .into(),
                    _ => panic!("Sub with unsupported type {:?}", ty),
                };
                self.store_value(dest.clone(), res);
            }

            Mul {
                dest, lhs, rhs, ty, ..
            } => {
                let lhs_val = self.codegen_value(lhs);
                let rhs_val = self.codegen_value(rhs);
                let res: BasicValueEnum = match self.get_llvm_type(ty) {
                    BasicTypeEnum::IntType(_) => self
                        .builder
                        .build_int_mul(lhs_val.into_int_value(), rhs_val.into_int_value(), dest)
                        .unwrap()
                        .into(),
                    BasicTypeEnum::FloatType(_) => self
                        .builder
                        .build_float_mul(
                            lhs_val.into_float_value(),
                            rhs_val.into_float_value(),
                            dest,
                        )
                        .unwrap()
                        .into(),
                    _ => panic!("Mul with unsupported type {:?}", ty),
                };
                self.store_value(dest.clone(), res);
            }

            Div {
                dest, lhs, rhs, ty, ..
            } => {
                let lhs_val = self.codegen_value(lhs);
                let rhs_val = self.codegen_value(rhs);
                let res: BasicValueEnum = match self.get_llvm_type(ty) {
                    BasicTypeEnum::IntType(_) => self
                        .builder
                        .build_int_signed_div(
                            lhs_val.into_int_value(),
                            rhs_val.into_int_value(),
                            dest,
                        )
                        .unwrap()
                        .into(),
                    BasicTypeEnum::FloatType(_) => self
                        .builder
                        .build_float_div(
                            lhs_val.into_float_value(),
                            rhs_val.into_float_value(),
                            dest,
                        )
                        .unwrap()
                        .into(),
                    _ => panic!("Div with unsupported type {:?}", ty),
                };
                self.store_value(dest.clone(), res);
            }

            // Comparison operations
            ICmp {
                dest,
                cond,
                lhs,
                rhs,
                ..
            } => {
                let lhs_val = self.codegen_value(lhs);
                let rhs_val = self.codegen_value(rhs);
                let predicate = match cond {
                    crate::ir::ICmpCond::Eq => inkwell::IntPredicate::EQ,
                    crate::ir::ICmpCond::Ne => inkwell::IntPredicate::NE,
                    crate::ir::ICmpCond::Slt => inkwell::IntPredicate::SLT,
                    crate::ir::ICmpCond::Sle => inkwell::IntPredicate::SLE,
                    crate::ir::ICmpCond::Sgt => inkwell::IntPredicate::SGT,
                    crate::ir::ICmpCond::Sge => inkwell::IntPredicate::SGE,
                    crate::ir::ICmpCond::Ult => inkwell::IntPredicate::ULT,
                    crate::ir::ICmpCond::Ule => inkwell::IntPredicate::ULE,
                    crate::ir::ICmpCond::Ugt => inkwell::IntPredicate::UGT,
                    crate::ir::ICmpCond::Uge => inkwell::IntPredicate::UGE,
                };
                let res = self
                    .builder
                    .build_int_compare(
                        predicate,
                        lhs_val.into_int_value(),
                        rhs_val.into_int_value(),
                        dest,
                    )
                    .unwrap();
                self.store_value(dest.clone(), res.into());
            }

            FCmp {
                dest,
                cond,
                lhs,
                rhs,
                ..
            } => {
                let lhs_val = self.codegen_value(lhs);
                let rhs_val = self.codegen_value(rhs);
                let predicate = match cond {
                    crate::ir::FCmpCond::Oeq => inkwell::FloatPredicate::OEQ,
                    crate::ir::FCmpCond::One => inkwell::FloatPredicate::ONE,
                    crate::ir::FCmpCond::Ogt => inkwell::FloatPredicate::OGT,
                    crate::ir::FCmpCond::Oge => inkwell::FloatPredicate::OGE,
                    crate::ir::FCmpCond::Olt => inkwell::FloatPredicate::OLT,
                    crate::ir::FCmpCond::Ole => inkwell::FloatPredicate::OLE,
                    _ => inkwell::FloatPredicate::OEQ,
                };
                let res = self
                    .builder
                    .build_float_compare(
                        predicate,
                        lhs_val.into_float_value(),
                        rhs_val.into_float_value(),
                        dest,
                    )
                    .unwrap();
                self.store_value(dest.clone(), res.into());
            }

            // Memory operations
            Alloca { dest, ty, .. } => {
                let alloca_type = self.get_llvm_type(ty);
                let alloca = self.builder.build_alloca(alloca_type, dest).unwrap();
                self.store_value(dest.clone(), alloca.into());
            }

            Load { dest, ptr, ty, .. } => {
                let ptr_val = self.codegen_value(ptr);
                let load_type = self.get_llvm_type(ty);
                let loaded = self
                    .builder
                    .build_load(load_type, ptr_val.into_pointer_value(), dest)
                    .unwrap();
                self.store_value(dest.clone(), loaded);
            }

            Store { value, ptr, .. } => {
                let val = self.codegen_value(value);
                let ptr_val = self.codegen_value(ptr);
                self.builder
                    .build_store(ptr_val.into_pointer_value(), val)
                    .unwrap();
            }

            // Function calls
            Call {
                dest, func, args, ..
            } => {
                let func_val = match func {
                    crate::ir::Value::Global(name) => *self
                        .function_value_map
                        .get(name)
                        .expect("Function not declared"),
                    _ => unimplemented!("Non-global function calls"),
                };

                let llvm_args: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|arg| self.codegen_value(arg).into())
                    .collect();

                let call_site = self
                    .builder
                    .build_call(func_val, &llvm_args, dest.as_deref().unwrap_or("calltmp"))
                    .unwrap();

                if let Some(dest_name) = dest {
                    if let Some(return_value) = call_site.try_as_basic_value().left() {
                        self.store_value(dest_name.clone(), return_value);
                    }
                }
            }

            // GC allocation (simplified to just call GC_malloc)
            GCAlloc { dest, size, .. } => {
                let gc_malloc_func = *self
                    .function_value_map
                    .get("GC_malloc")
                    .expect("GC_malloc not declared");

                let size_val = if let Some(sz) = size {
                    self.codegen_value(sz)
                } else {
                    self.context.i64_type().const_int(8, false).into()
                };

                let allocated = self
                    .builder
                    .build_call(gc_malloc_func, &[size_val.into()], dest)
                    .unwrap();

                if let Some(return_value) = allocated.try_as_basic_value().left() {
                    self.store_value(dest.clone(), return_value);
                }
            }

            StringInit { dest, content, .. } => {
                let dest_val = self
                    .value_map
                    .get(dest)
                    .expect("String destination not found");
                let dest_ptr = dest_val.into_pointer_value();

                // Create global constant for the string content
                let string_content = self.context.const_string(content.as_bytes(), true);
                let global_content =
                    self.module
                        .add_global(string_content.get_type(), None, "str_literal");
                global_content.set_initializer(&string_content);
                global_content.set_linkage(inkwell::module::Linkage::Private);

                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                let byte_ptr = self
                    .builder
                    .build_bit_cast(dest_ptr, ptr_type, "byte_ptr")
                    .unwrap()
                    .into_pointer_value();

                // Store length at offset 0
                let length_val = self
                    .context
                    .i64_type()
                    .const_int(content.len() as u64, false);
                let length_ptr = self
                    .builder
                    .build_bit_cast(byte_ptr, ptr_type, "length_ptr")
                    .unwrap()
                    .into_pointer_value();

                self.builder.build_store(length_ptr, length_val).unwrap();

                // Get data pointer (after length field)
                let data_offset = self.context.i64_type().const_int(8, false);
                let data_ptr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), byte_ptr, &[data_offset], "data_ptr")
                        .unwrap()
                };

                // Copy string content using memcpy
                let memcpy_fn = self.get_or_declare_memcpy();
                self.builder
                    .build_call(
                        memcpy_fn,
                        &[
                            data_ptr.into(),
                            global_content.as_pointer_value().into(),
                            self.context
                                .i64_type()
                                .const_int(content.len() as u64, false)
                                .into(),
                        ],
                        "",
                    )
                    .unwrap();

                // Add null terminator
                let null_offset = self
                    .context
                    .i64_type()
                    .const_int(content.len() as u64, false);
                let null_ptr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), data_ptr, &[null_offset], "null_ptr")
                        .unwrap()
                };
                self.builder
                    .build_store(null_ptr, self.context.i8_type().const_int(0, false))
                    .unwrap();
            }

            // String operations
            StringConcat {
                dest, left, right, ..
            } => {
                let left_val = self.codegen_value(left);
                let right_val = self.codegen_value(right);
                let string_concat_func = *self
                    .function_value_map
                    .get("string_concat")
                    .expect("string_concat not declared");
                let result = self
                    .builder
                    .build_call(
                        string_concat_func,
                        &[left_val.into(), right_val.into()],
                        dest,
                    )
                    .unwrap();
                if let Some(return_value) = result.try_as_basic_value().left() {
                    self.store_value(dest.clone(), return_value);
                }
            }

            // Array operations
            ArrayConcat {
                dest,
                left,
                right,
                element_type,
                ..
            } => {
                let left_val = self.codegen_value(left);
                let right_val = self.codegen_value(right);
                let element_size = self.get_element_size_for_heap_object(element_type);
                let element_size_const = self.context.i64_type().const_int(element_size, false);

                let array_concat_func = *self
                    .function_value_map
                    .get("array_concat")
                    .expect("array_concat not declared");
                let result = self
                    .builder
                    .build_call(
                        array_concat_func,
                        &[left_val.into(), right_val.into(), element_size_const.into()],
                        dest,
                    )
                    .unwrap();
                if let Some(return_value) = result.try_as_basic_value().left() {
                    self.store_value(dest.clone(), return_value);
                }
            }

            ArrayNew {
                dest,
                element_type,
                length,
                ..
            } => {
                let length_val = self.codegen_value(length);
                let element_size = self.get_element_size_for_heap_object(element_type);
                let element_size_const = self.context.i64_type().const_int(element_size, false);

                let array_new_func = *self
                    .function_value_map
                    .get("array_new")
                    .expect("array_new not declared");
                let result = self
                    .builder
                    .build_call(
                        array_new_func,
                        &[element_size_const.into(), length_val.into()],
                        dest,
                    )
                    .unwrap();
                if let Some(return_value) = result.try_as_basic_value().left() {
                    self.store_value(dest.clone(), return_value);
                }
            }

            ArrayGet {
                dest, array, index, ..
            } => {
                let array_val = self.codegen_value(array);
                let index_val = self.codegen_value(index);
                let array_get_func = *self
                    .function_value_map
                    .get("array_get")
                    .expect("array_get not declared");
                let result = self
                    .builder
                    .build_call(array_get_func, &[array_val.into(), index_val.into()], dest)
                    .unwrap();
                if let Some(return_value) = result.try_as_basic_value().left() {
                    self.store_value(dest.clone(), return_value);
                }
            }

            ArraySet {
                array,
                index,
                value,
                ..
            } => {
                let array_val = self.codegen_value(array);
                let index_val = self.codegen_value(index);
                let value_val = self.codegen_value(value);
                let array_set_func = *self
                    .function_value_map
                    .get("array_set")
                    .expect("array_set not declared");
                self.builder
                    .build_call(
                        array_set_func,
                        &[array_val.into(), index_val.into(), value_val.into()],
                        "",
                    )
                    .unwrap();
            }

            ArrayLength { dest, array, .. } => {
                let array_val = self.codegen_value(array);
                let array_length_func = *self
                    .function_value_map
                    .get("array_length")
                    .expect("array_length not declared");
                let result = self
                    .builder
                    .build_call(array_length_func, &[array_val.into()], dest)
                    .unwrap();
                if let Some(return_value) = result.try_as_basic_value().left() {
                    self.store_value(dest.clone(), return_value);
                }
            }

            StringLength { dest, string, .. } => {
                let string_val = self.codegen_value(string);
                let string_length_func = *self
                    .function_value_map
                    .get("string_length")
                    .expect("string_length not declared");
                let result = self
                    .builder
                    .build_call(string_length_func, &[string_val.into()], dest)
                    .unwrap();
                if let Some(return_value) = result.try_as_basic_value().left() {
                    self.store_value(dest.clone(), return_value);
                }
            }

            // Struct operations
            StructNew {
                dest, struct_type, ..
            } => {
                // Get struct type info
                let struct_ir_type = crate::ir::IRType::Struct {
                    name: struct_type.clone(),
                    fields: vec![], // We'll need to look this up properly
                };
                let struct_llvm_type = self.get_llvm_type(&struct_ir_type);
                let struct_size = struct_llvm_type.size_of().unwrap();

                // Allocate using GC
                let gc_malloc_func = *self
                    .function_value_map
                    .get("GC_malloc")
                    .expect("GC_malloc not declared");
                let allocated = self
                    .builder
                    .build_call(gc_malloc_func, &[struct_size.into()], dest)
                    .unwrap();

                if let Some(return_value) = allocated.try_as_basic_value().left() {
                    // Bitcast to proper struct type
                    let struct_ptr = self
                        .builder
                        .build_bit_cast(
                            return_value.into_pointer_value(),
                            self.context.ptr_type(inkwell::AddressSpace::default()),
                            "struct_ptr",
                        )
                        .unwrap();
                    self.store_value(dest.clone(), struct_ptr);
                }
            }

            StructGet {
                dest,
                object,
                field,
                ..
            } => {
                let object_val = self.codegen_value(object);
                let object_ptr = object_val.into_pointer_value();

                // For now, assume field access by name using GEP
                // This is a simplified implementation - in reality you'd need
                // to look up field indices from the struct type
                let field_index = self.get_field_index(field); // Helper method needed
                let field_ptr = self
                    .builder
                    .build_struct_gep(
                        self.context.i8_type(), // Placeholder type
                        object_ptr,
                        field_index,
                        "field_ptr",
                    )
                    .unwrap();

                let loaded_field = self
                    .builder
                    .build_load(
                        self.context.i64_type(), // Placeholder type
                        field_ptr,
                        dest,
                    )
                    .unwrap();
                self.store_value(dest.clone(), loaded_field);
            }

            StructSet {
                object,
                field,
                value,
                ..
            } => {
                let object_val = self.codegen_value(object);
                let value_val = self.codegen_value(value);
                let object_ptr = object_val.into_pointer_value();

                let field_index = self.get_field_index(field);
                let field_ptr = self
                    .builder
                    .build_struct_gep(
                        self.context.i8_type(), // Placeholder type
                        object_ptr,
                        field_index,
                        "field_ptr",
                    )
                    .unwrap();

                self.builder.build_store(field_ptr, value_val).unwrap();
            }

            // Enum operations
            EnumCreate {
                dest,

                variant_name,
                variant_data,
                ..
            } => {
                // Simplified enum implementation - allocate space for tag + data
                let enum_size = 16; // 8 bytes tag + 8 bytes data pointer
                let gc_malloc_func = *self
                    .function_value_map
                    .get("GC_malloc")
                    .expect("GC_malloc not declared");

                let size_val = self.context.i64_type().const_int(enum_size, false);
                let allocated = self
                    .builder
                    .build_call(gc_malloc_func, &[size_val.into()], dest)
                    .unwrap();

                if let Some(enum_ptr) = allocated.try_as_basic_value().left() {
                    let ptr = enum_ptr.into_pointer_value();

                    // Store tag (simplified - use hash of variant name)
                    let tag_val = self
                        .context
                        .i64_type()
                        .const_int(self.hash_variant_name(variant_name), false);
                    let tag_ptr = self
                        .builder
                        .build_bit_cast(
                            ptr,
                            self.context.ptr_type(inkwell::AddressSpace::default()),
                            "tag_ptr",
                        )
                        .unwrap()
                        .into_pointer_value();
                    self.builder.build_store(tag_ptr, tag_val).unwrap();

                    // Store data if present
                    if let Some(data) = variant_data {
                        let data_val = self.codegen_value(data);
                        let data_offset = self.context.i64_type().const_int(8, false);
                        let data_ptr = unsafe {
                            self.builder
                                .build_gep(self.context.i8_type(), ptr, &[data_offset], "data_ptr")
                                .unwrap()
                        };
                        let typed_data_ptr = self
                            .builder
                            .build_bit_cast(
                                data_ptr,
                                self.context.ptr_type(inkwell::AddressSpace::default()),
                                "typed_data_ptr",
                            )
                            .unwrap()
                            .into_pointer_value();
                        self.builder.build_store(typed_data_ptr, data_val).unwrap();
                    }

                    self.store_value(dest.clone(), enum_ptr);
                }
            }

            EnumGetTag {
                dest, enum_value, ..
            } => {
                let enum_val = self.codegen_value(enum_value);
                let enum_ptr = enum_val.into_pointer_value();

                // Load tag from first 8 bytes
                let tag_ptr = self
                    .builder
                    .build_bit_cast(
                        enum_ptr,
                        self.context.ptr_type(inkwell::AddressSpace::default()),
                        "tag_ptr",
                    )
                    .unwrap()
                    .into_pointer_value();
                let tag_val = self
                    .builder
                    .build_load(self.context.i64_type(), tag_ptr, dest)
                    .unwrap();
                self.store_value(dest.clone(), tag_val);
            }

            EnumExtractData {
                dest,
                enum_value,
                variant_name: _,
                expected_type,
                ..
            } => {
                let enum_val = self.codegen_value(enum_value);
                let enum_ptr = enum_val.into_pointer_value();

                // Get data pointer (offset 8 bytes from start)
                let data_offset = self.context.i64_type().const_int(8, false);
                let data_ptr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), enum_ptr, &[data_offset], "data_ptr")
                        .unwrap()
                };

                // Cast to expected type and load
                let expected_llvm_type = self.get_llvm_type(expected_type);
                let typed_ptr = self
                    .builder
                    .build_bit_cast(
                        data_ptr,
                        self.context.ptr_type(inkwell::AddressSpace::default()),
                        "typed_ptr",
                    )
                    .unwrap()
                    .into_pointer_value();
                let loaded_data = self
                    .builder
                    .build_load(expected_llvm_type, typed_ptr, dest)
                    .unwrap();
                self.store_value(dest.clone(), loaded_data);
            }

            // Type conversions
            BitCast {
                dest, value, to_ty, ..
            } => {
                let val = self.codegen_value(value);
                let target_type = self.get_llvm_type(to_ty);
                let cast_val = self.builder.build_bit_cast(val, target_type, dest).unwrap();
                self.store_value(dest.clone(), cast_val);
            }

            Trunc {
                dest, value, to_ty, ..
            } => {
                let val = self.codegen_value(value);
                let target_type = self.get_llvm_type(to_ty);
                let trunc_val = self
                    .builder
                    .build_int_truncate(val.into_int_value(), target_type.into_int_type(), dest)
                    .unwrap();
                self.store_value(dest.clone(), trunc_val.into());
            }

            ZExt {
                dest, value, to_ty, ..
            } => {
                let val = self.codegen_value(value);
                let target_type = self.get_llvm_type(to_ty);
                let ext_val = self
                    .builder
                    .build_int_z_extend(val.into_int_value(), target_type.into_int_type(), dest)
                    .unwrap();
                self.store_value(dest.clone(), ext_val.into());
            }

            SExt {
                dest, value, to_ty, ..
            } => {
                let val = self.codegen_value(value);
                let target_type = self.get_llvm_type(to_ty);
                let ext_val = self
                    .builder
                    .build_int_s_extend(val.into_int_value(), target_type.into_int_type(), dest)
                    .unwrap();
                self.store_value(dest.clone(), ext_val.into());
            }

            // Pointer operations
            GetElementPtr {
                dest,
                ptr,
                indices,
                ty,
                ..
            } => {
                let ptr_val = self.codegen_value(ptr);
                let element_type = self.get_llvm_type(ty);

                let index_vals: Vec<inkwell::values::IntValue> = indices
                    .iter()
                    .map(|idx| self.codegen_value(idx).into_int_value())
                    .collect();

                let gep = unsafe {
                    self.builder
                        .build_gep(
                            element_type,
                            ptr_val.into_pointer_value(),
                            &index_vals,
                            dest,
                        )
                        .unwrap()
                };
                self.store_value(dest.clone(), gep.into());
            }

            ExtractValue {
                dest,
                aggregate,
                indices,
                ..
            } => {
                let agg_val = self.codegen_value(aggregate);
                let mut current_val = agg_val;

                for &index in indices {
                    current_val = self
                        .builder
                        .build_extract_value(
                            current_val.into_struct_value(),
                            index as u32,
                            "extract",
                        )
                        .unwrap();
                }
                self.store_value(dest.clone(), current_val);
            }

            InsertValue {
                dest,
                aggregate,
                value,
                indices,
                ..
            } => {
                let agg_val = self.codegen_value(aggregate);
                let val = self.codegen_value(value);
                let mut current_val = agg_val;

                // For simplicity, only handle single index
                if let Some(&index) = indices.first() {
                    let inserted = self
                        .builder
                        .build_insert_value(
                            current_val.into_struct_value(),
                            val,
                            index as u32,
                            dest,
                        )
                        .unwrap();

                    // Convert AggregateValueEnum back to BasicValueEnum
                    current_val = match inserted {
                        inkwell::values::AggregateValueEnum::ArrayValue(arr) => arr.into(),
                        inkwell::values::AggregateValueEnum::StructValue(s) => s.into(),
                    };
                }
                self.store_value(dest.clone(), current_val);
            }

            // Phi nodes are handled separately in the function generation
            Phi { .. } => {
                // Already handled in codegen_function
            }
        }
    }

    // Helper methods
    fn get_element_size_for_heap_object(&self, obj_type: &crate::ir::HeapObjectType) -> u64 {
        match obj_type {
            crate::ir::HeapObjectType::Array { .. } => 8, // Pointer size
            crate::ir::HeapObjectType::Struct { .. } => 8, // Simplified
            crate::ir::HeapObjectType::String { .. } => 1, // Byte
        }
    }

    fn get_field_index(&self, _field_name: &str) -> u32 {
        // Simplified - in reality you'd look this up from struct metadata
        0
    }

    fn hash_variant_name(&self, name: &str) -> u64 {
        // Simple hash for variant names
        let mut hash = 0u64;
        for byte in name.bytes() {
            hash = hash.wrapping_mul(31).wrapping_add(byte as u64);
        }
        hash
    }
}
