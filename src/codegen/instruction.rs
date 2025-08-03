use crate::codegen::LLVMCodegen;

use inkwell::values::BasicMetadataValueEnum;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;

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
                // Store in value map
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
                    BasicTypeEnum::IntType(_) => {
                        // Use signed division for integers
                        self.builder
                            .build_int_signed_div(
                                lhs_val.into_int_value(),
                                rhs_val.into_int_value(),
                                dest,
                            )
                            .unwrap()
                            .into()
                    }
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
                    // Add other float predicates as needed
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

            // Function calls (already implemented above)
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

            // Reference counting operations
            Retain { ptr, .. } => {
                let ptr_val = self.codegen_value(ptr);
                let gc_retain_func = *self
                    .function_value_map
                    .get("gc_retain")
                    .expect("gc_retain not declared");
                self.builder
                    .build_call(gc_retain_func, &[ptr_val.into()], "")
                    .unwrap();
            }

            Release { ptr, .. } => {
                let ptr_val = self.codegen_value(ptr);
                let gc_release_func = *self
                    .function_value_map
                    .get("gc_release")
                    .expect("gc_release not declared");
                self.builder
                    .build_call(gc_release_func, &[ptr_val.into()], "")
                    .unwrap();
            }

            // GC operations - these become runtime calls
            GCAlloc {
                dest,
                object_type,
                size,
                ..
            } => {
                let gc_alloc_func = *self
                    .function_value_map
                    .get("gc_alloc")
                    .expect("gc_alloc not declared");

                let size_val = if let Some(sz) = size {
                    self.codegen_value(sz)
                } else {
                    self.context.i64_type().const_int(8, false).into() // Default size
                };

                // CRITICAL: Add the type_id argument
                let type_id = self.get_type_id_for_heap_object(object_type);
                let type_id_const = self.context.i32_type().const_int(type_id as u64, false);

                let allocated = self
                    .builder
                    .build_call(
                        gc_alloc_func,
                        &[size_val.into(), type_id_const.into()], // Pass BOTH arguments
                        dest,
                    )
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

                // Use context.ptr_type instead of type.ptr_type
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                let byte_ptr = self
                    .builder
                    .build_bit_cast(dest_ptr, ptr_type, "byte_ptr")
                    .unwrap()
                    .into_pointer_value(); // FIX: Convert to PointerValue

                // Store length at offset 0 - FIX: Convert length_ptr to PointerValue
                let length_val = self
                    .context
                    .i64_type()
                    .const_int(content.len() as u64, false);
                let length_ptr = self
                    .builder
                    .build_bit_cast(byte_ptr, ptr_type, "length_ptr")
                    .unwrap()
                    .into_pointer_value(); // FIX: Convert to PointerValue

                self.builder.build_store(length_ptr, length_val).unwrap();

                // Get data pointer (after length field) - FIX: Pass PointerValue to build_gep
                let data_offset = self.context.i64_type().const_int(8, false);
                let data_ptr = unsafe {
                    self.builder
                        .build_gep(
                            self.context.i8_type(),
                            byte_ptr, // This is already a PointerValue now
                            &[data_offset],
                            "data_ptr",
                        )
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
                let type_id = self.get_type_id_for_heap_object(element_type);
                let type_id_const = self.context.i32_type().const_int(type_id as u64, false);

                let array_concat_func = *self
                    .function_value_map
                    .get("array_concat")
                    .expect("array_concat not declared");
                let result = self
                    .builder
                    .build_call(
                        array_concat_func,
                        &[left_val.into(), right_val.into(), type_id_const.into()],
                        dest,
                    )
                    .unwrap();
                if let Some(return_value) = result.try_as_basic_value().left() {
                    self.store_value(dest.clone(), return_value);
                }
            }

            // Phi nodes
            Phi {
                dest, ty, incoming, ..
            } => {
                let phi_type = self.get_llvm_type(ty);
                let phi = self.builder.build_phi(phi_type, dest).unwrap();

                // Add incoming values (this will be filled in during a second pass)
                for (value, _label) in incoming {
                    let _val = self.codegen_value(value);
                    // Note: We'd need to resolve labels to basic blocks here
                    // This is typically done in a second pass
                }

                self.store_value(dest.clone(), phi.as_basic_value());
            }

            GCArrayNew {
                dest,
                element_type,
                length,
                ..
            } => {
                let length_val = self.codegen_value(length);
                let type_id = self.get_type_id_for_heap_object(element_type);
                let type_id_const = self.context.i32_type().const_int(type_id as u64, false);

                let gc_array_new_func = *self
                    .function_value_map
                    .get("gc_array_new")
                    .expect("gc_array_new not declared");
                let result = self
                    .builder
                    .build_call(
                        gc_array_new_func,
                        &[type_id_const.into(), length_val.into()],
                        dest,
                    )
                    .unwrap();

                if let Some(return_value) = result.try_as_basic_value().left() {
                    self.store_value(dest.clone(), return_value);
                }
            }

            GCArrayGet {
                dest, array, index, ..
            } => {
                let array_val = self.codegen_value(array);
                let index_val = self.codegen_value(index);

                let gc_array_get_func = *self
                    .function_value_map
                    .get("gc_array_get")
                    .expect("gc_array_get not declared");
                let result = self
                    .builder
                    .build_call(
                        gc_array_get_func,
                        &[array_val.into(), index_val.into()],
                        dest,
                    )
                    .unwrap();

                if let Some(return_value) = result.try_as_basic_value().left() {
                    self.store_value(dest.clone(), return_value);
                }
            }

            GCArraySet {
                array,
                index,
                value,
                ..
            } => {
                let array_val = self.codegen_value(array);
                let index_val = self.codegen_value(index);
                let value_val = self.codegen_value(value);

                let gc_array_set_func = *self
                    .function_value_map
                    .get("gc_array_set")
                    .expect("gc_array_set not declared");
                self.builder
                    .build_call(
                        gc_array_set_func,
                        &[array_val.into(), index_val.into(), value_val.into()],
                        "",
                    )
                    .unwrap();
            }

            Unbox {
                dest,
                boxed_ptr,
                primitive_type,
                ..
            } => {
                let ptr_val = self.codegen_value(boxed_ptr);
                let target_type = self.get_llvm_type(primitive_type);

                let loaded = self
                    .builder
                    .build_load(target_type, ptr_val.into_pointer_value(), dest)
                    .unwrap();

                self.store_value(dest.clone(), loaded);
            }

            _ => unimplemented!("Instruction {:?} not implemented yet", instr),
        }
    }
}
