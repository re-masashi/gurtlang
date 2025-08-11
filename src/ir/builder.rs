use super::*;
use crate::ast::{
    BinOp, Type, TypedASTNode, TypedExpr, TypedExprKind, TypedExtern, TypedFunction, TypedStruct,
};
use std::collections::HashMap;
use std::ops::Range;
use std::sync::Arc;

pub struct IRBuilder {
    module: Module,
    current_function: Option<usize>,
    current_block: Option<usize>,
    register_counter: usize,
    label_counter: usize,
    symbol_table: HashMap<String, Value>,
}

impl IRBuilder {
    pub fn new(module_name: String) -> Self {
        let mut builder = Self {
            module: Module {
                name: module_name,
                functions: Vec::new(),
                struct_types: Vec::new(),
                global_strings: HashMap::new(),
            },
            current_function: None,
            current_block: None,
            register_counter: 0,
            label_counter: 0,
            symbol_table: HashMap::new(),
        };

        builder.add_standard_library();
        builder
    }

    fn add_standard_library(&mut self) {
        // printf
        self.module.functions.push(Function {
            name: "printf".to_string(),
            params: vec![("format".to_string(), IRType::Ptr)],
            return_type: IRType::I32,
            blocks: Vec::new(),
            is_external: true,
        });

        // Boehm GC functions
        self.module.functions.push(Function {
            name: "GC_malloc".to_string(),
            params: vec![("size".to_string(), IRType::I64)],
            return_type: IRType::Ptr,
            blocks: Vec::new(),
            is_external: true,
        });

        self.module.functions.push(Function {
            name: "GC_malloc_atomic".to_string(),
            params: vec![("size".to_string(), IRType::I64)],
            return_type: IRType::Ptr,
            blocks: Vec::new(),
            is_external: true,
        });

        // String operations
        self.module.functions.push(Function {
            name: "string_concat".to_string(),
            params: vec![
                ("left".to_string(), IRType::Ptr),
                ("right".to_string(), IRType::Ptr),
            ],
            return_type: IRType::Ptr,
            blocks: Vec::new(),
            is_external: true,
        });

        self.module.functions.push(Function {
            name: "string_length".to_string(),
            params: vec![("str".to_string(), IRType::Ptr)],
            return_type: IRType::I64,
            blocks: Vec::new(),
            is_external: true,
        });

        // Array operations
        self.module.functions.push(Function {
            name: "array_new".to_string(),
            params: vec![
                ("element_size".to_string(), IRType::I64),
                ("length".to_string(), IRType::I64),
            ],
            return_type: IRType::Ptr,
            blocks: Vec::new(),
            is_external: true,
        });

        self.module.functions.push(Function {
            name: "array_set".to_string(),
            params: vec![
                ("array".to_string(), IRType::Ptr),
                ("index".to_string(), IRType::I64),
                ("value".to_string(), IRType::I64),
            ],
            return_type: IRType::Void,
            blocks: Vec::new(),
            is_external: true,
        });

        self.module.functions.push(Function {
            name: "array_get".to_string(),
            params: vec![
                ("array".to_string(), IRType::Ptr),
                ("index".to_string(), IRType::I64),
            ],
            return_type: IRType::Ptr,
            blocks: Vec::new(),
            is_external: true,
        });

        self.module.functions.push(Function {
            name: "array_set".to_string(),
            params: vec![
                ("array".to_string(), IRType::Ptr),
                ("index".to_string(), IRType::I64),
                ("value".to_string(), IRType::Ptr),
            ],
            return_type: IRType::Void,
            blocks: Vec::new(),
            is_external: true,
        });

        self.module.functions.push(Function {
            name: "array_concat".to_string(),
            params: vec![
                ("left".to_string(), IRType::Ptr),
                ("right".to_string(), IRType::Ptr),
                ("element_size".to_string(), IRType::I64),
            ],
            return_type: IRType::Ptr,
            blocks: Vec::new(),
            is_external: true,
        });

        self.module.functions.push(Function {
            name: "array_length".to_string(),
            params: vec![("arr".to_string(), IRType::Ptr)],
            return_type: IRType::I64,
            blocks: Vec::new(),
            is_external: true,
        });

        // Type conversion functions
        self.module.functions.push(Function {
            name: "int_to_string".to_string(),
            params: vec![("value".to_string(), IRType::I64)],
            return_type: IRType::Ptr,
            blocks: Vec::new(),
            is_external: true,
        });

        self.module.functions.push(Function {
            name: "float_to_string".to_string(),
            params: vec![("value".to_string(), IRType::F64)],
            return_type: IRType::Ptr,
            blocks: Vec::new(),
            is_external: true,
        });

        self.module.functions.push(Function {
            name: "bool_to_string".to_string(),
            params: vec![("value".to_string(), IRType::I1)],
            return_type: IRType::Ptr,
            blocks: Vec::new(),
            is_external: true,
        });

        self.module.functions.push(Function {
            name: "print_internal".to_string(),
            params: vec![("message".to_string(), IRType::Ptr)],
            return_type: IRType::Void,
            blocks: Vec::new(),
            is_external: true,
        });
    }

    pub fn generate_module(&mut self, ast: Vec<TypedASTNode>) -> Module {
        let mut top_level_expressions = Vec::new();

        // First pass: collect struct types and function signatures
        for node in &ast {
            match node {
                TypedASTNode::Struct((struct_def, _)) => {
                    self.add_struct_type(struct_def);
                }
                TypedASTNode::Function((func, _)) => {
                    self.declare_function(func);
                }
                TypedASTNode::Extern((extern_func, _)) => {
                    self.declare_extern_function(extern_func);
                }
                TypedASTNode::Expr((expr, span)) => {
                    top_level_expressions.push((expr.clone(), span.clone()));
                }
                _ => {}
            }
        }

        // Second pass: generate function bodies
        for node in ast {
            if let TypedASTNode::Function((func, _)) = node {
                self.generate_function_body(&func);
            }
        }

        // Third pass: generate main function if there are top-level expressions
        if !top_level_expressions.is_empty() {
            self.generate_main_function(top_level_expressions);
        }

        std::mem::take(&mut self.module)
    }

    fn declare_extern_function(&mut self, extern_func: &TypedExtern) {
        let param_types: Vec<IRType> = extern_func
            .args
            .iter()
            .map(|(ty, _)| convert_ast_type_to_ir(ty))
            .collect();

        let return_type = convert_ast_type_to_ir(&extern_func.return_type.0);

        let ir_function = Function {
            name: extern_func.name.clone(),
            params: extern_func
                .args
                .iter()
                .enumerate()
                .zip(param_types.iter())
                .map(|((i, _), ir_ty)| (format!("arg{}", i), ir_ty.clone()))
                .collect(),
            return_type,
            blocks: Vec::new(),
            is_external: true,
        };

        self.module.functions.push(ir_function);
    }

    fn generate_main_function(&mut self, expressions: Vec<(TypedExpr, Range<usize>)>) {
        let main_function = Function {
            name: "main".to_string(),
            params: vec![],
            return_type: IRType::I32,
            blocks: Vec::new(),
            is_external: false,
        };

        self.module.functions.push(main_function);
        let func_idx = self.module.functions.len() - 1;
        self.current_function = Some(func_idx);

        let entry_block_idx = self.create_block("entry".to_string());
        self.set_current_block(entry_block_idx);

        self.symbol_table.clear();

        // Generate IR for each top-level expression
        for (expr, _) in expressions {
            match &expr.kind {
                TypedExprKind::Let { var, value, .. } => {
                    if let Some(val) = self.generate_expr(value) {
                        self.symbol_table.insert(var.clone(), val);
                    }
                }
                _ => {
                    let _val = self.generate_expr(&expr);
                }
            }
        }

        // Return 0 from main
        self.set_terminator(Terminator::Ret {
            value: Some(Value::Constant(Constant::I32(0))),
            span: 0..0,
        });

        self.current_function = None;
        self.current_block = None;
    }

    fn generate_expr(&mut self, expr: &TypedExpr) -> Option<Value> {
        match &expr.kind {
            // Stack-allocated primitives
            TypedExprKind::Int(val) => Some(Value::Constant(Constant::Int(*val))),
            TypedExprKind::Bool(val) => Some(Value::Constant(Constant::Bool(*val))),
            TypedExprKind::Float(val) => Some(Value::Constant(Constant::Float(*val))),

            // Heap-allocated string
            TypedExprKind::String(s) => {
                let result_reg = self.new_register();

                // Calculate size: sizeof(size_t) + string length + null terminator
                let total_size = std::mem::size_of::<usize>() + s.len() + 1;

                self.add_instruction(Instruction::GCAlloc {
                    dest: result_reg.clone(),
                    object_type: HeapObjectType::String { len: s.len() },
                    size: Some(Value::Constant(Constant::Int(total_size as i64))),
                    span: expr.range.clone(),
                });

                self.add_instruction(Instruction::StringInit {
                    dest: result_reg.clone(),
                    content: s.clone(),
                    span: expr.range.clone(),
                });

                Some(Value::Register(result_reg))
            }

            TypedExprKind::Let { var, value, .. } => {
                let val = self.generate_expr(value)?;
                self.symbol_table.insert(var.clone(), val.clone());
                Some(val)
            }

            TypedExprKind::Variable(name) => {
                let value = self.symbol_table.get(name).cloned();
                if let Some(val) = value {
                    Some(val)
                } else {
                    Some(Value::Global(name.clone()))
                }
            }

            TypedExprKind::BinOp {
                operator: op,
                l_value: left,
                r_value: right,
            } => {
                let left_val = self.generate_expr(left)?;
                let right_val = self.generate_expr(right)?;
                let result_reg = self.new_register();
                let result_ty = convert_ast_type_to_ir(&expr.ty);

                let instruction = match op {
                    BinOp::Add => {
                        match (&*left.ty, &*right.ty) {
                            // String concatenation
                            (
                                Type::Constructor { name: l_name, .. },
                                Type::Constructor { name: r_name, .. },
                            ) if l_name == "string" && r_name == "string" => {
                                Instruction::StringConcat {
                                    dest: result_reg.clone(),
                                    left: left_val,
                                    right: right_val,
                                    span: expr.range.clone(),
                                }
                            }
                            // Array concatenation
                            (
                                Type::Constructor {
                                    name: l_name,
                                    generics: l_gen,
                                    ..
                                },
                                Type::Constructor {
                                    name: r_name,
                                    generics: _r_gen,
                                    ..
                                },
                            ) if l_name == "List" && r_name == "List" => Instruction::ArrayConcat {
                                dest: result_reg.clone(),
                                left: left_val,
                                right: right_val,
                                element_type: if !l_gen.is_empty() {
                                    infer_heap_object_type(&l_gen[0])
                                } else {
                                    HeapObjectType::Struct {
                                        name: "unknown".to_string(),
                                        fields: vec![],
                                    }
                                },
                                span: expr.range.clone(),
                            },
                            // Numeric addition
                            _ => Instruction::Add {
                                dest: result_reg.clone(),
                                lhs: left_val,
                                rhs: right_val,
                                ty: result_ty,
                                span: expr.range.clone(),
                            },
                        }
                    }
                    BinOp::Sub => Instruction::Sub {
                        dest: result_reg.clone(),
                        lhs: left_val,
                        rhs: right_val,
                        ty: result_ty,
                        span: expr.range.clone(),
                    },
                    BinOp::Mul => Instruction::Mul {
                        dest: result_reg.clone(),
                        lhs: left_val,
                        rhs: right_val,
                        ty: result_ty,
                        span: expr.range.clone(),
                    },
                    BinOp::Div => Instruction::Div {
                        dest: result_reg.clone(),
                        lhs: left_val,
                        rhs: right_val,
                        ty: result_ty,
                        span: expr.range.clone(),
                    },
                    BinOp::Eq => Instruction::ICmp {
                        dest: result_reg.clone(),
                        cond: ICmpCond::Eq,
                        lhs: left_val,
                        rhs: right_val,
                        span: expr.range.clone(),
                    },
                    BinOp::NotEq => Instruction::ICmp {
                        dest: result_reg.clone(),
                        cond: ICmpCond::Ne,
                        lhs: left_val,
                        rhs: right_val,
                        span: expr.range.clone(),
                    },
                    BinOp::Less => Instruction::ICmp {
                        dest: result_reg.clone(),
                        cond: ICmpCond::Slt,
                        lhs: left_val,
                        rhs: right_val,
                        span: expr.range.clone(),
                    },
                    BinOp::LessEq => Instruction::ICmp {
                        dest: result_reg.clone(),
                        cond: ICmpCond::Sle,
                        lhs: left_val,
                        rhs: right_val,
                        span: expr.range.clone(),
                    },
                    BinOp::Greater => Instruction::ICmp {
                        dest: result_reg.clone(),
                        cond: ICmpCond::Sgt,
                        lhs: left_val,
                        rhs: right_val,
                        span: expr.range.clone(),
                    },
                    BinOp::GreaterEq => Instruction::ICmp {
                        dest: result_reg.clone(),
                        cond: ICmpCond::Sge,
                        lhs: left_val,
                        rhs: right_val,
                        span: expr.range.clone(),
                    },
                    _ => return None,
                };

                self.add_instruction(instruction);
                Some(Value::Register(result_reg))
            }

            TypedExprKind::IfElse {
                condition,
                if_branch,
                else_branch,
            } => self.generate_if_else(condition, if_branch, else_branch.as_deref(), &expr.ty),

            TypedExprKind::Call { function, args } => {
                let func_name = match &function.kind {
                    TypedExprKind::Variable(name) => name.clone(),
                    _ => return None,
                };

                let mut arg_vals = Vec::new();
                for arg in args {
                    if let Some(val) = self.generate_expr(arg) {
                        arg_vals.push(val);
                    }
                }

                let return_ty = convert_ast_type_to_ir(&expr.ty);
                let dest = if return_ty == IRType::Void {
                    None
                } else {
                    Some(self.new_register())
                };

                self.add_instruction(Instruction::Call {
                    dest: dest.clone(),
                    func: Value::Global(func_name),
                    args: arg_vals,
                    ty: return_ty,
                    span: expr.range.clone(),
                });

                dest.map(Value::Register)
            }

            // Dynamic array creation
            TypedExprKind::Array { elements } => {
                let result_reg = self.new_register();

                // Create array
                self.add_instruction(Instruction::ArrayNew {
                    dest: result_reg.clone(),
                    element_type: HeapObjectType::Array {
                        element_type: Box::new(HeapObjectType::Struct {
                            name: "int".to_string(),
                            fields: vec![],
                        }),
                        len: 0,
                        capacity: elements.len(),
                    },
                    length: Value::Constant(Constant::Int(elements.len() as i64)),
                    span: expr.range.clone(),
                });

                // Initialize elements
                for (i, element) in elements.iter().enumerate() {
                    if let Some(val) = self.generate_expr(element) {
                        self.add_instruction(Instruction::ArraySet {
                            array: Value::Register(result_reg.clone()),
                            index: Value::Constant(Constant::Int(i as i64)),
                            value: val,
                            span: element.range.clone(),
                        });
                    }
                }

                Some(Value::Register(result_reg))
            }

            TypedExprKind::EnumVariant {
                enum_name,
                variant_name,
                fields,
            } => {
                let result_reg = self.new_register();

                let variant_data = if fields.is_empty() {
                    None
                } else if fields.len() == 1 {
                    self.generate_expr(&fields[0].1)
                } else {
                    // Multiple fields - create tuple
                    let tuple_reg = self.new_register();
                    self.add_instruction(Instruction::StructNew {
                        dest: tuple_reg.clone(),
                        struct_type: format!("{}_{}_tuple", enum_name, variant_name),
                        span: expr.range.clone(),
                    });

                    // Set tuple fields
                    for (i, field) in fields.iter().enumerate() {
                        if let Some(field_val) = self.generate_expr(&field.1) {
                            self.add_instruction(Instruction::StructSet {
                                object: Value::Register(tuple_reg.clone()),
                                field: format!("field_{}", i),
                                value: field_val,
                                span: 0..0,
                            });
                        }
                    }

                    Some(Value::Register(tuple_reg))
                };

                // Create the enum
                self.add_instruction(Instruction::EnumCreate {
                    dest: result_reg.clone(),
                    enum_type: enum_name.clone(),
                    variant_name: variant_name.clone(),
                    variant_data,
                    span: expr.range.clone(),
                });

                Some(Value::Register(result_reg))
            }

            // Array indexing
            TypedExprKind::Index { array, index } => {
                let array_val = self.generate_expr(array)?;
                let index_val = self.generate_expr(index)?;
                let result_reg = self.new_register();

                self.add_instruction(Instruction::ArrayGet {
                    dest: result_reg.clone(),
                    array: array_val,
                    index: index_val,
                    span: expr.range.clone(),
                });

                Some(Value::Register(result_reg))
            }

            TypedExprKind::StructAccess {
                struct_val,
                field_name,
            } => {
                let object_val = self.generate_expr(struct_val)?;
                let result_reg = self.new_register();
                self.add_instruction(Instruction::StructGet {
                    dest: result_reg.clone(),
                    object: object_val,
                    field: field_name.clone(),
                    span: expr.range.clone(),
                });
                Some(Value::Register(result_reg))
            }

            _ => None,
        }
    }

    fn generate_if_else(
        &mut self,
        condition: &TypedExpr,
        if_branch: &TypedExpr,
        else_branch: Option<&TypedExpr>,
        result_ty: &Arc<Type>,
    ) -> Option<Value> {
        let cond_val = self.generate_expr(condition)?;

        let then_label = self.new_label("then");
        let else_label = self.new_label("else");
        let merge_label = self.new_label("merge");

        self.set_terminator(Terminator::CondBr {
            cond: cond_val,
            then_label: then_label.clone(),
            else_label: else_label.clone(),
            span: condition.range.clone(),
        });

        // Then block
        let then_block_idx = self.create_block(then_label.clone());
        self.set_current_block(then_block_idx);
        let then_val = self.generate_expr(if_branch);
        self.set_terminator(Terminator::Br {
            label: merge_label.clone(),
            span: if_branch.range.clone(),
        });

        // Else block
        let else_block_idx = self.create_block(else_label.clone());
        self.set_current_block(else_block_idx);
        let else_val = if let Some(else_expr) = else_branch {
            self.generate_expr(else_expr)
        } else {
            Some(Value::Constant(Constant::Void))
        };
        self.set_terminator(Terminator::Br {
            label: merge_label.clone(),
            span: else_branch.map(|e| e.range.clone()).unwrap_or_default(),
        });

        // Merge block
        let merge_block_idx = self.create_block(merge_label);
        self.set_current_block(merge_block_idx);

        // Create phi node if both branches produce values
        if let (Some(then_v), Some(else_v)) = (then_val, else_val) {
            let phi_reg = self.new_register();
            self.add_instruction(Instruction::Phi {
                dest: phi_reg.clone(),
                ty: convert_ast_type_to_ir(result_ty),
                incoming: vec![(then_v, then_label), (else_v, else_label)],
                span: 0..0,
            });
            Some(Value::Register(phi_reg))
        } else {
            None
        }
    }

    fn generate_function_body(&mut self, func: &TypedFunction) {
        let func_idx = self
            .module
            .functions
            .iter()
            .position(|f| f.name == func.name)
            .expect("Function should have been declared");

        self.current_function = Some(func_idx);

        let entry_label = "entry".to_string();
        let entry_block_idx = self.create_block(entry_label);
        self.set_current_block(entry_block_idx);

        self.symbol_table.clear();

        // Add function parameters to symbol table
        for (param_name, _param_ty, _) in &func.args {
            let param_value = Value::Argument(param_name.clone());
            self.symbol_table.insert(param_name.clone(), param_value);
        }

        // Generate IR for function body
        let body_value = self.generate_expr(&func.body.0);

        // Return the value
        if let Some(ret_val) = body_value {
            let ret_terminator = Terminator::Ret {
                value: Some(ret_val),
                span: func.body.1.clone(),
            };
            self.set_terminator(ret_terminator);
        } else {
            let ret_terminator = Terminator::Ret {
                value: None,
                span: func.body.1.clone(),
            };
            self.set_terminator(ret_terminator);
        }

        self.current_function = None;
        self.current_block = None;
    }

    fn new_register(&mut self) -> String {
        self.register_counter += 1;
        format!("%{}", self.register_counter)
    }

    fn new_label(&mut self, prefix: &str) -> String {
        self.label_counter += 1;
        format!("{}{}", prefix, self.label_counter)
    }

    fn add_instruction(&mut self, instruction: Instruction) {
        if let Some(func_idx) = self.current_function {
            if let Some(block_idx) = self.current_block {
                self.module.functions[func_idx].blocks[block_idx]
                    .instructions
                    .push(instruction);
            }
        }
    }

    fn set_terminator(&mut self, terminator: Terminator) {
        if let Some(func_idx) = self.current_function {
            if let Some(block_idx) = self.current_block {
                self.module.functions[func_idx].blocks[block_idx].terminator = Some(terminator);
            }
        }
    }

    fn create_block(&mut self, label: String) -> usize {
        if let Some(func_idx) = self.current_function {
            let block = BasicBlock {
                label,
                instructions: Vec::new(),
                terminator: None,
            };
            self.module.functions[func_idx].blocks.push(block);
            self.module.functions[func_idx].blocks.len() - 1
        } else {
            panic!("Cannot create block without current function");
        }
    }

    fn set_current_block(&mut self, block_idx: usize) {
        self.current_block = Some(block_idx);
    }

    fn add_struct_type(&mut self, struct_def: &TypedStruct) {
        let fields: Vec<IRType> = struct_def
            .fields
            .iter()
            .map(|(_, ty, _)| convert_ast_type_to_ir(ty))
            .collect();

        let struct_type = StructType {
            name: struct_def.name.clone(),
            fields: struct_def
                .fields
                .iter()
                .zip(fields.iter())
                .map(|((name, _, _), ir_ty)| (name.clone(), ir_ty.clone()))
                .collect(),
        };

        self.module.struct_types.push(struct_type);
    }

    fn declare_function(&mut self, func: &TypedFunction) {
        let param_types: Vec<IRType> = func
            .args
            .iter()
            .map(|(_, ty, _)| convert_ast_type_to_ir(ty))
            .collect();

        let return_type = convert_ast_type_to_ir(&func.return_type.0);

        let ir_function = Function {
            name: func.name.clone(),
            params: func
                .args
                .iter()
                .zip(param_types.iter())
                .map(|((name, _, _), ir_ty)| (name.clone(), ir_ty.clone()))
                .collect(),
            return_type,
            blocks: Vec::new(),
            is_external: false,
        };

        self.module.functions.push(ir_function);
    }
}

fn convert_ast_type_to_ir(ty: &Arc<Type>) -> IRType {
    match &**ty {
        Type::Unit => IRType::Void,
        Type::Constructor { name, .. } => match name.as_str() {
            "bool" => IRType::I1,
            "int" => IRType::I64,
            "float" => IRType::F64,
            "string" => IRType::Ptr,
            "List" => IRType::Ptr,
            _ => IRType::Ptr,
        },
        Type::Function {
            params,
            return_type,
        } => IRType::Function {
            params: params.iter().map(convert_ast_type_to_ir).collect(),
            return_type: Box::new(convert_ast_type_to_ir(return_type)),
        },
        Type::Tuple(_) => IRType::Ptr,
        _ => IRType::Ptr,
    }
}

fn infer_heap_object_type(ty: &Arc<Type>) -> HeapObjectType {
    match &**ty {
        Type::Constructor { name, generics, .. } => match name.as_str() {
            "int" => HeapObjectType::Struct {
                name: "int".to_string(),
                fields: vec![],
            },
            "string" => HeapObjectType::String { len: 0 },
            "List" => {
                let element_type = if let Some(elem_ty) = generics.first() {
                    Box::new(infer_heap_object_type(elem_ty))
                } else {
                    Box::new(HeapObjectType::Struct {
                        name: "unknown".to_string(),
                        fields: vec![],
                    })
                };

                HeapObjectType::Array {
                    element_type,
                    len: 0,
                    capacity: 0,
                }
            }
            _ => HeapObjectType::Struct {
                name: name.clone(),
                fields: vec![],
            },
        },
        _ => HeapObjectType::Struct {
            name: "unknown".to_string(),
            fields: vec![],
        },
    }
}
