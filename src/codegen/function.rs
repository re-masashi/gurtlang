use crate::codegen::LLVMCodegen;

use inkwell::values::{FunctionValue, PhiValue};

use std::collections::HashMap;

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn codegen_function(&mut self, function: &crate::ir::Function) {
        let llvm_function = self.declare_function(function);
        self.current_function = Some(llvm_function);

        // Clear value map for this function
        self.value_map.clear();

        // Set up function parameters
        for (i, (param_name, _param_type)) in function.params.iter().enumerate() {
            if let Some(param_value) = llvm_function.get_nth_param(i as u32) {
                param_value.set_name(param_name);
                self.value_map.insert(param_name.clone(), param_value);
            }
        }

        // Create basic blocks first
        let mut block_map = HashMap::new();
        for ir_block in &function.blocks {
            let bb = self
                .context
                .append_basic_block(llvm_function, &ir_block.label);
            block_map.insert(ir_block.label.clone(), bb);
        }

        // Store phi nodes to fill later
        let mut phi_nodes: HashMap<String, (PhiValue<'ctx>, Vec<(crate::ir::Value, String)>)> =
            HashMap::new();

        // Generate instructions for each block
        for ir_block in &function.blocks {
            let bb = block_map[&ir_block.label];
            self.builder.position_at_end(bb);

            for instr in &ir_block.instructions {
                // Handle phi nodes specially - create them but don't fill yet
                if let crate::ir::Instruction::Phi {
                    dest, ty, incoming, ..
                } = instr
                {
                    let phi_type = self.get_llvm_type(ty);
                    let phi = self.builder.build_phi(phi_type, dest).unwrap();

                    // Store phi and its incoming data for later processing
                    phi_nodes.insert(dest.clone(), (phi, incoming.clone()));
                    self.value_map.insert(dest.clone(), phi.as_basic_value());
                } else {
                    self.codegen_instruction(instr, llvm_function);
                }
            }

            // Generate terminator
            if let Some(term) = &ir_block.terminator {
                self.codegen_terminator_with_blocks(term, &block_map);
            }
        }

        // SECOND PASS: Fill in phi node incoming values
        for (_dest, (phi, incoming)) in phi_nodes {
            for (value, label) in incoming {
                let val = self.codegen_value(&value);
                let bb = block_map
                    .get(&label)
                    .unwrap_or_else(|| panic!("Phi incoming block {} not found", label));
                phi.add_incoming(&[(&val, *bb)]);
            }
        }

        if !llvm_function.verify(true) {
            panic!("Function '{}' verification failed", function.name);
        }

        println!("✅ Function '{}' verified successfully", function.name);

        self.current_function = None;
    }

    pub fn codegen_terminator_with_blocks(
        &mut self,
        term: &crate::ir::Terminator,
        block_map: &HashMap<String, inkwell::basic_block::BasicBlock<'ctx>>,
    ) {
        use crate::ir::Terminator::*;
        match term {
            Ret {
                value: Some(val), ..
            } => {
                let ret_val = self.codegen_value(val);
                self.builder.build_return(Some(&ret_val)).unwrap();
            }
            Ret { value: None, .. } => {
                self.builder.build_return(None).unwrap();
            }
            Br { label, .. } => {
                let target_bb = block_map
                    .get(label)
                    .unwrap_or_else(|| panic!("Label block {} not found", label));
                self.builder.build_unconditional_branch(*target_bb).unwrap();
            }
            CondBr {
                cond,
                then_label,
                else_label,
                ..
            } => {
                let cond_val = self.codegen_value(cond);
                let then_bb = block_map
                    .get(then_label)
                    .unwrap_or_else(|| panic!("Then block {} not found", then_label));
                let else_bb = block_map
                    .get(else_label)
                    .unwrap_or_else(|| panic!("Else block {} not found", else_label));

                self.builder
                    .build_conditional_branch(cond_val.into_int_value(), *then_bb, *else_bb)
                    .unwrap();
            }
            Unreachable { .. } => {
                self.builder.build_unreachable().unwrap();
            }
        }
    }

    pub fn codegen_terminator(
        &mut self,
        term: crate::ir::Terminator,
        llvm_function: FunctionValue<'ctx>,
    ) {
        use crate::ir::Terminator::*;
        match term {
            Ret {
                value: Some(val), ..
            } => {
                let ret_val = self.codegen_value(&val);
                self.builder.build_return(Some(&ret_val)).unwrap();
            }
            Ret { value: None, .. } => {
                self.builder.build_return(None).unwrap();
            }
            Br { label, .. } => {
                // Fix the borrowing issue
                let basic_blocks = llvm_function.get_basic_blocks();
                let target_bb = basic_blocks
                    .iter()
                    .find(|bb| bb.get_name().to_str().unwrap() == label)
                    .expect("Label block not found");
                self.builder.build_unconditional_branch(*target_bb).unwrap();
            }
            CondBr {
                cond,
                then_label,
                else_label,
                ..
            } => {
                let cond_val = self.codegen_value(&cond);

                // Fix the borrowing issue
                let basic_blocks = llvm_function.get_basic_blocks();
                let then_bb = basic_blocks
                    .iter()
                    .find(|bb| bb.get_name().to_str().unwrap() == then_label)
                    .expect("Then block not found");
                let else_bb = basic_blocks
                    .iter()
                    .find(|bb| bb.get_name().to_str().unwrap() == else_label)
                    .expect("Else block not found");

                self.builder
                    .build_conditional_branch(cond_val.into_int_value(), *then_bb, *else_bb)
                    .unwrap();
            }
            Unreachable { .. } => {
                self.builder.build_unreachable().unwrap();
            }
        }
    }
}
