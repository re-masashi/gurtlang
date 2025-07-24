#![allow(clippy::expect_fun_call)]

use std::collections::HashMap;
use std::fmt;
use std::ops::Range;
use std::sync::Arc;

use crate::ast::{
    Type, TypedASTNode, TypedEnum, TypedExpr, TypedExprKind, TypedExtern, TypedFunction,
    TypedStruct,
};

// IR Types with debug spans
type Span = Range<usize>;

#[derive(Debug, Clone, PartialEq)]
pub enum IRType {
    I1,  // Boolean
    I8,  // Byte
    I32, // 32-bit integer
    I64, // 64-bit integer
    F32, // 32-bit float
    F64, // 64-bit float
    Ptr,
    Struct(String, Span),
    Array(Box<IRType>, usize),
    Function(Vec<IRType>, Box<IRType>),
    Void,
}

impl fmt::Display for IRType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IRType::I1 => write!(f, "i1"),
            IRType::I8 => write!(f, "i8"),
            IRType::I32 => write!(f, "i32"),
            IRType::I64 => write!(f, "i64"),
            IRType::F32 => write!(f, "f32"),
            IRType::F64 => write!(f, "f64"),
            IRType::Ptr => write!(f, "ptr"),
            IRType::Struct(name, _) => write!(f, "struct %{}", name),
            IRType::Array(ty, size) => write!(f, "[{} x {}]", size, ty),
            IRType::Function(params, ret_ty) => {
                let params_str: Vec<String> = params.iter().map(|ty| format!("{}", ty)).collect();
                write!(f, "func ({}) -> {}", params_str.join(", "), ret_ty)
            }
            IRType::Void => write!(f, "void"),
        }
    }
}

// Values in SSA form with source spans
#[derive(Debug, Clone)]
pub enum Value {
    Constant(Constant),
    Register(String),
    Global(String),
}

// Implement Display for Value
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Constant(c) => write!(f, "{}", c),
            Value::Register(s) => write!(f, "%{}", s),
            Value::Global(s) => write!(f, "@{}", s),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Constant {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Null,
    Array(Vec<Constant>),
    Struct(String, Vec<Constant>),
    Undef,
}

// Implement Display for Constant
impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Int(i) => write!(f, "{}", i),
            Constant::Float(fl) => write!(f, "{}", fl),
            Constant::Bool(b) => write!(f, "{}", b),
            Constant::Null => write!(f, "null"),
            Constant::Array(elements) => {
                let elems_str: Vec<String> = elements.iter().map(|c| format!("{}", c)).collect();
                write!(f, "[{}]", elems_str.join(", "))
            }
            Constant::Struct(name, fields) => {
                let fields_str: Vec<String> = fields.iter().map(|c| format!("{}", c)).collect();
                write!(f, "struct %{}({})", name, fields_str.join(", "))
            }
            Constant::Undef => write!(f, "undef"),
            Constant::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

// Instructions with source spans
#[derive(Debug, Clone)]
pub enum Instruction {
    // Memory operations
    Alloca {
        dest: String,
        ty: IRType,
        span: Span,
    },
    Load {
        dest: String,
        ptr: Value,
        ty: IRType,
        span: Span,
    },
    Store {
        value: Value,
        ptr: Value,
        ty: IRType,
        span: Span,
    },

    // Arithmetic
    BinOp {
        dest: String,
        op: BinOp,
        lhs: Value,
        rhs: Value,
        ty: IRType,
        span: Span,
    },
    UnOp {
        dest: String,
        op: UnOp,
        operand: Value,
        ty: IRType,
        span: Span,
    },

    // Control flow
    Call {
        dest: Option<String>,
        func: Value,
        args: Vec<Value>,
        ty: IRType,
        span: Span,
    },
    Phi {
        dest: String,
        options: Vec<(Value, String)>,
        span: Span,
    },

    // Aggregates
    GetElementPtr {
        dest: String,
        ty: IRType,
        base: Value,
        base_ty: IRType,
        indices: Vec<Value>,
        span: Span,
    },
    InsertValue {
        dest: String,
        aggregate: Value,
        aggregate_ty: IRType,
        value: Value,
        value_ty: IRType,
        index: usize,
        span: Span,
    },
    ExtractValue {
        dest: String,
        aggregate: Value,
        index: usize,
        span: Span,
    },

    // Comparison
    ICmp {
        dest: String,
        cond: ICmpCond,
        lhs: Value,
        rhs: Value,
        span: Span,
    },
    FCmp {
        dest: String,
        cond: FCmpCond,
        lhs: Value,
        rhs: Value,
        span: Span,
    },
}

// Implement Display for Instruction
impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "    ")?; // Indent instructions
        match self {
            Instruction::Alloca { dest, ty, .. } => write!(f, "%{} = alloca {}", dest, ty),
            Instruction::Load { dest, ptr, ty, .. } => write!(f, "%{} = load {} {}", dest, ty, ptr),
            Instruction::Store { value, ptr, ty, .. } => {
                write!(f, "store {} {}, {} ", value, ty, ptr)
            }
            Instruction::BinOp {
                dest,
                op,
                lhs,
                rhs,
                ty,
                ..
            } => write!(f, "%{} = {} {} {}, {}", dest, op, ty, lhs, rhs),
            Instruction::UnOp {
                dest,
                op,
                operand,
                ty,
                ..
            } => write!(f, "%{} = {} {} {}", dest, op, ty, operand),
            Instruction::Call {
                dest,
                func,
                args,
                ty,
                ..
            } => {
                let args_str: Vec<String> = args.iter().map(|arg| format!("{}", arg)).collect();
                match dest {
                    Some(d) => write!(f, "%{} = call {} {}({})", d, ty, func, args_str.join(", ")),
                    None => write!(f, "call {} {}({})", ty, func, args_str.join(", ")),
                }
            }
            Instruction::Phi { dest, options, .. } => {
                let opts_str: Vec<String> = options
                    .iter()
                    .map(|(val, block)| format!("[{} : %{}]", val, block))
                    .collect();
                write!(f, "%{} = phi ({})", dest, opts_str.join(", "))
            }
            Instruction::GetElementPtr {
                dest,
                base,
                base_ty,
                indices,
                ..
            } => {
                let indices_str: Vec<String> =
                    indices.iter().map(|idx| format!("{}", idx)).collect();
                write!(
                    f,
                    "%{} = gep {}, {}, [{}]",
                    dest, base_ty, base, indices_str.join(", ")
                )
            }
            Instruction::InsertValue {
                dest,
                aggregate,
                aggregate_ty,
                value,
                value_ty,
                index,
                ..
            } => write!(
                f,
                "%{} = insertvalue {} {}, {} {} at index {}",
                dest, aggregate_ty, aggregate, value_ty, value, index
            ),
            Instruction::ExtractValue {
                dest,
                aggregate,
                index,
                ..
            } => write!(
                f,
                "%{} = extractvalue {} at index {}",
                dest, aggregate, index
            ),
            Instruction::ICmp {
                dest,
                cond,
                lhs,
                rhs,
                ..
            } => write!(f, "%{} = icmp {} {}, {}", dest, cond, lhs, rhs),
            Instruction::FCmp {
                dest,
                cond,
                lhs,
                rhs,
                ..
            } => write!(f, "%{} = fcmp {} {}, {}", dest, cond, lhs, rhs),
        }
    }
}

// Binary Operators (matching your AST)
#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem, // Arithmetic
    And,
    Or,
    Xor, // Bitwise
    Shl,
    Shr, // Shifts
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge, // Comparisons
}

// Implement Display for BinOp
impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "add"),
            BinOp::Sub => write!(f, "sub"),
            BinOp::Mul => write!(f, "mul"),
            BinOp::Div => write!(f, "div"),
            BinOp::Rem => write!(f, "rem"),
            BinOp::And => write!(f, "and"),
            BinOp::Or => write!(f, "or"),
            BinOp::Xor => write!(f, "xor"),
            BinOp::Shl => write!(f, "shl"),
            BinOp::Shr => write!(f, "shr"),
            BinOp::Eq => write!(f, "eq"),
            BinOp::Ne => write!(f, "ne"),
            BinOp::Lt => write!(f, "lt"),
            BinOp::Le => write!(f, "le"),
            BinOp::Gt => write!(f, "gt"),
            BinOp::Ge => write!(f, "ge"),
        }
    }
}

// Unary Operators (matching your AST)
#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Neg, // Arithmetic negation
    Not, // Bitwise/Logical not
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnOp::Neg => write!(f, "neg"),
            UnOp::Not => write!(f, "not"),
        }
    }
}

// Integer Comparisons
#[derive(Debug, Clone, Copy)]
pub enum ICmpCond {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

// Implement Display for ICmpCond
impl fmt::Display for ICmpCond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ICmpCond::Eq => write!(f, "eq"),
            ICmpCond::Ne => write!(f, "ne"),
            ICmpCond::Lt => write!(f, "slt"), // Signed less than
            ICmpCond::Le => write!(f, "sle"), // Signed less or equal
            ICmpCond::Gt => write!(f, "sgt"), // Signed greater than
            ICmpCond::Ge => write!(f, "sge"), // Signed greater or equal
        }
    }
}

// Float Comparisons
#[derive(Debug, Clone, Copy)]
pub enum FCmpCond {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    OEq,
    ONe,
    OLt,
    OLe,
    OGt,
    OGe,
}

// Implement Display for FCmpCond
impl fmt::Display for FCmpCond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FCmpCond::Eq => write!(f, "eq"),
            FCmpCond::Ne => write!(f, "ne"),
            FCmpCond::Lt => write!(f, "lt"),
            FCmpCond::Le => write!(f, "le"),
            FCmpCond::Gt => write!(f, "gt"),
            FCmpCond::Ge => write!(f, "ge"),
            FCmpCond::OEq => write!(f, "oeq"),
            FCmpCond::ONe => write!(f, "one"),
            FCmpCond::OLt => write!(f, "olt"),
            FCmpCond::OLe => write!(f, "ole"),
            FCmpCond::OGt => write!(f, "ogt"),
            FCmpCond::OGe => write!(f, "oge"),
        }
    }
}

// Basic Blocks with span information
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub label: String,
    pub instructions: Vec<Instruction>,
    pub terminator: Option<Terminator>,
    pub span: Span,
}

// Implement Display for BasicBlock
impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}:", self.label)?;
        for instr in &self.instructions {
            writeln!(f, "{}", instr)?;
        }
        if let Some(term) = &self.terminator {
            writeln!(f, "    {}", term)?; // Indent terminator
        }
        Ok(())
    }
}

// Block Terminators with spans
#[derive(Debug, Clone)]
pub enum Terminator {
    Ret(Option<Value>, Span),
    BrCond {
        cond: Value,
        then_label: String,
        else_label: String,
        span: Span,
    },
    BrUncond(String, Span),
    Switch {
        value: Value,
        cases: Vec<(Constant, String)>,
        default: String,
        span: Span,
    },
    Unreachable(Span),
}

// Implement Display for Terminator
impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Terminator::Ret(Some(val), _) => write!(f, "ret {}", val),
            Terminator::Ret(None, _) => write!(f, "ret void"),
            Terminator::BrCond {
                cond,
                then_label,
                else_label,
                ..
            } => write!(
                f,
                "br {} label %{}, label %{}",
                cond, then_label, else_label
            ),
            Terminator::BrUncond(label, _) => write!(f, "br label %{}", label),
            Terminator::Switch {
                value,
                cases,
                default,
                ..
            } => {
                write!(f, "switch {} ", value)?;
                write!(f, "[")?;
                let cases_str: Vec<String> = cases
                    .iter()
                    .map(|(val, label)| format!("{} : %{}", val, label))
                    .collect();
                write!(f, "{}] default %{}", cases_str.join(", "), default)
            }
            Terminator::Unreachable(_) => write!(f, "unreachable"),
        }
    }
}

// Functions with comprehensive span tracking
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<(String, IRType)>, // (name, type)
    pub return_type: IRType,
    pub basic_blocks: Vec<BasicBlock>,
    pub is_entry: bool,
    pub span: Span,
    pub is_extern: bool,
}

// Implement Display for Function
impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params_str: Vec<String> = self
            .params
            .iter()
            .map(|(name, ty)| {
                if self.is_extern {
                    format!("{}", ty)
                } else {
                    format!("{} %{}", ty, name)
                }
            })
            .collect();
        let entry_keyword = if self.is_entry { "entry" } else { "" };
        if self.is_extern {
            writeln!(
                f,
                "extern {} @{}({})",
                self.return_type,
                self.name,
                params_str.join(", ")
            )
        } else {
            writeln!(
                f,
                "define {} {} @{}({}) {{",
                entry_keyword,
                self.return_type,
                self.name,
                params_str.join(", ")
            )?;
            for block in &self.basic_blocks {
                write!(f, "{}", block)?;
            }
            writeln!(f, "}}")
        }
    }
}

// Data Types with debug spans
#[derive(Debug)]
pub struct StructType {
    pub name: String,
    pub fields: Vec<(String, IRType)>, // (field name, type)
    pub span: Span,
}

// Implement Display for StructType
impl fmt::Display for StructType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fields_str: Vec<String> = self
            .fields
            .iter()
            .map(|(name, ty)| format!("{} %{}", ty, name))
            .collect();
        writeln!(f, "struct %{} {{ {} }}", self.name, fields_str.join(", "))
    }
}

#[derive(Debug)]
pub struct EnumType {
    pub name: String,
    pub tag_type: IRType,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}

// Implement Display for EnumType
impl fmt::Display for EnumType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "enum %{} tag:{} {{", self.name, self.tag_type)?;
        for variant in &self.variants {
            writeln!(f, "    {}", variant)?;
        }
        writeln!(f, "}}")
    }
}

#[derive(Debug)]
pub struct EnumVariant {
    pub name: String,
    pub tag: i32,
    pub data_type: Option<IRType>,
    pub span: Span,
}

// Implement Display for EnumVariant
impl fmt::Display for EnumVariant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "variant {} = {}", self.name, self.tag)?;
        if let Some(ty) = &self.data_type {
            write!(f, " data: {}", ty)?;
        }
        Ok(())
    }
}

// Global Module with spans
#[derive(Debug)]
pub struct Module {
    pub functions: Vec<Function>,
    pub struct_types: Vec<StructType>,
    pub enum_types: Vec<EnumType>,
    pub global_vars: Vec<GlobalVariable>,
}

// Implement Display for Module
impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for struct_ty in &self.struct_types {
            writeln!(f, "{}", struct_ty)?;
        }
        if !self.struct_types.is_empty() {
            writeln!(f)?;
        }

        for enum_ty in &self.enum_types {
            writeln!(f, "{}", enum_ty)?;
        }
        if !self.enum_types.is_empty() {
            writeln!(f)?;
        }

        for global_var in &self.global_vars {
            writeln!(f, "{}", global_var)?;
        }
        if !self.global_vars.is_empty() {
            writeln!(f)?;
        }

        for func in &self.functions {
            writeln!(f, "{}", func)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct GlobalVariable {
    pub name: String,
    pub ty: IRType,
    pub init: Option<Constant>,
    pub span: Span,
}

// Implement Display for GlobalVariable
impl fmt::Display for GlobalVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "global @{} {}", self.name, self.ty)?;
        if let Some(init) = &self.init {
            write!(f, " = {}", init)?;
        }
        Ok(())
    }
}

/// Generates an IR `Module` from a monomorphized `TypedAST`.
pub struct IRGenerator {
    module: Module,
    top_level_expressions: Vec<TypedExpr>,
    constructors: Vec<String>,
    // global_string_count: usize,
}

impl Default for IRGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Manages the state for generating a single function.
struct FunctionGenerator<'a> {
    type_converter: &'a IRGenerator,
    function: Function,
    symbol_table: HashMap<String, Value>, // Maps variable names to their stack pointers (from alloca)
    current_block: Option<String>,
    block_map: HashMap<String, BasicBlock>,
    register_count: u32,
    block_count: u32,
    global_string_count: usize,
}

impl IRGenerator {
    /// Creates a new `IRGenerator`.
    pub fn new() -> Self {
        IRGenerator {
            module: Module {
                functions: vec![],
                struct_types: vec![],
                enum_types: vec![],
                global_vars: vec![],
            },
            top_level_expressions: vec![],
            constructors: vec![],
            // global_string_count: 0,
        }
    }

    /// Consumes the generator and returns the completed IR module.
    pub fn build(mut self) -> Module {
        if !self.top_level_expressions.is_empty() {
            self.generate_main_function();
        }
        self.module
    }

    /// The main entry point to generate IR from the AST.
    pub fn generate(&mut self, ast: Vec<TypedASTNode>) {
        for node in ast {
            match node {
                TypedASTNode::Function((func, _span)) => {
                    if func.is_constructor {
                        self.constructors.push(func.name.clone());
                        continue;
                    }
                    self.generate_function(func)
                }
                TypedASTNode::Struct((struct_def, _span)) => self.generate_struct_def(struct_def),
                TypedASTNode::Enum((enum_def, _span)) => self.generate_enum_def(enum_def),
                TypedASTNode::Expr((expr, _span)) => {
                    self.top_level_expressions.push(expr);
                }
                TypedASTNode::Error => {
                    // Ignore errors that made it this far
                }
                TypedASTNode::Extern((e, _span)) => self.generate_extern(e),
            }
        }
    }

    /// Generates a function definition.
    fn generate_function(&mut self, typed_func: TypedFunction) {
        if typed_func.is_constructor {
            self.generate_constructor(typed_func);
            return;
        }
        let return_type = convert_type(&typed_func.return_type.0);
        let params: Vec<(String, IRType)> = typed_func
            .args
            .iter()
            .map(|(name, ty, _)| (name.clone(), convert_type(ty)))
            .collect();

        let mut func_gen = FunctionGenerator::new(self, typed_func.name.clone());

        func_gen.function.return_type = return_type;
        func_gen.function.params = params.clone();

        // Entry block setup
        let entry_label = func_gen.new_label("entry");
        func_gen.set_current_block(entry_label);

        // Allocate space for parameters and store their initial values
        for (i, (name, ir_ty)) in params.iter().enumerate() {
            let ptr_reg = func_gen.new_register();
            let alloca = Instruction::Alloca {
                dest: ptr_reg.clone(),
                ty: ir_ty.clone(),
                span: typed_func.body.1.clone(),
            };
            func_gen.add_instruction(alloca);

            let param_val = Value::Register(format!("%param.{i}.{name}")); // Assuming params are passed in registers
            let store = Instruction::Store {
                value: param_val,
                ptr: Value::Register(ptr_reg.clone()),
                ty: ir_ty.clone(),
                span: typed_func.args[i].2.clone(),
            };
            func_gen.add_instruction(store);

            func_gen
                .symbol_table
                .insert(name.clone(), Value::Register(ptr_reg));
        }

        // Generate IR for the function body
        let (body_expr, body_span) = *typed_func.body;
        let last_val = func_gen.generate_expr(&body_expr);

        // Add a return terminator if one isn't already present
        if func_gen.get_current_block().terminator.is_none() {
            match last_val {
                Some(val) => func_gen.terminate_block(Terminator::Ret(Some(val), body_span)),
                None => func_gen.terminate_block(Terminator::Ret(None, body_span)),
            }
        }

        // Finalize the function and add it to the module
        self.module.functions.push(func_gen.finalize());
    }

    /// Generates an extern definition.
    fn generate_extern(&mut self, typed_extern: TypedExtern) {
        let return_type = convert_type(&typed_extern.return_type.0);
        let params: Vec<(String, IRType)> = typed_extern
            .args
            .iter()
            .enumerate()
            .map(|(i, (ty, _span))| {
                // Generate parameter names since externs don't have them
                (format!("arg{}", i), convert_type(ty))
            })
            .collect();

        // Create an extern function (without body)
        self.module.functions.push(Function {
            name: typed_extern.name,
            params,
            return_type,
            basic_blocks: Vec::new(),
            is_entry: false,
            is_extern: true, // Mark as extern
            span: 0..0,      // Span not available in TypedExtern
        });
    }

    /// Generates a constructor definition
    fn generate_constructor(&mut self, typed_func: TypedFunction) {
        let return_type = convert_type(&typed_func.return_type.0);
        let params: Vec<(String, IRType)> = typed_func
            .args
            .iter()
            .map(|(name, ty, _)| (name.clone(), convert_type(ty)))
            .collect();

        let mut func_gen = FunctionGenerator::new(self, typed_func.name.clone());
        func_gen.function.return_type = return_type.clone();
        func_gen.function.params = params.clone();

        // Entry block setup
        let entry_label = func_gen.new_label("entry");
        func_gen.set_current_block(entry_label);

        // For constructors, build the struct directly
        let mut current_value = None;
        for (i, (name, _)) in params.iter().enumerate() {
            let param_val = func_gen.symbol_table[name].clone();
            if let Some(val) = current_value {
                let dest = func_gen.new_register();
                func_gen.add_instruction(Instruction::InsertValue {
                    dest: dest.clone(),
                    aggregate: val,
                    aggregate_ty: return_type.clone(),
                    value: param_val,
                    value_ty: params[i].1.clone(),
                    index: i,
                    span: typed_func.body.1.clone(),
                });
                current_value = Some(Value::Register(dest));
            } else {
                // First field
                current_value = Some(param_val);
            }
        }

        // Return the constructed struct
        if let Some(final_value) = current_value {
            func_gen.terminate_block(Terminator::Ret(Some(final_value), typed_func.body.1));
        } else {
            // Empty struct case
            func_gen.terminate_block(Terminator::Ret(None, typed_func.body.1));
        }

        self.module.functions.push(func_gen.finalize());
    }

    /// Generates a struct type definition.
    fn generate_struct_def(&mut self, typed_struct: TypedStruct) {
        let fields = typed_struct
            .fields
            .into_iter()
            .map(|(name, ty, _)| (name, convert_type(&ty)))
            .collect();

        self.module.struct_types.push(StructType {
            name: typed_struct.name,
            fields,
            span: 0..0, // Spans can be added here
        });
    }

    /// Generates an enum type definition.
    fn generate_enum_def(&mut self, typed_enum: TypedEnum) {
        // Enums often become a struct: { tag, payload }
        // This is a simplified representation.
        println!(
            "Warning: Enum '{}' IR generation is a placeholder.",
            typed_enum.name
        );
    }

    /// Generates the implicit `main` function from top-level expressions.
    fn generate_main_function(&mut self) {
        let top_level_expressions = std::mem::take(&mut self.top_level_expressions);
        let mut func_gen = FunctionGenerator::new(self, "main".to_string());
        func_gen.function.is_entry = true;
        func_gen.function.return_type = IRType::I32;
        func_gen.function.span = 0..0;

        let entry_label = func_gen.new_label("entry");
        func_gen.set_current_block(entry_label);

        let mut last_expr_span = 0..0;

        for expr in top_level_expressions {
            last_expr_span = expr.range.clone();
            func_gen.generate_expr(&expr);
        }

        if func_gen.current_block.is_some() {
            func_gen.terminate_block(Terminator::Ret(
                Some(Value::Constant(Constant::Int(0))),
                last_expr_span,
            ));
        }

        self.module.functions.push(func_gen.finalize());
    }
}

impl<'a> FunctionGenerator<'a> {
    fn new(type_converter: &'a IRGenerator, name: String) -> Self {
        FunctionGenerator {
            type_converter, // Use the new immutable reference
            function: Function {
                name,
                params: Vec::new(),
                return_type: IRType::Void,
                basic_blocks: Vec::new(),
                is_entry: false, // Set for `main` or program entry
                span: 0..0,
                is_extern: false,
            },
            symbol_table: HashMap::new(),
            current_block: None,
            block_map: HashMap::new(),
            register_count: 0,
            block_count: 0,
            global_string_count: 0,
        }
    }

    /// Generates a fresh register name.
    fn new_register(&mut self) -> String {
        let i = self.register_count;
        self.register_count += 1;
        format!("{}", i)
    }

    /// Generates a fresh basic block label.
    fn new_label(&mut self, prefix: &str) -> String {
        let i = self.block_count;
        self.block_count += 1;
        format!("{}.{}", prefix, i)
    }

    /// Adds an instruction to the current basic block.
    fn add_instruction(&mut self, instr: Instruction) {
        self.get_current_block_mut().instructions.push(instr);
    }

    /// Switches to a new basic block, creating it if necessary.
    // In FunctionGenerator struct
    fn set_current_block(&mut self, label: String) {
        // Remove and store the old block ONLY if it hasn't been terminated
        if let Some(ref old_label) = self.current_block {
            if let Some(old_block) = self.block_map.remove(old_label) {
                self.function.basic_blocks.push(old_block);
            }
        }

        // Get or create the new block
        self.block_map
            .entry(label.clone())
            .or_insert_with(|| BasicBlock {
                label: label.clone(),
                instructions: Vec::new(),
                terminator: None,
                span: 0..0,
            });

        self.current_block = Some(label);
    }

    /// Finalizes the function by collecting all basic blocks.
    fn finalize(mut self) -> Function {
        for block in &self.function.basic_blocks {
            if block.terminator.is_none() {
                panic!("Block {} has no terminator", block.label);
            }
        }

        // Add current block if it exists
        if let Some(label) = self.current_block.take() {
            if let Some(block) = self.block_map.remove(&label) {
                self.function.basic_blocks.push(block);
            }
        }

        // Add ALL remaining blocks from block_map
        for (_, block) in self.block_map.into_iter() {
            self.function.basic_blocks.push(block);
        }

        self.function
    }

    fn get_current_block(&self) -> &BasicBlock {
        self.block_map
            .get(self.current_block.as_ref().unwrap())
            .unwrap()
    }

    fn get_current_block_mut(&mut self) -> &mut BasicBlock {
        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
    }

    /// Sets the terminator for the current block and resets context.
    fn terminate_block(&mut self, terminator: Terminator) {
        let block = self.get_current_block_mut();
        if block.terminator.is_none() {
            block.terminator = Some(terminator);
        }
        self.current_block = None;
    }

    /// Recursively generates IR for an expression.
    fn generate_expr(&mut self, expr: &TypedExpr) -> Option<Value> {
        let span = expr.range.clone();
        match &expr.kind {
            TypedExprKind::Int(val) => Some(Value::Constant(Constant::Int(*val))),
            TypedExprKind::Bool(val) => Some(Value::Constant(Constant::Bool(*val))),
            TypedExprKind::Float(val) => Some(Value::Constant(Constant::Float(*val))),

            TypedExprKind::Variable(name) => {
                let dest = self.new_register();
                // Check if it's a local variable
                if let Some(ptr_val) = self.symbol_table.get(name) {
                    // Local variable: load from pointer
                    let ty = convert_type(&expr.ty);
                    self.add_instruction(Instruction::Load {
                        dest: dest.clone(),
                        ptr: ptr_val.clone(),
                        ty,
                        span,
                    });
                    Some(Value::Register(dest))
                } else {
                    // Global function reference
                    Some(Value::Global(name.clone()))
                }
            }

            TypedExprKind::Let { var, value } => {
                let val = self
                    .generate_expr(value)
                    .expect("Let expression must have a value");
                let ty = convert_type(&value.ty);

                let dest_ptr = self.new_register();
                self.add_instruction(Instruction::Alloca {
                    dest: dest_ptr.clone(),
                    ty: ty.clone(),
                    span: value.range.clone(),
                });

                self.add_instruction(Instruction::Store {
                    value: val,
                    ptr: Value::Register(dest_ptr.clone()),
                    ty,
                    span,
                });

                self.symbol_table
                    .insert(var.clone(), Value::Register(dest_ptr));
                None // `let` statements produce no value
            }

            TypedExprKind::BinOp {
                operator,
                l_value,
                r_value,
            } => {
                let lhs = self.generate_expr(l_value).unwrap();
                let rhs = self.generate_expr(r_value).unwrap();
                let dest = self.new_register();
                let ty = convert_type(&expr.ty);

                // This requires a mapping from AST BinOp to IR BinOp/ICmpCond
                // This is a simplified example.
                let op = match operator {
                    crate::ast::BinOp::Add => crate::ir::BinOp::Add,
                    crate::ast::BinOp::Sub => crate::ir::BinOp::Sub,
                    crate::ast::BinOp::Mul => crate::ir::BinOp::Mul,
                    crate::ast::BinOp::Div => crate::ir::BinOp::Div,
                    crate::ast::BinOp::Eq => {
                        self.add_instruction(Instruction::ICmp {
                            dest: dest.clone(),
                            cond: ICmpCond::Eq,
                            lhs,
                            rhs,
                            span,
                        });
                        return Some(Value::Register(dest));
                    }
                    // ... other operators
                    _ => unimplemented!("Binary operator {:?} not supported", operator),
                };

                self.add_instruction(Instruction::BinOp {
                    dest: dest.clone(),
                    op,
                    lhs,
                    rhs,
                    ty,
                    span,
                });
                Some(Value::Register(dest))
            }

            TypedExprKind::IfElse {
                condition,
                if_branch,
                else_branch,
            } => {
                let cond_val = self.generate_expr(condition).unwrap();

                let then_label = self.new_label("then");
                let else_label = self.new_label("else");
                let merge_label = self.new_label("if.cont");

                // Conditional branch from the current block
                self.terminate_block(Terminator::BrCond {
                    cond: cond_val,
                    then_label: then_label.clone(),
                    else_label: else_label.clone(),
                    span: condition.range.clone(),
                });

                // Generate `then` block
                self.set_current_block(then_label.clone());
                let then_val = self.generate_expr(if_branch).unwrap();
                // Avoid terminating if the branch itself did (e.g., via a return)
                if self.current_block.is_some() {
                    self.terminate_block(Terminator::BrUncond(
                        merge_label.clone(),
                        if_branch.range.clone(),
                    ));
                }

                // Generate `else` block
                self.set_current_block(else_label.clone());
                let else_val = self.generate_expr(else_branch.as_ref().unwrap()).unwrap();
                if self.current_block.is_some() {
                    self.terminate_block(Terminator::BrUncond(
                        merge_label.clone(),
                        else_branch.as_ref().unwrap().range.clone(),
                    ));
                }

                // Start the merge block and add a Phi node
                self.set_current_block(merge_label);
                let dest = self.new_register();
                let phi = Instruction::Phi {
                    dest: dest.clone(),
                    options: vec![(then_val, then_label), (else_val, else_label)],
                    span,
                };
                self.add_instruction(phi);

                Some(Value::Register(dest))
            }

            TypedExprKind::Do { expressions } => {
                let mut last_val = None;
                for expr in expressions {
                    last_val = self.generate_expr(expr);
                }
                last_val
            }

            TypedExprKind::Call { args, function } => {
                let func = function;
                if let TypedExprKind::Variable(name) = &function.kind {
                    if self
                        .type_converter
                        .module
                        .struct_types
                        .iter()
                        .any(|s| s.name == *name)
                    {
                        // Direct constructor call
                        let struct_ty = convert_type(&expr.ty);
                        let mut current_value = Value::Constant(Constant::Undef);

                        for (index, arg_expr) in args.iter().enumerate() {
                            let arg_val = self
                                .generate_expr(arg_expr)
                                .expect("Constructor argument must produce a value");

                            let dest = self.new_register();
                            self.add_instruction(Instruction::InsertValue {
                                dest: dest.clone(),
                                aggregate: current_value,
                                aggregate_ty: struct_ty.clone(),
                                value: arg_val,
                                value_ty: convert_type(&arg_expr.ty),
                                index,
                                span: arg_expr.range.clone(),
                            });
                            current_value = Value::Register(dest);
                        }
                        return Some(current_value);
                    }
                }

                // 1. Generate IR for the function expression itself.
                // This `func` could resolve to a global function symbol, a register holding a function pointer, etc.
                let func_val = self.generate_expr(func).expect(&format!(
                    "Function expression must resolve to a value for call at {:?}",
                    func.range
                ));

                // 2. Generate IR for each argument.
                let mut arg_vals = Vec::new();
                for arg_expr in args {
                    let arg_val = self.generate_expr(arg_expr).expect(&format!(
                        "Argument expression must resolve to a value for call at {:?}",
                        arg_expr.range
                    ));
                    arg_vals.push(arg_val);
                }

                // 3. Determine the return type of the function call.
                let return_ty = convert_type(&expr.ty); // `expr.ty` is the type of the entire call expression.

                // 4. Decide if a destination register is needed.
                // If the function returns Void, no register is needed to hold the result.
                let dest_reg = if return_ty == IRType::Void {
                    None
                } else {
                    Some(self.new_register())
                };

                // 5. Create and add the Call instruction.
                self.add_instruction(Instruction::Call {
                    dest: dest_reg.clone(), // Clone because `dest_reg` is an `Option<String>`
                    func: func_val,
                    args: arg_vals,
                    ty: return_ty,
                    span, // Use the span of the entire call expression for the instruction
                });

                // 6. Return the result register if applicable.
                dest_reg.map(Value::Register) // Convert `Option<String>` to `Option<Value::Register>`
            }

            TypedExprKind::String(s) => {
                // Create a unique name for the string
                let _global_name =
                    format!("str_{}_{}", self.function.name, self.global_string_count);
                self.global_string_count += 1;

                // Create the string as a constant value
                let string_val = Value::Constant(Constant::String(s.clone()));

                // Store it in a temporary alloca
                let alloca_reg = self.new_register();
                self.add_instruction(Instruction::Alloca {
                    dest: alloca_reg.clone(),
                    ty: IRType::Ptr,
                    span: span.clone(),
                });

                self.add_instruction(Instruction::Store {
                    value: string_val,
                    ptr: Value::Register(alloca_reg.clone()),
                    ty: IRType::Ptr,
                    span,
                });

                // Return the pointer to the string
                Some(Value::Register(alloca_reg))
            }

            TypedExprKind::Return(expr) => {
                let retval = self.generate_expr(expr);
                self.terminate_block(Terminator::Ret(retval, span));
                None
            }

            TypedExprKind::Array { elements } => {
                let element_ty = if let Type::Constructor{
                    name,
                    generics,
                    ..
                } = &*expr.ty{
                    if name == "List" {
                        convert_type(&generics[0])
                    }else {
                        panic!("Array expression doesn't have array type");
                    }
                } else {
                    panic!("Array expression doesn't have array type");
                };

                let mut current_value = Value::Constant(Constant::Undef);

                for (index, element) in elements.iter().enumerate() {
                    let element_val = self.generate_expr(element).unwrap();
                    let dest = self.new_register();

                    self.add_instruction(Instruction::InsertValue {
                        dest: dest.clone(),
                        aggregate: current_value,
                        aggregate_ty: convert_type(&expr.ty),
                        value: element_val,
                        value_ty: element_ty.clone(),
                        index,
                        span: element.range.clone(),
                    });
                    current_value = Value::Register(dest);
                }

                Some(current_value)
            }

            TypedExprKind::Index { array, index } => {
                let array_val = self.generate_expr(array).unwrap();
                let _index_val = self.generate_expr(index).unwrap();
                let dest = self.new_register();

                self.add_instruction(Instruction::ExtractValue {
                    dest: dest.clone(),
                    aggregate: array_val,
                    index: 0, // Placeholder - need actual index calculation
                    span,
                });

                Some(Value::Register(dest))
            }

            TypedExprKind::StructAccess {
                struct_val,
                field_name,
            } => {
                let struct_ty = convert_type(&struct_val.ty);
                let struct_val = self.generate_expr(struct_val).unwrap();

                // Look up field index
                let field_index = if let IRType::Struct(name, _) = &struct_ty {
                    self.type_converter
                        .module
                        .struct_types
                        .iter()
                        .find(|s| &s.name == name)
                        .and_then(|s| s.fields.iter().position(|(n, _)| n == field_name))
                        .expect("Struct field not found")
                } else {
                    panic!("Non-struct type in struct access");
                };

                let dest = self.new_register();
                self.add_instruction(Instruction::ExtractValue {
                    dest: dest.clone(),
                    aggregate: struct_val,
                    index: field_index,
                    span,
                });

                Some(Value::Register(dest))
            }

            TypedExprKind::MethodCall {
                struct_val,
                method_name,
                args,
            } => {
                // Look up method function
                let func_val = Value::Global(method_name.clone());

                // Generate struct value
                let struct_val = self.generate_expr(struct_val).unwrap();

                // Generate arguments
                let mut arg_vals = vec![struct_val];
                for arg in args {
                    arg_vals.push(self.generate_expr(arg).unwrap());
                }

                // Determine return type
                let return_ty = convert_type(&expr.ty);
                let dest_reg = if return_ty == IRType::Void {
                    None
                } else {
                    Some(self.new_register())
                };

                // Add call instruction
                self.add_instruction(Instruction::Call {
                    dest: dest_reg.clone(),
                    func: func_val,
                    args: arg_vals,
                    ty: return_ty,
                    span,
                });

                dest_reg.map(Value::Register)
            }

            TypedExprKind::Assign {
                l_value,
                r_value,
                assign_op: _,
            } => {
                let r_val = self.generate_expr(r_value).unwrap();
                let l_val = self.generate_expr(l_value).unwrap();

                let ty = convert_type(&r_value.ty);

                self.add_instruction(Instruction::Store {
                    value: r_val,
                    ptr: l_val,
                    ty,
                    span,
                });

                None
            }

            TypedExprKind::UnOp { unop, expression } => {
                let operand_val = self.generate_expr(expression).unwrap();
                let dest = self.new_register();
                let ty = convert_type(&expression.ty);

                let op = match unop {
                    crate::ast::UnOp::Minus => crate::ir::UnOp::Neg,
                    crate::ast::UnOp::Not => crate::ir::UnOp::Not,
                    crate::ast::UnOp::Plus => todo!(), // why would you do this
                };

                self.add_instruction(Instruction::UnOp {
                    dest: dest.clone(),
                    op,
                    operand: operand_val,
                    ty,
                    span,
                });

                Some(Value::Register(dest))
            }

            TypedExprKind::Tuple(elements) => {
                let tuple_ty = convert_type(&expr.ty);
                let mut current_value = Value::Constant(Constant::Undef);

                for (index, element) in elements.iter().enumerate() {
                    let element_val = self.generate_expr(element).unwrap();
                    let element_ty = convert_type(&element.ty);

                    let dest = self.new_register();
                    self.add_instruction(Instruction::InsertValue {
                        dest: dest.clone(),
                        aggregate: current_value,
                        aggregate_ty: tuple_ty.clone(),
                        value: element_val,
                        value_ty: element_ty,
                        index,
                        span: element.range.clone(),
                    });
                    current_value = Value::Register(dest);
                }

                Some(current_value)
            }

            // TypedExprKind::EnumVariant { enum_name, variant_name, fields } => {
            //     // Look up enum definition
            //     let enum_def = self.type_converter.module.enum_types
            //         .iter()
            //         .find(|e| e.name == *enum_name)
            //         .expect("Enum type not found");

            //     // Find variant
            //     let variant = enum_def.variants
            //         .iter()
            //         .find(|v| v.name == *variant_name)
            //         .expect("Enum variant not found");

            //     // Create undef value for the enum
            //     let enum_ty = IRType::Enum(enum_name.clone(), enum_def.variants.clone(), span.clone());
            //     let mut enum_val = Value::Constant(Constant::Undef);

            //     // Insert tag
            //     let tag_dest = self.new_register();
            //     self.add_instruction(Instruction::InsertValue {
            //         dest: tag_dest.clone(),
            //         aggregate: enum_val,
            //         aggregate_ty: enum_ty.clone(),
            //         value: Value::Constant(Constant::Int(variant.tag as i64)),
            //         value_ty: IRType::I32,
            //         index: 0,
            //         span,
            //     });
            //     enum_val = Value::Register(tag_dest);

            //     // Handle payload if exists
            //     if let Some(data_ty) = &variant.data_type {
            //         let mut payload_val = Value::Constant(Constant::Undef);

            //         match fields {
            //             // Tuple variant
            //             fields if fields.iter().all(|(name, _)| name.is_none()) => {
            //                 for (index, (_, expr)) in fields.iter().enumerate() {
            //                     let field_val = self.generate_expr(expr).unwrap();
            //                     let field_ty = convert_type(&expr.ty);

            //                     let dest = self.new_register();
            //                     self.add_instruction(Instruction::InsertValue {
            //                         dest: dest.clone(),
            //                         aggregate: payload_val,
            //                         aggregate_ty: data_ty.clone(),
            //                         value: field_val,
            //                         value_ty: field_ty,
            //                         index,
            //                         span: expr.range.clone(),
            //                     });
            //                     payload_val = Value::Register(dest);
            //                 }
            //             }
            //             // Struct variant
            //             _ => {
            //                 let struct_ty = convert_type(&expr.ty);
            //                 for (field_name, expr) in fields {
            //                     let field_name = field_name.as_ref().expect("Field name missing");
            //                     let field_index = if let IRType::Struct(name, _) = data_ty {
            //                         self.type_converter.module.struct_types
            //                             .iter()
            //                             .find(|s| &s.name == name)
            //                             .and_then(|s| s.fields.iter().position(|(n, _)| n == field_name))
            //                             .expect("Struct field not found")
            //                     } else {
            //                         panic!("Non-struct type in struct variant");
            //                     };

            //                     let field_val = self.generate_expr(expr).unwrap();
            //                     let field_ty = convert_type(&expr.ty);

            //                     let dest = self.new_register();
            //                     self.add_instruction(Instruction::InsertValue {
            //                         dest: dest.clone(),
            //                         aggregate: payload_val,
            //                         aggregate_ty: data_ty.clone(),
            //                         value: field_val,
            //                         value_ty: field_ty,
            //                         index: field_index,
            //                         span: expr.range.clone(),
            //                     });
            //                     payload_val = Value::Register(dest);
            //                 }
            //             }
            //         }

            //         // Insert payload into enum
            //         let payload_dest = self.new_register();
            //         self.add_instruction(Instruction::InsertValue {
            //             dest: payload_dest.clone(),
            //             aggregate: enum_val,
            //             aggregate_ty: enum_ty.clone(),
            //             value: payload_val,
            //             value_ty: data_ty.clone(),
            //             index: 1,
            //             span,
            //         });
            //         enum_val = Value::Register(payload_dest);
            //     }

            //     Some(enum_val)
            // }

            // TypedExprKind::Match { expr, arms } => {
            //     let match_val = self.generate_expr(expr).unwrap();
            //     let match_ty = convert_type(&expr.ty);

            //     // Create blocks for each arm
            //     let end_label = self.new_label("match_end");
            //     let mut arm_labels = Vec::new();
            //     for _ in arms {
            //         arm_labels.push(self.new_label("match_arm"));
            //     }

            //     // Generate tag extraction
            //     let tag_reg = self.new_register();
            //     self.add_instruction(Instruction::ExtractValue {
            //         dest: tag_reg.clone(),
            //         aggregate: match_val,
            //         index: 0, // Tag is first field
            //         span,
            //     });

            //     // Create switch based on tag
            //     let cases: Vec<(Constant, String)> = arms.iter()
            //         .zip(&arm_labels)
            //         .map(|(arm, label)| {
            //             if let TypedPattern::EnumVariant { variant_name, .. } = &arm.pattern {
            //                 let variant = self.type_converter.module.enum_types
            //                     .iter()
            //                     .find(|e| e.name == match_ty.get_name().unwrap())
            //                     .and_then(|e| e.variants.iter().find(|v| v.name == *variant_name))
            //                     .expect("Variant not found");
            //                 (Constant::Int(variant.tag as i64), label.clone())
            //             } else {
            //                 panic!("Non-enum pattern in match");
            //             }
            //         })
            //         .collect();

            //     self.terminate_block(Terminator::Switch {
            //         value: Value::Register(tag_reg),
            //         cases,
            //         default: end_label.clone(),
            //         span,
            //     });

            //     // Generate each arm
            //     let mut arm_results = Vec::new();
            //     for (arm, arm_label) in arms.iter().zip(arm_labels) {
            //         self.set_current_block(arm_label.clone());

            //         // TODO: Implement pattern bindings
            //         let arm_val = self.generate_expr(&arm.body).unwrap();
            //         self.terminate_block(Terminator::BrUncond(end_label.clone(), arm.body.range.clone()));
            //         arm_results.push((arm_val, arm_label));
            //     }

            //     // End block
            //     self.set_current_block(end_label);
            //     let result_reg = self.new_register();

            //     let phi_options = arm_results.into_iter()
            //         .map(|(val, label)| (val, label))
            //         .collect();

            //     self.add_instruction(Instruction::Phi {
            //         dest: result_reg.clone(),
            //         options: phi_options,
            //         span,
            //     });

            //     Some(Value::Register(result_reg))
            // }

            // TypedExprKind::Lambda { args, expression } => {
            //     // Create a new function for the lambda
            //     let lambda_name = format!("lambda_{}", self.lambda_count);
            //     self.lambda_count += 1;

            //     let mut func_gen = FunctionGenerator::new(self.type_converter, lambda_name.clone());
            //     func_gen.function.return_type = convert_type(&expression.ty);

            //     // Add parameters
            //     for (name, ty, _) in args {
            //         func_gen.function.params.push((name.clone(), convert_type(ty)));
            //         func_gen.symbol_table.insert(
            //             name.clone(),
            //             Value::Register(format!("%{}", name))
            //         );
            //     }

            //     // Generate body
            //     let entry_label = func_gen.new_label("entry");
            //     func_gen.set_current_block(entry_label);
            //     let body_val = func_gen.generate_expr(expression);

            //     if func_gen.current_block.is_some() {
            //         func_gen.terminate_block(Terminator::Ret(body_val, expression.range.clone()));
            //     }

            //     // Add function to module
            //     self.type_converter.module.functions.push(func_gen.finalize());

            //     // Return function pointer
            //     Some(Value::Global(lambda_name))
            // }
            TypedExprKind::Error => None,
            _ => todo!(),
        }
    }
}

/// Converts an AST type into an IR type.
fn convert_type(ty: &Arc<Type>) -> IRType {
    match &**ty {
        Type::Constructor { name, .. } => match name.as_str() {
            "bool" => IRType::I1,
            "byte" | "i8" | "char" => IRType::I8,
            "int" | "i32" => IRType::I32,
            "long" | "i64" => IRType::I64,
            "float" | "f32" => IRType::F32,
            "double" | "f64" => IRType::F64,
            "ptr" | "string" => IRType::Ptr,
            _ => IRType::Struct(name.clone(), 0..0), // User-defined struct
        },
        Type::Function {
            params,
            return_type,
        } => {
            let param_types = params.iter().map(convert_type).collect();
            let ret_type = Box::new(convert_type(return_type));
            IRType::Function(param_types, ret_type)
        }
        Type::Tuple(_types) => {
            IRType::Struct("tuple".into(), 0..0)
        }
        Type::Unit => IRType::Void,
        Type::Variable(_) => panic!("Cannot generate IR for unresolved generic type variable. If this happens. Please report it as a bug."),
        _ => unimplemented!("Unsupported type for IR generation: {:?}", ty),
    }
}
