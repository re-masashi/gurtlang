use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use std::ops::Range;

pub mod builder;

pub use builder::IRBuilder;

#[derive(Debug, Clone, PartialEq)]
pub enum IRType {
    Void,
    I1,  // bool
    I8,  // byte
    I32, // int
    I64, // long
    F64, // float
    Ptr, // generic pointer (GC-managed)
    Struct {
        name: String,
        fields: Vec<IRType>,
    },
    TaggedUnion {
        name: String,
        tag_type: Box<IRType>,
        max_variant_size: usize,
        variants: Vec<TaggedVariant>,
    },
    Array {
        element_type: Box<IRType>,
        size: usize,
    },
    Function {
        params: Vec<IRType>,
        return_type: Box<IRType>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TaggedVariant {
    pub name: String,
    pub tag_value: u64,
    pub data_type: Option<IRType>,
}

#[derive(Debug, Clone)]
pub enum Value {
    Constant(Constant),
    Register(String),
    Global(String),
    Argument(String),
}

#[derive(Debug, Clone)]
pub enum Constant {
    Void,
    Bool(bool),
    Int(i64),
    I32(i32),
    Float(f64),
    String(String),
    Null,
    Undef,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub label: String,
    pub instructions: Vec<Instruction>,
    pub terminator: Option<Terminator>,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    // Memory operations
    Alloca {
        dest: String,
        ty: IRType,
        span: Range<usize>,
    },
    Load {
        dest: String,
        ptr: Value,
        ty: IRType,
        span: Range<usize>,
    },
    Store {
        value: Value,
        ptr: Value,
        ty: IRType,
        span: Range<usize>,
    },

    // Arithmetic operations
    Add {
        dest: String,
        lhs: Value,
        rhs: Value,
        ty: IRType,
        span: Range<usize>,
    },
    Sub {
        dest: String,
        lhs: Value,
        rhs: Value,
        ty: IRType,
        span: Range<usize>,
    },
    Mul {
        dest: String,
        lhs: Value,
        rhs: Value,
        ty: IRType,
        span: Range<usize>,
    },
    Div {
        dest: String,
        lhs: Value,
        rhs: Value,
        ty: IRType,
        span: Range<usize>,
    },

    // Comparison operations
    ICmp {
        dest: String,
        cond: ICmpCond,
        lhs: Value,
        rhs: Value,
        span: Range<usize>,
    },
    FCmp {
        dest: String,
        cond: FCmpCond,
        lhs: Value,
        rhs: Value,
        span: Range<usize>,
    },

    // Function calls
    Call {
        dest: Option<String>,
        func: Value,
        args: Vec<Value>,
        ty: IRType,
        span: Range<usize>,
    },

    // Struct/Array operations
    GetElementPtr {
        dest: String,
        ptr: Value,
        indices: Vec<Value>,
        ty: IRType,
        span: Range<usize>,
    },
    ExtractValue {
        dest: String,
        aggregate: Value,
        indices: Vec<usize>,
        span: Range<usize>,
    },
    InsertValue {
        dest: String,
        aggregate: Value,
        value: Value,
        indices: Vec<usize>,
        span: Range<usize>,
    },

    // Phi node for SSA
    Phi {
        dest: String,
        ty: IRType,
        incoming: Vec<(Value, String)>,
        span: Range<usize>,
    },

    // Type conversions
    BitCast {
        dest: String,
        value: Value,
        from_ty: IRType,
        to_ty: IRType,
        span: Range<usize>,
    },
    Trunc {
        dest: String,
        value: Value,
        from_ty: IRType,
        to_ty: IRType,
        span: Range<usize>,
    },
    ZExt {
        dest: String,
        value: Value,
        from_ty: IRType,
        to_ty: IRType,
        span: Range<usize>,
    },
    SExt {
        dest: String,
        value: Value,
        from_ty: IRType,
        to_ty: IRType,
        span: Range<usize>,
    },

    // GC allocation (simplified - just allocate)
    GCAlloc {
        dest: String,
        object_type: HeapObjectType,
        size: Option<Value>,
        span: Range<usize>,
    },

    // String operations
    StringInit {
        dest: String,
        content: String,
        span: Range<usize>,
    },
    StringConcat {
        dest: String,
        left: Value,
        right: Value,
        span: Range<usize>,
    },
    StringLength {
        dest: String,
        string: Value,
        span: Range<usize>,
    },

    // Array operations
    ArrayConcat {
        dest: String,
        left: Value,
        right: Value,
        element_type: HeapObjectType,
        span: Range<usize>,
    },
    ArrayLength {
        dest: String,
        array: Value,
        span: Range<usize>,
    },
    ArrayNew {
        dest: String,
        element_type: HeapObjectType,
        length: Value,
        span: Range<usize>,
    },
    ArrayGet {
        dest: String,
        array: Value,
        index: Value,
        span: Range<usize>,
    },
    ArraySet {
        array: Value,
        index: Value,
        value: Value,
        span: Range<usize>,
    },

    // Struct operations
    StructNew {
        dest: String,
        struct_type: String,
        span: Range<usize>,
    },
    StructGet {
        dest: String,
        object: Value,
        field: String,
        span: Range<usize>,
    },
    StructSet {
        object: Value,
        field: String,
        value: Value,
        span: Range<usize>,
    },

    // Enum operations
    EnumCreate {
        dest: String,
        enum_type: String,
        variant_name: String,
        variant_data: Option<Value>,
        span: Range<usize>,
    },
    EnumGetTag {
        dest: String,
        enum_value: Value,
        span: Range<usize>,
    },
    EnumExtractData {
        dest: String,
        enum_value: Value,
        variant_name: String,
        expected_type: IRType,
        span: Range<usize>,
    },
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Ret {
        value: Option<Value>,
        span: Range<usize>,
    },
    Br {
        label: String,
        span: Range<usize>,
    },
    CondBr {
        cond: Value,
        then_label: String,
        else_label: String,
        span: Range<usize>,
    },
    Unreachable {
        span: Range<usize>,
    },
}

#[derive(Debug, Clone)]
pub enum ICmpCond {
    Eq,
    Ne,
    Slt,
    Sle,
    Sgt,
    Sge,
    Ult,
    Ule,
    Ugt,
    Uge,
}

#[derive(Debug, Clone)]
pub enum FCmpCond {
    False,
    True,
    Oeq,
    One,
    Ogt,
    Oge,
    Olt,
    Ole,
    Ord,
    Ueq,
    Une,
    Ugt,
    Uge,
    Ult,
    Ule,
    Uno,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<(String, IRType)>,
    pub return_type: IRType,
    pub blocks: Vec<BasicBlock>,
    pub is_external: bool,
}

#[derive(Debug, Clone)]
pub struct StructType {
    pub name: String,
    pub fields: Vec<(String, IRType)>,
}

#[derive(Debug, Default)]
pub struct Module {
    pub name: String,
    pub functions: Vec<Function>,
    pub struct_types: Vec<StructType>,
    pub global_strings: HashMap<String, String>,
}

#[derive(Debug, Clone)]
pub enum HeapObjectType {
    Array {
        element_type: Box<HeapObjectType>,
        len: usize,
        capacity: usize,
    },
    Struct {
        name: String,
        fields: Vec<(String, HeapObjectType)>,
    },
    String {
        len: usize,
    },
}

// Display implementations

impl Display for HeapObjectType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            HeapObjectType::Array {
                element_type,
                len,
                capacity,
            } => {
                write!(f, "array<{}>[len={}, cap={}]", element_type, len, capacity)
            }
            HeapObjectType::Struct { name, fields } => {
                write!(f, "{}", name)?;
                if !fields.is_empty() {
                    write!(f, " {{ ")?;
                    for (i, (field_name, field_type)) in fields.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}: {}", field_name, field_type)?;
                    }
                    write!(f, " }}")?;
                }
                Ok(())
            }
            HeapObjectType::String { len } => {
                write!(f, "string[{}]", len)
            }
        }
    }
}

impl Display for IRType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            IRType::Void => write!(f, "void"),
            IRType::I1 => write!(f, "i1"),
            IRType::I8 => write!(f, "i8"),
            IRType::I32 => write!(f, "i32"),
            IRType::I64 => write!(f, "i64"),
            IRType::F64 => write!(f, "double"),
            IRType::Ptr => write!(f, "ptr"),
            IRType::Struct { name, fields } => {
                write!(f, "%{} = type {{ ", name)?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", field)?;
                }
                write!(f, " }}")
            }
            IRType::Array { element_type, size } => {
                write!(f, "[{} x {}]", size, element_type)
            }
            IRType::Function {
                params,
                return_type,
            } => {
                write!(f, "{} (", return_type)?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ")")
            }
            IRType::TaggedUnion { .. } => write!(f, "tagged_union"),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Constant(c) => write!(f, "{}", c),
            Value::Register(name) => write!(f, "{}", name),
            Value::Global(name) => write!(f, "@{}", name),
            Value::Argument(name) => write!(f, "%{}", name),
        }
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Void => write!(f, "void"),
            Constant::Bool(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Constant::Int(i) => write!(f, "{}", i),
            Constant::Float(fl) => write!(f, "{:.6}", fl),
            Constant::String(s) => write!(f, "\"{}\"", s.escape_default()),
            Constant::Null => write!(f, "null"),
            Constant::Undef => write!(f, "undef"),
            Constant::I32(i) => write!(f, "i32_{i}"),
        }
    }
}

impl Display for ICmpCond {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            ICmpCond::Eq => "eq",
            ICmpCond::Ne => "ne",
            ICmpCond::Slt => "slt",
            ICmpCond::Sle => "sle",
            ICmpCond::Sgt => "sgt",
            ICmpCond::Sge => "sge",
            ICmpCond::Ult => "ult",
            ICmpCond::Ule => "ule",
            ICmpCond::Ugt => "ugt",
            ICmpCond::Uge => "uge",
        };
        write!(f, "{}", s)
    }
}

impl Display for FCmpCond {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            FCmpCond::False => "false",
            FCmpCond::True => "true",
            FCmpCond::Oeq => "oeq",
            FCmpCond::One => "one",
            FCmpCond::Ogt => "ogt",
            FCmpCond::Oge => "oge",
            FCmpCond::Olt => "olt",
            FCmpCond::Ole => "ole",
            FCmpCond::Ord => "ord",
            FCmpCond::Ueq => "ueq",
            FCmpCond::Une => "une",
            FCmpCond::Ugt => "ugt",
            FCmpCond::Uge => "uge",
            FCmpCond::Ult => "ult",
            FCmpCond::Ule => "ule",
            FCmpCond::Uno => "uno",
        };
        write!(f, "{}", s)
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Alloca { dest, ty, .. } => {
                write!(f, "  {} = alloca {}", dest, ty)
            }
            Instruction::Load { dest, ptr, ty, .. } => {
                write!(f, "  {} = load {}, ptr {}", dest, ty, ptr)
            }
            Instruction::Store { value, ptr, ty, .. } => {
                write!(f, "  store {} {}, ptr {}", ty, value, ptr)
            }
            Instruction::Add {
                dest, lhs, rhs, ty, ..
            } => {
                write!(f, "  {} = add {} {}, {}", dest, ty, lhs, rhs)
            }
            Instruction::Sub {
                dest, lhs, rhs, ty, ..
            } => {
                write!(f, "  {} = sub {} {}, {}", dest, ty, lhs, rhs)
            }
            Instruction::Mul {
                dest, lhs, rhs, ty, ..
            } => {
                write!(f, "  {} = mul {} {}, {}", dest, ty, lhs, rhs)
            }
            Instruction::Div {
                dest, lhs, rhs, ty, ..
            } => {
                write!(f, "  {} = div {} {}, {}", dest, ty, lhs, rhs)
            }
            Instruction::ICmp {
                dest,
                cond,
                lhs,
                rhs,
                ..
            } => {
                write!(f, "  {} = icmp {} {}, {}", dest, cond, lhs, rhs)
            }
            Instruction::FCmp {
                dest,
                cond,
                lhs,
                rhs,
                ..
            } => {
                write!(f, "  {} = fcmp {} {}, {}", dest, cond, lhs, rhs)
            }
            Instruction::Call {
                dest,
                func,
                args,
                ty,
                ..
            } => {
                write!(f, "  ")?;
                if let Some(d) = dest {
                    write!(f, "{} = ", d)?;
                }
                write!(f, "call {} {}(", ty, func)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Instruction::GetElementPtr {
                dest,
                ptr,
                indices,
                ty,
                ..
            } => {
                write!(f, "  {} = getelementptr {}, ptr {}", dest, ty, ptr)?;
                for idx in indices {
                    write!(f, ", {}", idx)?;
                }
                Ok(())
            }
            Instruction::ExtractValue {
                dest,
                aggregate,
                indices,
                ..
            } => {
                write!(f, "  {} = extractvalue {}", dest, aggregate)?;
                for idx in indices {
                    write!(f, ", {}", idx)?;
                }
                Ok(())
            }
            Instruction::InsertValue {
                dest,
                aggregate,
                value,
                indices,
                ..
            } => {
                write!(f, "  {} = insertvalue {}, {}", dest, aggregate, value)?;
                for idx in indices {
                    write!(f, ", {}", idx)?;
                }
                Ok(())
            }
            Instruction::Phi {
                dest, ty, incoming, ..
            } => {
                write!(f, "  {} = phi {}", dest, ty)?;
                for (i, (val, label)) in incoming.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, " [ {}, %{} ]", val, label)?;
                }
                Ok(())
            }
            Instruction::BitCast {
                dest,
                value,
                from_ty,
                to_ty,
                ..
            } => {
                write!(f, "  {} = bitcast {} {} to {}", dest, from_ty, value, to_ty)
            }
            Instruction::Trunc {
                dest,
                value,
                from_ty,
                to_ty,
                ..
            } => {
                write!(f, "  {} = trunc {} {} to {}", dest, from_ty, value, to_ty)
            }
            Instruction::ZExt {
                dest,
                value,
                from_ty,
                to_ty,
                ..
            } => {
                write!(f, "  {} = zext {} {} to {}", dest, from_ty, value, to_ty)
            }
            Instruction::SExt {
                dest,
                value,
                from_ty,
                to_ty,
                ..
            } => {
                write!(f, "  {} = sext {} {} to {}", dest, from_ty, value, to_ty)
            }
            Instruction::GCAlloc {
                dest,
                object_type,
                size,
                ..
            } => {
                write!(f, "  {} = gc_alloc {}", dest, object_type)?;
                if let Some(sz) = size {
                    write!(f, ", size {}", sz)?;
                }
                Ok(())
            }
            Instruction::ArrayNew {
                dest,
                element_type,
                length,
                ..
            } => {
                write!(
                    f,
                    "  {} = array_new {}, length {}",
                    dest, element_type, length
                )
            }
            Instruction::ArrayGet {
                dest, array, index, ..
            } => {
                write!(f, "  {} = array_get {}, {}", dest, array, index)
            }
            Instruction::ArraySet {
                array,
                index,
                value,
                ..
            } => {
                write!(f, "  array_set {}, {}, {}", array, index, value)
            }
            Instruction::StructNew {
                dest, struct_type, ..
            } => {
                write!(f, "  {} = struct_new {}", dest, struct_type)
            }
            Instruction::StructGet {
                dest,
                object,
                field,
                ..
            } => {
                write!(f, "  {} = struct_get {}, {}", dest, object, field)
            }
            Instruction::StructSet {
                object,
                field,
                value,
                ..
            } => {
                write!(f, "  struct_set {}, {}, {}", object, field, value)
            }
            Instruction::EnumCreate {
                dest,
                enum_type,
                variant_name,
                variant_data,
                ..
            } => {
                write!(
                    f,
                    "  {} = enum_create {} ::{}",
                    dest, enum_type, variant_name
                )?;
                if let Some(data) = variant_data {
                    write!(f, "({})", data)?;
                }
                Ok(())
            }
            Instruction::EnumGetTag {
                dest, enum_value, ..
            } => {
                write!(f, "  {} = enum_get_tag {}", dest, enum_value)
            }
            Instruction::EnumExtractData {
                dest,
                enum_value,
                variant_name,
                expected_type,
                ..
            } => {
                write!(
                    f,
                    "  {} = enum_extract {} as {} -> {}",
                    dest, enum_value, variant_name, expected_type
                )
            }
            Instruction::StringConcat {
                dest, left, right, ..
            } => {
                write!(f, "  {} = string_concat {}, {}", dest, left, right)
            }
            Instruction::StringLength { dest, string, .. } => {
                write!(f, "  {} = string_length {}", dest, string)
            }
            Instruction::ArrayConcat {
                dest,
                left,
                right,
                element_type,
                ..
            } => {
                write!(
                    f,
                    "  {} = array_concat {}, {} -> array<{}>",
                    dest, left, right, element_type
                )
            }
            Instruction::ArrayLength { dest, array, .. } => {
                write!(f, "  {} = array_length {}", dest, array)
            }
            Instruction::StringInit { dest, content, .. } => {
                write!(f, "  {} = string_init \"{}\"", dest, content.escape_debug())
            }
        }
    }
}

impl Display for Terminator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Terminator::Ret {
                value: Some(val), ..
            } => {
                write!(f, "  ret {}", val)
            }
            Terminator::Ret { value: None, .. } => {
                write!(f, "  ret void")
            }
            Terminator::Br { label, .. } => {
                write!(f, "  br label %{}", label)
            }
            Terminator::CondBr {
                cond,
                then_label,
                else_label,
                ..
            } => {
                write!(
                    f,
                    "  br i1 {}, label %{}, label %{}",
                    cond, then_label, else_label
                )
            }
            Terminator::Unreachable { .. } => {
                write!(f, "  unreachable")
            }
        }
    }
}

impl Display for BasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}:", self.label)?;
        for instruction in &self.instructions {
            writeln!(f, "{}", instruction)?;
        }
        if let Some(term) = &self.terminator {
            writeln!(f, "{}", term)?;
        }
        Ok(())
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.is_external {
            write!(f, "declare {} @{}(", self.return_type, self.name)?;
        } else {
            write!(f, "define {} @{}(", self.return_type, self.name)?;
        }

        for (i, (param_name, param_type)) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{} %{}", param_type, param_name)?;
        }

        if self.is_external {
            writeln!(f, ")")?;
        } else {
            writeln!(f, ") {{")?;
            for block in &self.blocks {
                write!(f, "{}", block)?;
            }
            writeln!(f, "}}")?;
        }
        Ok(())
    }
}

impl Display for StructType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "%{} = type {{ ", self.name)?;
        for (i, (_, field_type)) in self.fields.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", field_type)?;
        }
        write!(f, " }}")
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "; Module: {}", self.name)?;
        writeln!(f)?;

        // Print global strings
        if !self.global_strings.is_empty() {
            writeln!(f, "; Global strings")?;
            for (name, value) in &self.global_strings {
                writeln!(
                    f,
                    "@{} = private unnamed_addr constant [{}] c\"{}\\00\"",
                    name,
                    value.len() + 1,
                    value.escape_default()
                )?;
            }
            writeln!(f)?;
        }

        // Print struct types
        if !self.struct_types.is_empty() {
            writeln!(f, "; Struct types")?;
            for struct_type in &self.struct_types {
                writeln!(f, "{}", struct_type)?;
            }
            writeln!(f)?;
        }

        // Print functions
        for function in &self.functions {
            writeln!(f, "{}", function)?;
        }

        Ok(())
    }
}
