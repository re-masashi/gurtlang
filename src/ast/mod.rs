use std::ops::Range;
use std::vec::Vec;

#[derive(Debug)]
pub enum ASTNode {
    Expr((Expr, Range<usize>)),
    Function(Function),
    Struct(Struct),
    Error, // dummy node for error recovery
}

#[derive(Debug)]
pub enum Expr {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),

    Variable(String),

    Array {
        elements: Vec<(Expr, Range<usize>)>,
    },

    Index {
        array: Box<(Expr, Range<usize>)>,
        index: Box<(Expr, Range<usize>)>,
    },

    Call {
        function: Box<(Expr, Range<usize>)>,
        args: Vec<(Expr, Range<usize>)>,
    },

    StructAccess {
        struct_val: Box<(Expr, Range<usize>)>,
        field_name: String,
    },

    MethodCall {
        struct_val: Box<(Expr, Range<usize>)>,
        method_name: String,
        args: Vec<(Expr, Range<usize>)>,
    },

    BinOp {
        operator: BinOp,
        l_value: Box<(Expr, Range<usize>)>,
        r_value: Box<(Expr, Range<usize>)>,
    },

    Assign {
        l_value: Box<(Expr, Range<usize>)>,
        r_value: Box<(Expr, Range<usize>)>,
        assign_op: AssignOp,
    },

    UnOp {
        unop: UnOp,
        expression: Box<(Expr, Range<usize>)>,
    },

    Do {
        expressions: Vec<(Expr, Range<usize>)>,
    },

    IfElse {
        condition: Box<(Expr, Range<usize>)>,
        if_branch: Box<(Expr, Range<usize>)>,
        else_branch: Option<Box<(Expr, Range<usize>)>>,
    },

    Tuple(Vec<(Expr, Range<usize>)>),

    Error, // dummy node for error recovery
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Power,

    Eq,
    NotEq,
    Less,
    Greater,
    LessEq,
    GreaterEq,

    And,
    Or,
    Xor,
    Nor,
}

#[derive(Debug)]
pub enum UnOp {
    Plus,
    Minus,
    Not,
}

#[derive(Debug)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub args: Vec<(String, Option<TypeAnnot>, Range<usize>)>,
    pub body: Box<(Expr, Range<usize>)>,
    pub return_type: Option<(TypeAnnot, Range<usize>)>,
}

#[derive(Debug)]
pub struct Struct {
    pub name: String,
    pub generics: Vec<(String, Range<usize>)>, // only boring types?
    pub fields: Vec<(String, TypeAnnot, Range<usize>)>,
}

#[derive(Debug)]
pub enum TypeAnnot {
    Bool,
    Int,
    Float,
    String,
    Boring(String),
    Generic(String, Vec<TypeAnnot>),
    Union(Vec<TypeAnnot>),
    Function {
        params: Vec<TypeAnnot>,
        return_type: Box<TypeAnnot>,
    },
    Tuple(Vec<TypeAnnot>),
    Trait(String),
}
