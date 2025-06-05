use std::ops::Range;
use std::sync::Arc;
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

    Let {
        var: String,
        type_annot: Option<(TypeAnnot, Range<usize>)>,
        value: Box<(Expr, Range<usize>)>,
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

#[derive(Debug, Clone)]
pub enum UnOp {
    Plus,
    Minus,
    Not,
}

#[derive(Debug, Clone)]
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
    pub name: (String, Range<usize>),
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Constructor {
        name: String,
        generics: Vec<Arc<Type>>,
        traits: Vec<String>,
    },
    Variable(usize),
    Trait(String),
    Function {
        params: Vec<Arc<Type>>,
        return_type: Arc<Type>,
    },
    Tuple(Vec<Arc<Type>>),
    Union(Vec<Arc<Type>>),
    Unit,
}

#[derive(Debug)]
#[derive(Clone)]
pub struct TypedExpr {
    pub kind: TypedExprKind,
    pub ty: Arc<Type>,
    pub range: Range<usize>,
}

#[derive(Debug)]
#[derive(Clone)]
pub enum TypedExprKind {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),

    Variable(String),

    Array {
        elements: Vec<TypedExpr>,
    },

    Index {
        array: Box<TypedExpr>,
        index: Box<TypedExpr>,
    },

    Call {
        function: Box<TypedExpr>,
        args: Vec<TypedExpr>,
    },

    StructAccess {
        struct_val: Box<TypedExpr>,
        field_name: String,
    },

    MethodCall {
        struct_val: Box<TypedExpr>,
        method_name: String,
        args: Vec<TypedExpr>,
    },

    BinOp {
        operator: BinOp,
        l_value: Box<TypedExpr>,
        r_value: Box<TypedExpr>,
    },
    Assign {
        l_value: Box<TypedExpr>,
        r_value: Box<TypedExpr>,
        assign_op: AssignOp,
    },

    UnOp {
        unop: UnOp,
        expression: Box<TypedExpr>,
    },

    Do {
        expressions: Vec<TypedExpr>,
    },

    Let {
        var: String,
        value: Box<TypedExpr>,
    },

    IfElse {
        condition: Box<TypedExpr>,
        if_branch: Box<TypedExpr>,
        else_branch: Option<Box<TypedExpr>>,
    },

    Tuple(Vec<TypedExpr>),

    Error, // dummy node for error recovery
}

#[derive(Debug)]
pub enum TypedASTNode {
    Expr((TypedExpr, Range<usize>)),
    Function((TypedFunction, Range<usize>)),
    Struct((TypedStruct, Range<usize>)),
    Error, // dummy node for error recovery
}

#[derive(Debug)]
#[derive(Clone)]
pub struct TypedFunction {
    pub name: String,
    pub args: Vec<(String, Arc<Type>, Range<usize>)>,
    pub body: Box<(TypedExpr, Range<usize>)>,
    pub return_type: (Arc<Type>, Range<usize>),
}

#[derive(Debug)]
pub struct TypedStruct {
    pub name: String,
    pub generics: Vec<(String, Range<usize>)>, // only boring types?
    pub fields: Vec<(String, Arc<Type>, Range<usize>)>,
}

impl TypedFunction {
    /// Checks if a function is generic (contains type variables)
    pub fn is_generic(&self) -> bool {
        self.args.iter().any(|(_, ty, _)| matches!(&**ty, Type::Variable(_))) ||
        matches!(&*self.return_type.0, Type::Variable(_))
    }
}