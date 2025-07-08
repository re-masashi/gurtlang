use std::ops::Range;
use std::sync::Arc;
use std::vec::Vec;

#[derive(Debug)]
pub enum ASTNode {
    Expr((Expr, Range<usize>)),
    Function(Function),
    Extern(Extern),
    Struct(Struct),
    Enum(Enum, Range<usize>),
    TypeAlias(TypeAlias, Range<usize>),
    Impl(Impl),
    Error, // dummy node for error recovery
}

type EnumFields = Vec<(Option<String>, (Expr, Range<usize>))>;

#[derive(Debug, Clone)]
pub enum Expr {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),

    Variable(String),
    Return(Box<Expr>),
    Lambda {
        args: Vec<(String, Option<TypeAnnot>, Range<usize>)>,
        expression: Box<(Expr, Range<usize>)>,
    },

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

    EnumVariant {
        enum_name: String,
        variant_name: String,
        fields: EnumFields, // Vec<(Option<String>, (Expr, Range<usize>))>,
        range: Range<usize>,
    },
    Match {
        expr: Box<(Expr, Range<usize>)>,
        arms: Vec<MatchArm>,
        range: Range<usize>,
    },

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
pub struct Extern {
    pub name: String,
    pub args: Vec<(TypeAnnot, Range<usize>)>,
    pub return_type: (TypeAnnot, Range<usize>),
}

#[derive(Debug)]
pub struct Struct {
    pub name: (String, Range<usize>),
    pub generics: Vec<(String, Range<usize>)>, // only boring types?
    pub fields: Vec<(String, TypeAnnot, Range<usize>)>,
}

#[derive(Debug)]
pub struct Impl {
    pub struct_: (String, Range<usize>),
    pub trait_: Option<(Trait, Range<usize>)>,
}

#[derive(Debug)]
pub struct Trait {
    pub name: (String, Range<usize>),
    pub generics: Vec<(String, Range<usize>)>,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Box<(Expr, Range<usize>)>,
    pub range: Range<usize>,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Variable(String),
    EnumVariant {
        enum_name: Option<String>,
        variant_name: String,
        subpatterns: Vec<(Pattern, Range<usize>)>,
    },
    Union(Vec<(Pattern, Range<usize>)>),
    Tuple(Vec<(Pattern, Range<usize>)>),
    Guard(Box<(Pattern, Range<usize>)>, (Expr, Range<usize>)),
    Literal(Expr), // For bool/int/float
    Wildcard,      // _
    Error,         // placeholder
}

#[derive(Debug)]
pub struct Enum {
    pub name: (String, Range<usize>),
    pub generics: Vec<(String, Range<usize>)>,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug)]
pub struct EnumVariant {
    pub name: (String, Range<usize>),
    pub kind: EnumVariantKind,
    pub range: Range<usize>,
}

#[derive(Debug)]
pub enum EnumVariantKind {
    Unit,
    Tuple(Vec<(TypeAnnot, Range<usize>)>),
    Struct(Vec<(String, TypeAnnot, Range<usize>)>),
}

#[derive(Debug)]
pub struct TypeAlias {
    pub name: (String, Range<usize>),
    pub generics: Vec<(String, Range<usize>)>,
    pub aliased_type: Box<TypeAnnot>,
    pub range: Range<usize>,
}

#[derive(Debug, Clone)]
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
    Trait(Vec<String>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Type {
    Constructor {
        name: String,
        generics: Vec<Arc<Type>>,
        traits: Vec<String>,
    },
    Variable(usize),
    Trait(Vec<String>), // trait A + B
    Function {
        params: Vec<Arc<Type>>,
        return_type: Arc<Type>,
    },
    Tuple(Vec<Arc<Type>>),
    Union(Vec<Arc<Type>>),

    Never,
    Unit,
}

#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub kind: TypedExprKind,
    pub ty: Arc<Type>,
    pub range: Range<usize>,
}

#[derive(Debug, Clone)]
pub enum TypedExprKind {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),

    Variable(String),

    Return(Box<TypedExpr>),

    Lambda {
        args: Vec<(String, Arc<Type>, Range<usize>)>,
        expression: Box<TypedExpr>,
    },

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

    EnumVariant {
        enum_name: String,
        variant_name: String,
        fields: Vec<(Option<String>, TypedExpr)>,
    },

    Match {
        expr: Box<TypedExpr>,
        arms: Vec<TypedMatchArm>,
    },

    Error, // dummy node for error recovery
}

#[derive(Debug, Clone)]
pub struct TypedMatchArm {
    pub pattern: TypedPattern,
    pub body: Box<TypedExpr>,
}

#[derive(Debug, Clone)]
pub enum TypedPattern {
    Variable(String, Arc<Type>),
    EnumVariant {
        enum_name: String,
        variant_name: String,
        subpatterns: Vec<(TypedPattern, Range<usize>)>,
    },
    Union(Vec<(TypedPattern, Range<usize>)>),
    Tuple(Vec<(TypedPattern, Range<usize>)>),
    Guard(Box<(TypedPattern, Range<usize>)>, (TypedExpr, Range<usize>)),
    Literal(TypedExpr),
    Wildcard,
    Error,
    // ill add other patterns
}

#[derive(Debug)]
pub struct TypedTypeAlias {
    pub name: String,
    pub generics: Vec<(String, Range<usize>)>,
    pub aliased_type: Arc<Type>,
    pub range: Range<usize>,
}

#[derive(Debug)]
pub enum TypedASTNode {
    Expr((TypedExpr, Range<usize>)),
    Function((TypedFunction, Range<usize>)),
    Extern((TypedExtern, Range<usize>)),
    Struct((TypedStruct, Range<usize>)),
    Enum((TypedEnum, Range<usize>)),
    // type aliases are omitted here
    Error, // dummy node for error recovery
}

#[derive(Debug, Clone)]
pub struct TypedFunction {
    pub name: String,
    pub args: Vec<(String, Arc<Type>, Range<usize>)>,
    pub body: Box<(TypedExpr, Range<usize>)>,
    pub return_type: (Arc<Type>, Range<usize>),
    pub is_constructor: bool,
}

#[derive(Debug, Clone)]
pub struct TypedExtern {
    pub name: String,
    pub args: Vec<(Arc<Type>, Range<usize>)>,
    pub return_type: (Arc<Type>, Range<usize>),
}

#[derive(Debug)]
pub struct TypedStruct {
    pub name: String,
    pub generics: Vec<(String, Range<usize>)>, // only boring types?
    pub fields: Vec<(String, Arc<Type>, Range<usize>)>,
}

#[derive(Debug)]
pub struct TypedEnum {
    pub name: String,
    pub generics: Vec<(String, Range<usize>)>,
    pub variants: Vec<TypedEnumVariant>,
}

#[derive(Debug)]
pub struct TypedEnumVariant {
    pub name: String,
    pub kind: TypedEnumVariantKind,
    pub range: Range<usize>,
}

#[derive(Debug)]
pub enum TypedEnumVariantKind {
    Unit,
    Tuple(Vec<Arc<Type>>),
    Struct(Vec<(String, Arc<Type>)>),
}

impl TypedFunction {
    /// Checks if a function is generic (contains type variables)
    pub fn is_generic(&self) -> bool {
        self.args
            .iter()
            .any(|(_, ty, _)| matches!(&**ty, Type::Variable(_)))
            || matches!(&*self.return_type.0, Type::Variable(_))
    }
}
