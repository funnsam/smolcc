#![allow(unused)]

use bitflags::bitflags;

pub type Span = core::ops::Range<usize>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node<T> {
    pub node: T,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IntConst {
    pub value: u64,
    pub suffix: IntSuffix,
    pub infer_unsigned: bool,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct IntSuffix(u8);

bitflags! {
    impl IntSuffix: u8 {
        const U  = 1 << 0;
        const L  = 1 << 1;
        const LL = 1 << 2;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CharConst {
    pub ch: u32,
    pub wide: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringLit {
    pub string: Vec<u32>,
    pub wide: bool,
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Ident(&'a str),
    IntConst(IntConst),
    CharConst(CharConst),
    StringLit(StringLit),

    Comma(Vec<Node<Self>>),
    Assign(Box<Node<Self>>, Box<Node<Self>>),
    AddAssign(Box<Node<Self>>, Box<Node<Self>>),
    SubAssign(Box<Node<Self>>, Box<Node<Self>>),
    MulAssign(Box<Node<Self>>, Box<Node<Self>>),
    DivAssign(Box<Node<Self>>, Box<Node<Self>>),
    ModAssign(Box<Node<Self>>, Box<Node<Self>>),
    LshAssign(Box<Node<Self>>, Box<Node<Self>>),
    RshAssign(Box<Node<Self>>, Box<Node<Self>>),
    AndAssign(Box<Node<Self>>, Box<Node<Self>>),
    XorAssign(Box<Node<Self>>, Box<Node<Self>>),
    OrAssign(Box<Node<Self>>, Box<Node<Self>>),
    Ternary(Box<Node<Self>>, Box<Node<Self>>, Box<Node<Self>>),
    LcOr(Box<Node<Self>>, Box<Node<Self>>),
    LcAnd(Box<Node<Self>>, Box<Node<Self>>),
    BwOr(Box<Node<Self>>, Box<Node<Self>>),
    BwXor(Box<Node<Self>>, Box<Node<Self>>),
    BwAnd(Box<Node<Self>>, Box<Node<Self>>),
    Eq(Box<Node<Self>>, Box<Node<Self>>),
    Ne(Box<Node<Self>>, Box<Node<Self>>),
    Lt(Box<Node<Self>>, Box<Node<Self>>),
    Le(Box<Node<Self>>, Box<Node<Self>>),
    Gt(Box<Node<Self>>, Box<Node<Self>>),
    Ge(Box<Node<Self>>, Box<Node<Self>>),
    Lsh(Box<Node<Self>>, Box<Node<Self>>),
    Rsh(Box<Node<Self>>, Box<Node<Self>>),
    Add(Box<Node<Self>>, Box<Node<Self>>),
    Sub(Box<Node<Self>>, Box<Node<Self>>),
    Mul(Box<Node<Self>>, Box<Node<Self>>),
    Div(Box<Node<Self>>, Box<Node<Self>>),
    Mod(Box<Node<Self>>, Box<Node<Self>>),
    Cast(Node<TypeName<'a>>, Box<Node<Self>>),
    UnInc(Box<Node<Self>>),
    UnDec(Box<Node<Self>>),
    Ref(Box<Node<Self>>),
    Deref(Box<Node<Self>>),
    Pos(Box<Node<Self>>),
    Neg(Box<Node<Self>>),
    BwNot(Box<Node<Self>>),
    LcNot(Box<Node<Self>>),
    SizeOfE(Box<Node<Self>>),
    SizeOfT(Node<TypeName<'a>>),
    Index(Box<Node<Self>>, Box<Node<Self>>),
    FnCall(Box<Node<Self>>, Vec<Node<Self>>),
    PfInc(Box<Node<Self>>),
    PfDec(Box<Node<Self>>),
    Dot(Box<Node<Self>>, &'a str),
    Arrow(Box<Node<Self>>, &'a str),
    Initialize(Node<TypeName<'a>>, InitList<'a>),
}

#[derive(Debug, Clone)]
pub enum TypeSpec<'a> {
    Void,
    Char,
    Short,
    Int,
    Long,
    Bool,
    Float,
    Double,
    Signed,
    Unsigned,
    Struct(Option<&'a str>, Option<Vec<StructItem<'a>>>),
    Union(Option<&'a str>, Option<Vec<StructItem<'a>>>),
    Enum(Option<&'a str>, Option<Vec<EnumItem<'a>>>),
    TypedefName(&'a str),
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeQual(u8);

bitflags! {
    impl TypeQual: u8 {
        const CONST = 1 << 0;
        const RESTRICT = 1 << 1;
        const VOLATILE = 1 << 2;
    }
}

pub(crate) enum DeclSpecToken<'a> {
    Spec(TypeSpec<'a>),
    Qual(TypeQual),
    StorageClass(StorageClass),
    FunctionSpec(FunctionSpec),
}

#[derive(Debug, Clone)]
pub struct DeclarationSpec<'a> {
    pub base_type: BaseType<'a>,
    pub qual: TypeQual,
    pub storage: StorageClass,
    pub fn_spec: FunctionSpec,
}

#[derive(Debug, Clone)]
pub enum BaseType<'a> {
    Void,
    SChar,
    SShort,
    SInt,
    SLong,
    SLongLong,
    UChar,
    UShort,
    UInt,
    ULong,
    ULongLong,
    Bool,
    Float,
    Double,
    LongDouble,
    Struct(Option<&'a str>, Option<Vec<StructItem<'a>>>),
    Union(Option<&'a str>, Option<Vec<StructItem<'a>>>),
    Enum(Option<&'a str>, Option<Vec<EnumItem<'a>>>),
    TypedefName(&'a str),
}

#[derive(Debug, Clone)]
pub struct StructItem<'a> {
    pub typ: Node<DeclarationSpec<'a>>,
    pub item: Option<Node<Declarator<'a>>>,
    pub bits: Option<Node<Expr<'a>>>,
}

#[derive(Debug, Clone)]
pub struct EnumItem<'a> {
    pub ident: &'a str,
    pub number: Option<Node<Expr<'a>>>,
}

impl<'a> BaseType<'a> {
    pub fn from_type_specs<TS: Iterator<Item = TypeSpec<'a>>>(mut ts: TS) -> Result<Self, &'static str> {
        let mut base = None;
        let mut shorts = 0;
        let mut longs = 0;
        let mut signed = false;
        let mut unsigned = false;

        for s in ts {
            match s {
                TypeSpec::Short => shorts += 1,
                TypeSpec::Long => longs += 1,
                TypeSpec::Signed if !signed => signed = true,
                TypeSpec::Signed => return Err(">1 `signed`"),
                TypeSpec::Unsigned if !unsigned => unsigned = true,
                TypeSpec::Unsigned => return Err(">1 `unsigned`"),
                _ if base.is_none() => base = Some(s),
                _ => return Err("<2 base type"),
            }
        }

        if signed && unsigned {
            return Err("both signed and unsigned specified");
        }

        match (base.unwrap_or(TypeSpec::Int), shorts, longs, signed, unsigned) {
            (TypeSpec::Void, 0, 0, false, false) => Ok(BaseType::Void),
            (TypeSpec::Bool, 0, 0, false, false) => Ok(BaseType::Bool),
            (TypeSpec::Int, 0, 0, _, false) => Ok(BaseType::SInt),
            (TypeSpec::Int, 0, 0, _, true) => Ok(BaseType::UInt),
            (TypeSpec::Int, 0, 1, _, false) => Ok(BaseType::SLong),
            (TypeSpec::Int, 0, 1, _, true) => Ok(BaseType::ULong),
            (TypeSpec::Int, 0, 2, _, false) => Ok(BaseType::SLongLong),
            (TypeSpec::Int, 0, 2, _, true) => Ok(BaseType::ULongLong),
            (TypeSpec::Int, 1, 0, _, false) => Ok(BaseType::SShort),
            (TypeSpec::Int, 1, 0, _, true) => Ok(BaseType::UShort),
            (TypeSpec::Char, 0, 0, _, false) => Ok(BaseType::SChar),
            (TypeSpec::Char, 0, 0, _, true) => Ok(BaseType::UChar),
            (TypeSpec::Float, 0, 0, false, false) => Ok(BaseType::Float),
            (TypeSpec::Double, 0, 0, false, false) => Ok(BaseType::Double),
            (TypeSpec::Double, 0, 1, false, false) => Ok(BaseType::LongDouble),
            (TypeSpec::Struct(s, i), 0, 0, false, false) => Ok(BaseType::Struct(s, i)),
            (TypeSpec::Union(s, i), 0, 0, false, false) => Ok(BaseType::Union(s, i)),
            (TypeSpec::Enum(s, i), 0, 0, false, false) => Ok(BaseType::Enum(s, i)),
            (TypeSpec::TypedefName(s), 0, 0, false, false) => Ok(BaseType::TypedefName(s)),
            _ => Err("no such type"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeName<'a> {
    pub spec: Node<DeclarationSpec<'a>>,
    pub decl: Option<Box<AbsDeclarator<'a>>>,
}

#[derive(Debug, Clone)]
pub struct Declaration<'a> {
    pub typ: Node<DeclarationSpec<'a>>,
    pub inits: Vec<(Node<Declarator<'a>>, Option<Node<Initializer<'a>>>)>,
}

#[derive(Debug, Clone)]
pub enum Declarator<'a> {
    Root(&'a str, Node<DeclarationSpec<'a>>),
    Pointer(Box<Node<Self>>, TypeQual),
    Array(Box<Node<Self>>, TypeQual, Option<Node<Expr<'a>>>),
    Function(Box<Node<Self>>, ParamTypeList<'a>),
}

#[derive(Debug, Clone)]
pub enum Initializer<'a> {
    Expression(Expr<'a>),
    InitList(InitList<'a>),
}
pub type InitList<'a> = Vec<(Option<Node<Designator<'a>>>, Box<Node<Initializer<'a>>>)>;

#[derive(Debug, Clone)]
pub enum Designator<'a> {
    Field(&'a str),
    Index(Node<Expr<'a>>),
}

#[derive(Debug, Clone)]
pub enum AbsDeclarator<'a> {
    Pointer(Option<Box<Self>>),
    Array(Option<Box<Self>>, TypeQual, Option<Node<Expr<'a>>>),
    Function(Option<Box<Self>>, ParamTypeList<'a>),
}

#[derive(Debug, Clone)]
pub struct ParamTypeList<'a> {
    pub param: Vec<Node<ParamDeclaration<'a>>>,
    pub more: bool,
}

#[derive(Debug, Clone)]
pub struct ParamDeclaration<'a> {
    pub spec: Node<DeclarationSpec<'a>>,
    pub decl: MayAbsDeclarator<'a>,
}

#[derive(Debug, Clone)]
pub enum MayAbsDeclarator<'a> {
    NonAbs(Node<Declarator<'a>>),
    AbsDecl(Option<AbsDeclarator<'a>>),
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct StorageClass(u8);

bitflags! {
    impl StorageClass: u8 {
        const TYPEDEF = 1 << 0;
        const EXTERN = 1 << 1;
        const STATIC = 1 << 2;
        const AUTO = 1 << 3;
        const REGISTER = 1 << 4;
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionSpec(u8);

bitflags! {
    impl FunctionSpec: u8 {
        const INLINE = 1 << 0;
    }
}

#[derive(Debug, Clone)]
pub enum ExternalDecl<'a> {
    Function(FunctionDef<'a>),
    Declaration(Declaration<'a>),
}

#[derive(Debug, Clone)]
pub struct FunctionDef<'a> {
    pub decl_spec: DeclarationSpec<'a>,
    pub declarator: Node<Declarator<'a>>,
    pub declarations: Vec<Node<Declaration<'a>>>,
    pub body: CompoundStmt<'a>,
}

pub type CompoundStmt<'a> = Vec<Node<BlockItem<'a>>>;

#[derive(Debug, Clone)]
pub enum BlockItem<'a> {
    Declaration(Declaration<'a>),
    Statement(Statement<'a>),
}

#[derive(Debug, Clone)]
pub enum Statement<'a> {
    Label(&'a str, Box<Node<Self>>),
    Case(Option<Node<Expr<'a>>>, Box<Node<Self>>),

    Compound(CompoundStmt<'a>),

    Expression(Expr<'a>),

    IfElse(Node<Expr<'a>>, Box<Node<Self>>, Option<Box<Node<Self>>>),
    Select(Node<Expr<'a>>, Box<Node<Self>>),

    While(Node<Expr<'a>>, Box<Node<Self>>),
    DoWhile(Node<Expr<'a>>, Box<Node<Self>>),
    For3E(Vec<Option<Node<Expr<'a>>>>, Box<Node<Self>>),
    For2E(Node<Declaration<'a>>, Vec<Option<Node<Expr<'a>>>>, Box<Node<Self>>),

    Goto(&'a str),
    Continue,
    Break,
    Return(Option<Node<Expr<'a>>>),

    Empty,
}
