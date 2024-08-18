#![allow(unused)]

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

impl IntSuffix {
    const _U: u8 = 0b001;
    const _L: u8 = 0b010;
    const _LL: u8 = 0b100;

    pub const U: Self = Self(Self::_U);
    pub const UL: Self = Self(Self::_U | Self::_L);
    pub const ULL: Self = Self(Self::_U | Self::_LL);
    pub const L: Self = Self(Self::_L);
    pub const LL: Self = Self(Self::_LL);
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
    BwLsh(Box<Node<Self>>, Box<Node<Self>>),
    BwRsh(Box<Node<Self>>, Box<Node<Self>>),
    Add(Box<Node<Self>>, Box<Node<Self>>),
    Sub(Box<Node<Self>>, Box<Node<Self>>),
    Mul(Box<Node<Self>>, Box<Node<Self>>),
    Div(Box<Node<Self>>, Box<Node<Self>>),
    Mod(Box<Node<Self>>, Box<Node<Self>>),
    // Cast(Box<Node<Type<'a>>>, Box<Node<Self>>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeSpec<'a> {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Struct(&'a str),
    Union(&'a str),
    Enum(&'a str),
    TypedefName(&'a str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeQual {
    Const,
    Restrict,
    Volatile,
}
