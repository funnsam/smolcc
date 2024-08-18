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

#[derive(Debug, Clone, PartialEq, Eq)]
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeQual(u8);

impl TypeQual {
    const _CONST: u8 = 0b001;
    const _RESTR: u8 = 0b010;
    const _VOLAT: u8 = 0b100;

    pub const CONST: Self = Self(Self::_CONST);
    pub const RESTRICT: Self = Self(Self::_RESTR);
    pub const VOLATILE: Self = Self(Self::_VOLAT);
}

impl core::ops::BitOr for TypeQual {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

pub enum TypeSpecOrQual<'a> {
    Spec(TypeSpec<'a>),
    Qual(TypeQual),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeSpecQual<'a> {
    pub base_type: BaseType<'a>,
    pub qual: TypeQual,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    Float,
    Double,
    LongDouble,
    Struct(&'a str),
    Union(&'a str),
    Enum(&'a str),
    TypedefName(&'a str),
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
            (TypeSpec::Void, 0, 0, false, false) => Ok(BaseType::Void),
            (TypeSpec::Struct(s), 0, 0, false, false) => Ok(BaseType::Struct(s)),
            (TypeSpec::Union(s), 0, 0, false, false) => Ok(BaseType::Union(s)),
            (TypeSpec::Enum(s), 0, 0, false, false) => Ok(BaseType::Enum(s)),
            (TypeSpec::TypedefName(s), 0, 0, false, false) => Ok(BaseType::TypedefName(s)),
            _ => Err("no such type"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeName<'a> {
    pub type_sq: TypeSpecQual<'a>,
    // pub abs_decl: Vec<AbstractDecl<'a>>,
}

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum AbstractDecl<'a> {
//     Pointer,
//     Array(Option<Expr<'a>>),
// }

#[derive(Debug, Clone)]
pub struct Declaration<'a> {
    // TODO: storage class & func spec
    pub typ: Node<TypeName<'a>>,
    pub inits: Vec<(&'a str, Option<Node<Expr<'a>>>)>,
}
