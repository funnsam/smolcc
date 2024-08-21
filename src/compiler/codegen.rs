use funnssa::*;
use super::ast::*;

pub struct Codegen<'a> {
    ast: &'a TranslationUnit<'a>,
    builder: builder::Builder<'a>,
    errors: Vec<(CodegenError, Span)>,
}

impl<'a> Codegen<'a> {
    pub fn new(ast: &'a TranslationUnit<'a>) -> Self {
        Self {
            ast,
            builder: builder::Builder::new(),
            errors: Vec::new(),
        }
    }

    pub fn compile(mut self) -> Result<(), Vec<(CodegenError, Span)>> {
        for e in self.ast.iter() {
            self.compile_ext_decl(e);
        }

        if !self.errors.is_empty() { return Err(self.errors); }

        let p = self.builder.done();
        let p = arch::VCode::generate::<_, regalloc::graph::GraphAlloc<_>>(&p, arch::x86_64::X64Selector::new());
        p.emit_assembly(&mut std::io::stdout()).unwrap();

        Ok(())
    }

    fn compile_ext_decl(&mut self, ed: &'a Node<ExternalDecl<'a>>) {
        match &ed.node {
            ExternalDecl::Function(f) => self.compile_function(f),
            ExternalDecl::Declaration(d) => {
                for (v, i) in d.inits.iter() {
                    let (v, typ) = Type::from_spec_declarator(&d.typ, v);
                    println!("{v} {typ:?}");
                }
            },
        }
    }

    fn compile_function(&mut self, func: &'a FunctionDef<'a>) {
        let (id, typ) = Type::from_spec_declarator(&func.decl_spec, &func.declarator);
        println!("{id} {typ:?}");

        match &typ.node {
            Type::Function(rt, args, _) => {
                let args = if args.len() != 1 || !matches!(args[0].node, Type::Root(
                    DeclarationSpec {
                        base_type: BaseType::Void,
                        qual: TypeQual(0),
                        fn_spec: FunctionSpec(0),
                        ..
                    })
                ) {
                    args.iter().filter_map(|a| a.node.to_ssa_vtype().map_or_else(|| {
                        self.errors.push((CodegenError::UnexpectedVoidType, a.span.clone()));
                        None
                    }, |a| Some(a))).collect()
                } else { vec![] };

                let (func, arg) = self.builder.create_function(
                    Linkage::Public,
                    id,
                    args,
                    rt.node.to_ssa_vtype(),
                );
                self.builder.position_at_function(func);
            }
            _ => self.errors.push((CodegenError::TypeMustBeFn, typ.span)),
        }
    }
}

#[derive(Debug, Clone)]
enum Type<'a> {
    Root(&'a DeclarationSpec<'a>),
    Pointer(Box<Node<Self>>, TypeQual),
    Function(Box<Node<Self>>, Vec<Node<Self>>, bool),
}

impl<'a> Type<'a> {
    fn from_spec_declarator(spec: &'a Node<DeclarationSpec<'a>>, decl: &'a Node<Declarator<'a>>) -> (&'a str, Node<Self>) {
        Type::Root(&spec.node).wrap_by_decl(spec.span.clone(), decl)
    }

    fn from_spec_abs_decl(spec: &'a Node<DeclarationSpec<'a>>, decl: &'a Option<Node<AbsDeclarator<'a>>>) -> Node<Self> {
        Type::Root(&spec.node).wrap_by_abs_decl(spec.span.clone(), decl.as_ref())
    }

    fn from_spec_may_abs_decl(spec: &'a Node<DeclarationSpec<'a>>, decl: &'a MayAbsDeclarator<'a>) -> (Option<&'a str>, Node<Self>) {
        match decl {
            MayAbsDeclarator::NonAbs(decl) => {
                let (id, t) = Self::from_spec_declarator(spec, &decl);
                (Some(id), t)
            },
            MayAbsDeclarator::AbsDecl(decl) => (None, Self::from_spec_abs_decl(spec, decl)),
        }
    }

    fn wrap_by_decl(self, ss: Span, decl: &'a Node<Declarator<'a>>) -> (&'a str, Node<Self>) {
        match &decl.node {
            Declarator::Root(r) => (r, Node { node: self, span: ss }),
            Declarator::Pointer(e, q) => Type::Pointer(Box::new(Node { node: self, span: decl.span.clone() }), *q).wrap_by_decl(ss, &e),
            Declarator::Function(r, p) => {
                let e = p.param.iter().map(|e| Self::from_spec_may_abs_decl(&e.node.spec, &e.node.decl).1).collect();
                Self::Function(Box::new(Node { node: self, span: decl.span.clone() }), e, p.more).wrap_by_decl(ss, &r)
            },
            _ => todo!("{decl:?}"),
        }
    }

    fn wrap_by_abs_decl(self, ss: Span, decl: Option<&'a Node<AbsDeclarator<'a>>>) -> Node<Self> {
        match decl {
            Some(Node { node: AbsDeclarator::Pointer(e), span }) => {
                Type::Pointer(Box::new(Node { node: self, span: span.clone() }), TypeQual::default()).wrap_by_abs_decl(ss, e.as_deref())
            },
            Some(Node { node: AbsDeclarator::Function(r, p), span }) => {
                let e = p.param.iter().map(|e| Self::from_spec_may_abs_decl(&e.node.spec, &e.node.decl).1).collect();
                Self::Function(Box::new(Node { node: self, span: span.clone() }), e, p.more).wrap_by_abs_decl(ss, r.as_deref())
            },
            None => Node { node: self, span: ss },
            _ => todo!("{decl:?}"),
        }
    }

    fn root(&'a self) -> &'a Self {
        match self {
            Self::Root(r) => self,
            Self::Pointer(p, _) => p.node.root(),
            Self::Function(r, ..) => r.node.root(),
        }
    }

    fn to_ssa_vtype(&self) -> Option<types::ValueType> {
        match self {
            Self::Root(DeclarationSpec { base_type: BaseType::Void, .. }) => None,
            Self::Root(DeclarationSpec { base_type: BaseType::SInt | BaseType::UInt, .. }) => Some(types::ValueType::Int(32)),
            Self::Pointer(..) | Self::Function(..) => Some(types::ValueType::Ptr),

            _ => todo!("{self:?}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::Display)]
pub enum CodegenError {
    #[strum(serialize = "expected a function type")]
    TypeMustBeFn,
    #[strum(serialize = "unexpected void type")]
    UnexpectedVoidType,
}
