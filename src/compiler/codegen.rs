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
            self.compile_ext_decl(e.as_deref());
        }

        if !self.errors.is_empty() { return Err(self.errors); }

        println!("{}", self.builder);
        let p = self.builder.done();

        let p = arch::VCode::generate::<_, regalloc::graph::GraphAlloc<_>>(&p, arch::urcl::UrclSelector::new());
        p.emit_assembly(&mut std::io::stdout()).unwrap();

        Ok(())
    }

    fn compile_ext_decl(&mut self, ed: Node<&'a ExternalDecl<'a>>) {
        match ed.node {
            ExternalDecl::Function(f) => self.compile_function(f),
            ExternalDecl::Declaration(d) => {
                for (v, i) in d.inits.iter() {
                    let (v, typ) = Type::from_spec_declarator(&d.typ, v);
                    println!("{v} {typ:?} {i:?}");
                }
            },
        }
    }

    fn compile_function(&mut self, func: &'a FunctionDef<'a>) {
        let (id, typ) = Type::from_spec_declarator(&func.decl_spec, &func.declarator);

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
                    }, Some)).collect()
                } else { vec![] };

                let (func, arg) = self.builder.create_function(
                    Linkage::Public,
                    id,
                    args,
                    rt.node.to_ssa_vtype(),
                );
                self.builder.position_at_function(func);

                let b = self.builder.push_block();
                self.builder.position_at_bb(b);
            }
            _ => self.errors.push((CodegenError::TypeMustBeFn, typ.span)),
        }

        for bi in func.body.iter() {
            self.compile_block_item(bi.as_deref());
        }
    }

    fn compile_block_item(&mut self, bi: Node<&BlockItem<'a>>) {
        match &bi.node {
            BlockItem::Statement(stmt) => self.compile_statement(Node { node: stmt, span: bi.span.clone() }),
            _ => todo!("{bi:?}"),
        }
    }

    fn compile_statement(&mut self, stmt: Node<&Statement<'a>>) {
        match stmt.node {
            Statement::Return(rv) => {
                let rv = rv.as_ref().map(|r| {
                    let (v, t) = self.compile_expr(r.as_deref());
                    v.of_type(t.to_ssa_vtype().unwrap())
                });
                self.builder.set_ret(rv);

                let b = self.builder.push_block();
                self.builder.position_at_bb(b);
            },
            Statement::Expression(e) => { self.compile_expr(Node { node: e, span: stmt.span.clone() }); },
            Statement::IfElse(c, a, b) => {
                let c = self.compile_expr(c.as_deref());
                let c = c.0.of_type(c.1.to_ssa_vtype().unwrap());

                let ab = self.builder.push_block();
                let bb = self.builder.push_block();
                let r = self.builder.push_block();
                self.builder.set_cond_br(c.try_into().unwrap(), ab, bb);

                self.builder.position_at_bb(ab);
                self.compile_statement(a.as_deref());
                self.builder.set_uncond_br(r);

                self.builder.position_at_bb(bb);
                if let Some(b) = b { self.compile_statement(b.as_deref()); }
                self.builder.set_uncond_br(r);

                self.builder.position_at_bb(r);
            },
            Statement::Empty => {},
            _ => todo!("{stmt:?}"),
        }
    }

    fn compile_expr(&mut self, expr: Node<&Expr<'a>>) -> (value::ValueId, Type<'a>) {
        match &expr.node {
            Expr::IntConst(ic) => {
                let t = DeclarationSpec::int_const_type(ic);
                let v = self.builder.build_int_const(t.base_type.int_size(), ic.value as _);
                (v.id, Type::Root(t))
            },
            Expr::CharConst(CharConst { ch, wide: false }) => {
                let t = const { &DeclarationSpec::base_type(BaseType::SInt) };
                let v = self.builder.build_int_const(t.base_type.int_size(), *ch as _);
                (v.id, Type::Root(t))
            },
            Expr::Comma(op) => {
                for e in op.iter().take(op.len() - 1) {
                    self.compile_expr(e.as_deref());
                }

                self.compile_expr(op.last().unwrap().as_deref())
            },
            _ => todo!("{expr:?}"),
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
                let (id, t) = Self::from_spec_declarator(spec, decl);
                (Some(id), t)
            },
            MayAbsDeclarator::AbsDecl(decl) => (None, Self::from_spec_abs_decl(spec, decl)),
        }
    }

    fn wrap_by_decl(self, ss: Span, decl: &'a Node<Declarator<'a>>) -> (&'a str, Node<Self>) {
        match &decl.node {
            Declarator::Root(r) => (r, Node { node: self, span: ss }),
            Declarator::Pointer(e, q) => Type::Pointer(Box::new(Node { node: self, span: decl.span.clone() }), *q).wrap_by_decl(ss, e),
            Declarator::Function(r, p) => {
                let e = p.param.iter().map(|e| Self::from_spec_may_abs_decl(&e.node.spec, &e.node.decl).1).collect();
                Self::Function(Box::new(Node { node: self, span: decl.span.clone() }), e, p.more).wrap_by_decl(ss, r)
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

    fn root(&'a self) -> &'a DeclarationSpec<'a> {
        match self {
            Self::Root(r) => r,
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
