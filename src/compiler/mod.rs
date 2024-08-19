pub mod ast;

peg::parser! {
    pub grammar parser() for str {
        use super::ast::*;

        // temp testing thing
        pub rule numbers() -> Vec<Node<ExternalDecl<'input>>>
            = _ l:(external_decl() ** _) _ { l }

        // utilities
        rule hex_digits()
            = ['0'..='9' | 'a'..='f' | 'A'..='F']+ {};

        rule _
            = quiet! { [' ' | '\n' | '\t']* ("/*" b_comment_c()* "*/" _)? ("//" [^ '\n']* _)? }
        rule b_comment_c()
            = [^ '*']
            / "*" !"/";
        rule __
            = quiet! { &[' ' | '\n' | '\t' | '[' | ']' | '{' | '}' | '.' | '-' | '+' | '&' | '*' | '~' | '!' | '/' | '%' | '<' | '>' | '=' | '?' | ':' | ';' | ',' | '#' | '(' | ')'] };
        rule ___
            = quiet! { __ _ }

        rule ident() -> &'input str
            = quiet! { _ i:$(['_' | 'a'..='z' | 'A'..='Z']['_' | '0'..='9' | 'a'..='z' | 'A'..='Z']*) _ {?
                (!matches!(i, "auto" | "break" | "case" | "char" | "const" | "continue" | "default" | "do" | "double" | "else" | "enum" | "extern" | "float" | "for" | "goto" | "if" | "inline" | "int" | "long" | "register" | "restrict" | "return" | "short" | "signed" | "sizeof" | "static" | "struct" | "switch" | "typedef" | "union" | "unsigned" | "void" | "volatile" | "while" | "_Bool" | "_Complex" | "_Imaginary")).then_some(i).ok_or("identifier can't be a keyword")
            } }
            / expected!("identifier");

        // 6.4.4.1 integer constant
        rule int_const() -> IntConst
            = quiet! { _ value:dec_const() suffix:int_suffix() _ { IntConst { value, suffix, infer_unsigned: false } } }
            / quiet! { _ value:dec_const() _ { IntConst { value, suffix: IntSuffix::default(), infer_unsigned: false } } }
            / quiet! { _ value:oct_or_hex_const() suffix:int_suffix() _ { IntConst { value, suffix, infer_unsigned: true } } }
            / quiet! { _ value:oct_or_hex_const() _ { IntConst { value, suffix: IntSuffix::default(), infer_unsigned: true } } }
            / expected!("integer constant");

        rule dec_const() -> u64
            = n:$(['1'..='9']['0'..='9']*) {? n.parse().or(Err("value too big")) };
        rule oct_or_hex_const() -> u64
            = "0" n:$(['0'..='7']*) {? u64::from_str_radix(n, 8).or(Err("value too big")) }
            / "0x" n:$(hex_digits()) {? u64::from_str_radix(n, 16).or(Err("value too big")) }
            / "0" { 0 };

        rule ll() = "ll" / "LL";
        rule int_suffix() -> IntSuffix
            = ['u' | 'U']ll() { IntSuffix::U | IntSuffix::LL }
            / ['u' | 'U']['l' | 'L'] { IntSuffix::U | IntSuffix::L }
            / ['u' | 'U'] { IntSuffix::U }
            / ll()['u' | 'U'] { IntSuffix::LL | IntSuffix::U }
            / ll() { IntSuffix::LL }
            / ['l' | 'L']['u' | 'U'] { IntSuffix::L | IntSuffix::U }
            / ['l' | 'L'] { IntSuffix::L };

        // 6.4.4.4 character constant
        rule char_const() -> CharConst
            = quiet! { _ "'" ch:c_char() "'" _ { CharConst { ch, wide: false } } }
            / quiet! { _ "L'" ch:c_char() "'" _ { CharConst { ch, wide: true } } }
            / expected!("character constant");

        rule c_char() -> u32
            = ch:$([^ '\'' | '\\' | '\n']) { ch.chars().next().unwrap() as u32 }
            / ch:esc_seq() { ch }

        rule esc_seq() -> u32
            = r"\'" { '\'' as u32 }
            / r#"\""# { '"' as u32 }
            / r"\?" { '?' as u32 }
            / r"\\" { '\\' as u32 }
            / r"\a" { '\x07' as u32 }
            / r"\b" { '\x08' as u32 }
            / r"\e" { '\x1b' as u32 }
            / r"\f" { '\x0c' as u32 }
            / r"\n" { '\n' as u32 }
            / r"\r" { '\r' as u32 }
            / r"\t" { '\t' as u32 }
            / r"\v" { '\x0b' as u32 }
            / r"\" n:$(['0'..='7']*<1, 3>) { u32::from_str_radix(n, 8).unwrap() }
            / r"\x" n:$(hex_digits()) {? u32::from_str_radix(n, 16).or(Err("value too big")) };

        // 6.4.5 string literal
        rule string_lit() -> StringLit
            = quiet! { _ "\"" string:s_char()* "\"" _ { StringLit { string, wide: false } } }
            / quiet! { _ "L\"" string:s_char()* "\"" _ { StringLit { string, wide: true } } }
            / expected!("string literal");

        rule s_char() -> u32
            = ch:$([^ '"' | '\\' | '\n']) { ch.chars().next().unwrap() as u32 }
            / ch:esc_seq() { ch }

        // 6.5 expressions
        rule prim_expr() -> Node<Expr<'input>>
            = s:position!() node:_prim_expr() e:position!() { Node { node, span: s..e } };
        rule _prim_expr() -> Expr<'input>
            = cc:char_const() { Expr::CharConst(cc) }
            / sl:string_lit() { Expr::StringLit(sl) }
            / ic:int_const() { Expr::IntConst(ic) }
            / id:ident() { Expr::Ident(id) }
            / "(" ex: expr() ")" { ex.node };

        rule expr() -> Node<Expr<'input>>
            = quiet! { e:(assign_expr() ++ (_ "," _)) { if e.len() == 1 {
                let mut e = e;
                e.swap_remove(0)
            } else {
                Node { span: e[0].span.start..e.last().unwrap().span.end, node: Expr::Comma(e) }
            }} }
            / expected!("expression");
        rule assign_expr() -> Node<Expr<'input>> = precedence! {
            l:@ _ "=" _ r:(@) { Node { span: l.span.start..r.span.end, node: Expr::Assign(Box::new(l), Box::new(r)) } }
            l:@ _ "+=" _ r:(@) { Node { span: l.span.start..r.span.end, node: Expr::AddAssign(Box::new(l), Box::new(r)) } }
            l:@ _ "-=" _ r:(@) { Node { span: l.span.start..r.span.end, node: Expr::SubAssign(Box::new(l), Box::new(r)) } }
            l:@ _ "*=" _ r:(@) { Node { span: l.span.start..r.span.end, node: Expr::MulAssign(Box::new(l), Box::new(r)) } }
            l:@ _ "/=" _ r:(@) { Node { span: l.span.start..r.span.end, node: Expr::DivAssign(Box::new(l), Box::new(r)) } }
            l:@ _ "%=" _ r:(@) { Node { span: l.span.start..r.span.end, node: Expr::ModAssign(Box::new(l), Box::new(r)) } }
            l:@ _ "<<=" _ r:(@) { Node { span: l.span.start..r.span.end, node: Expr::LshAssign(Box::new(l), Box::new(r)) } }
            l:@ _ ">>=" _ r:(@) { Node { span: l.span.start..r.span.end, node: Expr::RshAssign(Box::new(l), Box::new(r)) } }
            l:@ _ "&=" _ r:(@) { Node { span: l.span.start..r.span.end, node: Expr::AndAssign(Box::new(l), Box::new(r)) } }
            l:@ _ "^=" _ r:(@) { Node { span: l.span.start..r.span.end, node: Expr::XorAssign(Box::new(l), Box::new(r)) } }
            l:@ _ "|=" _ r:(@) { Node { span: l.span.start..r.span.end, node: Expr::OrAssign(Box::new(l), Box::new(r)) } }
            --
            e:const_expr() { e }
        };
        rule const_expr() -> Node<Expr<'input>> = precedence! {
            c:@ _ "?" _ l:expr() _ ":" _ r:(@) { Node { span: c.span.start..r.span.end, node: Expr::Ternary(Box::new(c), Box::new(l), Box::new(r)) } }
            --
            l:(@) _ "||" _ r:@ { Node { span: l.span.start..r.span.end, node: Expr::LcOr(Box::new(l), Box::new(r)) } }
            --
            l:(@) _ "&&" _ r:@ { Node { span: l.span.start..r.span.end, node: Expr::LcAnd(Box::new(l), Box::new(r)) } }
            --
            l:(@) _ "|" _ r:@ { Node { span: l.span.start..r.span.end, node: Expr::BwOr(Box::new(l), Box::new(r)) } }
            --
            l:(@) _ "^" _ r:@ { Node { span: l.span.start..r.span.end, node: Expr::BwXor(Box::new(l), Box::new(r)) } }
            --
            l:(@) _ "&" _ r:@ { Node { span: l.span.start..r.span.end, node: Expr::BwAnd(Box::new(l), Box::new(r)) } }
            --
            l:(@) _ "==" _ r:@ { Node { span: l.span.start..r.span.end, node: Expr::Eq(Box::new(l), Box::new(r)) } }
            l:(@) _ "!=" _ r:@ { Node { span: l.span.start..r.span.end, node: Expr::Ne(Box::new(l), Box::new(r)) } }
            --
            l:(@) _ "<" _ r:@ { Node { span: l.span.start..r.span.end, node: Expr::Lt(Box::new(l), Box::new(r)) } }
            l:(@) _ "<=" _ r:@ { Node { span: l.span.start..r.span.end, node: Expr::Le(Box::new(l), Box::new(r)) } }
            l:(@) _ ">" _ r:@ { Node { span: l.span.start..r.span.end, node: Expr::Gt(Box::new(l), Box::new(r)) } }
            l:(@) _ ">=" _ r:@ { Node { span: l.span.start..r.span.end, node: Expr::Ge(Box::new(l), Box::new(r)) } }
            --
            l:(@) _ "<<" _ r:@ { Node { span: l.span.start..r.span.end, node: Expr::Lsh(Box::new(l), Box::new(r)) } }
            l:(@) _ ">>" _ r:@ { Node { span: l.span.start..r.span.end, node: Expr::Rsh(Box::new(l), Box::new(r)) } }
            --
            l:(@) _ "+" _ r:@ { Node { span: l.span.start..r.span.end, node: Expr::Add(Box::new(l), Box::new(r)) } }
            l:(@) _ "-" _ r:@ { Node { span: l.span.start..r.span.end, node: Expr::Sub(Box::new(l), Box::new(r)) } }
            --
            l:(@) _ "*" _ r:@ { Node { span: l.span.start..r.span.end, node: Expr::Mul(Box::new(l), Box::new(r)) } }
            l:(@) _ "/" _ r:@ { Node { span: l.span.start..r.span.end, node: Expr::Div(Box::new(l), Box::new(r)) } }
            l:(@) _ "%" _ r:@ { Node { span: l.span.start..r.span.end, node: Expr::Mod(Box::new(l), Box::new(r)) } }
            --
            l:position!() "(" _ t:type_name() _ ")" _ x:(@) { Node { span: l..x.span.end, node: Expr::Cast(t, Box::new(x)) } }
            --
            l:position!() "++" _ x:(@) { Node { span: l..x.span.end, node: Expr::UnInc(Box::new(x)) } }
            l:position!() "--" _ x:(@) { Node { span: l..x.span.end, node: Expr::UnInc(Box::new(x)) } }
            l:position!() "&" _ x:(@) { Node { span: l..x.span.end, node: Expr::Ref(Box::new(x)) } }
            l:position!() "*" _ x:(@) { Node { span: l..x.span.end, node: Expr::Deref(Box::new(x)) } }
            l:position!() "+" _ x:(@) { Node { span: l..x.span.end, node: Expr::Pos(Box::new(x)) } }
            l:position!() "-" _ x:(@) { Node { span: l..x.span.end, node: Expr::Neg(Box::new(x)) } }
            l:position!() "~" _ x:(@) { Node { span: l..x.span.end, node: Expr::BwNot(Box::new(x)) } }
            l:position!() "!" _ x:(@) { Node { span: l..x.span.end, node: Expr::LcNot(Box::new(x)) } }
            l:position!() "sizeof" ___ x:(@) { Node { span: l..x.span.end, node: Expr::SizeOfE(Box::new(x)) } }
            l:position!() "sizeof" ___ "(" _ t:type_name() _ ")" r:position!() { Node { span: l..r, node: Expr::SizeOfT(t) } }
            --
            x:(@) _ "[" _ i:expr() _ "]" r:position!() { Node { span: x.span.start..r, node: Expr::Index(Box::new(x), Box::new(i)) } }
            x:(@) _ "(" _ i:(assign_expr() ** (_ "," _)) _ ")" r:position!() { Node { span: x.span.start..r, node: Expr::FnCall(Box::new(x), i) } }
            x:(@) _ "++" r:position!() { Node { span: x.span.start..r, node: Expr::PfInc(Box::new(x)) } }
            x:(@) _ "--" r:position!() { Node { span: x.span.start..r, node: Expr::PfDec(Box::new(x)) } }
            x:(@) _ "." _ i:ident() r:position!() { Node { span: x.span.start..r, node: Expr::Dot(Box::new(x), i) } }
            x:(@) _ "->" _ i:ident() r:position!() { Node { span: x.span.start..r, node: Expr::Arrow(Box::new(x), i) } }
            // TODO: {init} initialization
            --
            e:prim_expr() { e }
        };

        // 6.7.1
        rule storage_class() -> StorageClass
            = "typedef" __ { StorageClass::TYPEDEF };

        // 6.7.2 type specifiers
        rule type_spec() -> TypeSpec<'input>
            = quiet! { "void" __ { TypeSpec::Void } }
            / quiet! { "char" __ { TypeSpec::Char } }
            / quiet! { "short" __ { TypeSpec::Short } }
            / quiet! { "int" __ { TypeSpec::Int } }
            / quiet! { "long" __ { TypeSpec::Long } }
            / quiet! { "float" __ { TypeSpec::Float } }
            / quiet! { "double" __ { TypeSpec::Double } }
            / quiet! { "signed" __ { TypeSpec::Signed } }
            / quiet! { "unsigned" __ { TypeSpec::Unsigned } }
            / quiet! { "_Bool" __ { TypeSpec::Bool } }
            / quiet! { "struct" __ i:ident()? _ "{" _ e:struct_items() _ "}" { TypeSpec::Struct(i, Some(e)) } }
            / quiet! { "struct" __ i:ident() { TypeSpec::Struct(Some(i), None) } }
            / quiet! { "union" __ i:ident()? _ "{" _ e:struct_items() _ "}" { TypeSpec::Union(i, Some(e)) } }
            / quiet! { "union" __ i:ident() { TypeSpec::Union(Some(i), None) } }
            / quiet! { "enum" __ i:ident()? _ "{" _ e:(enumerator() ++ (_ "," _)) ","? _ "}" { TypeSpec::Enum(i, Some(e)) } }
            / quiet! { "enum" __ i:ident() { TypeSpec::Enum(Some(i), None) } }
            / quiet! { i:ident() { TypeSpec::TypedefName(i) } }
            / expected!("type specifier");

        rule struct_items() -> Vec<StructItem<'input>>
            = s:(struct_item() ** _) { s.into_iter().flatten().collect() };
        rule struct_item() -> Vec<StructItem<'input>>
            = l:position!() t:spec_qual_list() r:position!() _ d:(struct_declor(&t, l..r) ** (_ "," _)) _ ";" { d };
        rule struct_declor(typ: &DeclarationSpec<'input>, ts: Span) -> StructItem<'input>
            = item:declarator()? _ ":" _ b:const_expr() { StructItem { typ: Node { node: typ.clone(), span: ts.clone() }, item, bits: Some(b) } }
            / d:declarator() { StructItem { typ: Node { node: typ.clone(), span: ts.clone() }, item: Some(d), bits: None } };

        rule enumerator() -> EnumItem<'input>
            = ident:ident() _ "=" _ expr:const_expr() { EnumItem { ident, number: Some(expr) } }
            / ident:ident() { EnumItem { ident, number: None } };

        rule type_spec_or_qual() -> DeclSpecToken<'input>
            = q:type_qual() { DeclSpecToken::Qual(q) }
            / s:type_spec() { DeclSpecToken::Spec(s) };
        rule spec_qual_list() -> DeclarationSpec<'input>
            = sq:(type_spec_or_qual() ++ (___ !ident())) {? // NOTE: `qual()+ TypedefName` no work
                Ok(DeclarationSpec {
                    base_type: BaseType::from_type_specs(sq.iter().filter_map(|f|
                        if let DeclSpecToken::Spec(s) = f { Some(s.clone()) } else { None })
                    )?,
                    qual: sq.iter().fold(TypeQual::default(), |a, q| if let DeclSpecToken::Qual(q) = q { a | *q } else { a }),
                    storage: StorageClass::default(),
                    fn_spec: FunctionSpec::default(),
                })
            };

        // 6.7.3 type qualifiers
        rule type_qual() -> TypeQual
            = "const" __ { TypeQual::CONST }
            / "restrict" __ { TypeQual::RESTRICT }
            / "volatile" __ { TypeQual::VOLATILE };
        rule type_quals() -> TypeQual
            = q:(type_qual() ** ___) { q.iter().fold(TypeQual::default(), |a, q| a | *q) };

        // 6.7.4
        rule function_spec() -> FunctionSpec
            = "inline" __ { FunctionSpec::INLINE };

        // 6.7.5
        rule param_decl() -> Node<ParamDeclaration<'input>>
            = l:position!() ds:declaration_spec() _ d:declarator() r:position!() { Node { node: ParamDeclaration {
                spec: ds,
                decl: MayAbsDeclarator::NonAbs(d),
            }, span: l..r } }
            / l:position!() ds:declaration_spec() _ d:abs_declarator() r:position!() { Node { node: ParamDeclaration {
                spec: ds,
                decl: MayAbsDeclarator::AbsDecl(d),
            }, span: l..r } };

        // 6.7.6
        rule type_name() -> Node<TypeName<'input>>
            = l:position!() node:_type_name() r:position!() { Node { node, span: l..r } };

        rule _type_name_ptr() -> TypeName<'input> = precedence! {
            t:(@) _ "*" _ q:(type_qual() ** ___) { TypeName::Pointer(Box::new(t), q.iter().fold(TypeQual::default(), |a, q| a | *q)) }
            type_sq:spec_qual_list() { TypeName::DeclarationSpec(type_sq) }
        };
        rule _type_name() -> TypeName<'input> = quiet! { precedence! {
            t:(@) _ "[" _ s:assign_expr() _ "]" { TypeName::Array(Box::new(t), Some(Box::new(s))) }
            t:(@) _ "[" _ "*" _ "]" { TypeName::Array(Box::new(t), None) }
            t:(@) _ "(" _ p:(param_decl() ** (_ "," _)) _ ")" { TypeName::Function(Box::new(t), p) }
            t:_type_name_ptr() { t }
            "(" _ t:_type_name_ptr() _ ")" { t }
        } } / expected!("type");

        rule abs_declarator() -> Option<AbsDeclarator<'input>>
            = d:_abs_declarator() { Some(d) }
            / { None };

        rule _abs_declarator() -> AbsDeclarator<'input>
            = "*" d:abs_declarator() { AbsDeclarator::Pointer(d.map(Box::new)) }
            / "[" _ e:assign_expr()? _ "]" _ d:abs_declarator() { AbsDeclarator::Array(d.map(Box::new), TypeQual::default(), e) }
            / "[" _ q:type_quals() ___ e:assign_expr()? _ "]" _ d:abs_declarator() { AbsDeclarator::Array(d.map(Box::new), q, e) }
            / "(" _ p:(param_decl() ** (_ "," _)) _ ")" _ d:abs_declarator() { AbsDeclarator::Function(d.map(Box::new), p) }
            / "(" _ d:_abs_declarator() _ ")" { d };

        // 6.7
        rule declaration() -> Node<Declaration<'input>>
            = s:position!() typ:declaration_spec() _ inits:(decl_item() ** (_ "," _)) _ ";" e:position!() {
                Node { node: Declaration { typ, inits }, span: s..e }
            };
        rule decl_item() -> (Node<Declarator<'input>>, Option<Node<Expr<'input>>>)
            = quiet! { v:declarator() _ "=" _ i:assign_expr() { (v, Some(i)) } }
            / quiet! { v:declarator() { (v, None) } }
            / expected!("declaration item");

        rule declarator() -> Node<Declarator<'input>> = precedence! {
            l:position!() "*" _ d:@ { Node { span: l..d.span.end, node: Declarator::Pointer(Box::new(d)) } }
            --
            d:@ _ "[" _ e:assign_expr()? _ "]" r:position!() { Node { span: d.span.start..r, node: Declarator::Array(Box::new(d), TypeQual::default(), e) } }
            d:@ _ "[" _ q:type_quals() ___ e:assign_expr()? _ "]" r:position!() { Node { span: d.span.start..r, node: Declarator::Array(Box::new(d), q, e) } }
            d:@ _ "(" _ p:(param_decl() ** (_ "," _)) _ ")" r:position!() { Node { span: d.span.start..r, node: Declarator::Function(Box::new(d), p) } }
            --
            d:decl_ident() { d }
            l:position!() "(" _ d:declarator() _ ")" r:position!() { Node { node: d.node, span: l..r } }
        };
        rule decl_ident() -> Node<Declarator<'input>>
            = l:position!() v:ident() r:position!() { Node { node: Declarator::Ident(v), span: l..r } };

        rule decl_spec_token() -> DeclSpecToken<'input>
            = q:type_qual() { DeclSpecToken::Qual(q) }
            / s:type_spec() { DeclSpecToken::Spec(s) }
            / s:storage_class() { DeclSpecToken::StorageClass(s) }
            / f:function_spec() { DeclSpecToken::FunctionSpec(f) };

        rule declaration_spec() -> Node<DeclarationSpec<'input>>
            // NOTE: `qual()+ TypedefName` no work
            = l:position!() sq:(decl_spec_token() ++ (___ !ident())) r:position!() {?
                Ok(Node {
                    node: DeclarationSpec {
                        base_type: BaseType::from_type_specs(sq.iter().filter_map(|f|
                            if let DeclSpecToken::Spec(s) = f { Some(s.clone()) } else { None })
                        )?,
                        qual: sq.iter().fold(TypeQual::default(), |a, q| if let DeclSpecToken::Qual(q) = q { a | *q } else { a }),
                        storage: sq.iter().fold(StorageClass::default(), |a, q| if let DeclSpecToken::StorageClass(q) = q { a | *q } else { a }),
                        fn_spec: sq.iter().fold(FunctionSpec::default(), |a, q| if let DeclSpecToken::FunctionSpec(q) = q { a | *q } else { a }),
                    },
                    span: l..r,
                })
            };

        // 6.8 statements
        rule statement() -> Node<Statement<'input>>
            = l:position!() i:ident() _ ":" _ s:statement() r:position!() { Node { node: Statement::Label(i, Box::new(s)), span: l..r } }
            / l:position!() "case" ___ e:expr() _ ":" _ s:statement() r:position!() { Node { node: Statement::Case(Some(e), Box::new(s)), span: l..r } }
            / l:position!() "default" ___ ":" _ s:statement() r:position!() { Node { node: Statement::Case(None, Box::new(s)), span: l..r } }

            / l:position!() comp:compound_stmt() r:position!() { Node { node: Statement::Compound(comp), span: l..r } }

            / expr:expr() ";" r:position!() { Node { span: expr.span.start..r, node: Statement::Expression(expr.node) } }
            / l:position!() ";" r:position!() { Node { node: Statement::Empty, span: l..r } }

            / l:position!() "if" ___ "(" _ e:expr() _ ")" _ a:statement() _ "else" ___ b:statement() r:position!() { Node { node: Statement::IfElse(e, Box::new(a), Some(Box::new(b))), span: l..r } }
            / l:position!() "if" ___ "(" _ e:expr() _ ")" _ a:statement() r:position!() { Node { node: Statement::IfElse(e, Box::new(a), None), span: l..r } }
            / l:position!() "select" ___ "(" _ e:expr() _ ")" _ a:statement() r:position!() { Node { node: Statement::Select(e, Box::new(a)), span: l..r } }

            / l:position!() "while" ___ "(" _ e:expr() _ ")" _ b:statement() r:position!() { Node { node: Statement::While(e, Box::new(b)), span: l..r } }
            / l:position!() "do" ___ b:statement() _ "while" ___ "(" _ e:expr() _ ")" r:position!() { Node { node: Statement::DoWhile(e, Box::new(b)), span: l..r } }
            / l:position!() "for" ___ "(" _ e:((expr()?) **<3, 3> (_ ";" _)) _ ")" _ b:statement() r:position!() { Node { node: Statement::For3E(e, Box::new(b)), span: l..r } }
            / l:position!() "for" ___ "(" _ d:declaration() e:((expr()?) **<2, 2> (_ ";" _)) _ ")" _ b:statement() r:position!() { Node { node: Statement::For2E(d, e, Box::new(b)), span: l..r } }

            / l:position!() "goto" ___ i:ident() _ ";" r:position!() { Node { node: Statement::Goto(i), span: l..r } }
            / l:position!() "continue" _ ";" r:position!() { Node { node: Statement::Continue, span: l..r } }
            / l:position!() "break" _ ";" r:position!() { Node { node: Statement::Break, span: l..r } }
            / l:position!() "return" ___ i:expr()? _ ";" r:position!() { Node { node: Statement::Return(i), span: l..r } };

        rule compound_stmt() -> CompoundStmt<'input>
            = "{" _ i:(block_item() ** _) _ "}" { i };

        rule block_item() -> Node<BlockItem<'input>>
            = d:declaration() { Node { span: d.span, node: BlockItem::Declaration(d.node) } }
            / s:statement() { Node { span: s.span, node: BlockItem::Statement(s.node) } };

        // 6.9 ext defs
        rule external_decl() -> Node<ExternalDecl<'input>>
            = l:position!() decl_spec:declaration_spec() _ declarator:declarator() _ declarations:(declaration() ** _) _ body:compound_stmt() r:position!() {
                Node { node: ExternalDecl::Function(FunctionDef { decl_spec: decl_spec.node, declarator, declarations, body }), span: l..r }
            }
            / d:declaration() { Node { span: d.span, node: ExternalDecl::Declaration(d.node) } };
    }
}
