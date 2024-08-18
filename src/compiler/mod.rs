pub mod ast;

peg::parser! {
    pub grammar parser() for str {
        use super::ast::*;

        // temp testing thing
        pub rule numbers() -> Vec<Node<Declaration<'input>>>
            = l:(declaration()*) "\n"? { l }

        // utilities
        rule hex_digits()
            = ['0'..='9' | 'a'..='f' | 'A'..='F']+ {};

        rule _
            = quiet!{[' ' | '\n' | '\t']* ("/*" b_comment_c()* "*/" _)? ("//" [^ '\n']* _)? }
        rule wsm()
            = quiet!{[' ' | '\n' | '\t']+ ("/*" b_comment_c()* "*/" _)? ("//" [^ '\n']* _)? }
        rule b_comment_c()
            = [^ '*']
            / "*" !"/";

        rule ident() -> &'input str
            = quiet!{ _ i:$(['_' | 'a'..='z' | 'A'..='Z']['_' | '0'..='9' | 'a'..='z' | 'A'..='Z']*) _ {?
                (!matches!(i, "auto" | "break" | "case" | "char" | "const" | "continue" | "default" | "do" | "double" | "else" | "enum" | "extern" | "float" | "for" | "goto" | "if" | "inline" | "int" | "long" | "register" | "restrict" | "return" | "short" | "signed" | "sizeof" | "static" | "struct" | "switch" | "typedef" | "union" | "unsigned" | "void" | "volatile" | "while" | "_Bool" | "_Complex" | "_Imaginary")).then_some(i).ok_or("identifier can't be a keyword")
            } }
            / expected!("identifier");

        // 6.4.4.1 integer constant
        rule int_const() -> IntConst
            = quiet!{ _ value:dec_const() suffix:int_suffix() _ { IntConst { value, suffix, infer_unsigned: false } } }
            / quiet!{ _ value:dec_const() _ { IntConst { value, suffix: IntSuffix::default(), infer_unsigned: false } } }
            / quiet!{ _ value:oct_or_hex_const() suffix:int_suffix() _ { IntConst { value, suffix, infer_unsigned: true } } }
            / quiet!{ _ value:oct_or_hex_const() _ { IntConst { value, suffix: IntSuffix::default(), infer_unsigned: true } } }
            / expected!("integer constant");

        rule dec_const() -> u64
            = n:$(['1'..='9']['0'..='9']*) {? n.parse().or(Err("value too big")) };
        rule oct_or_hex_const() -> u64
            = "0" n:$(['0'..='7']*) {? u64::from_str_radix(n, 8).or(Err("value too big")) }
            / "0x" n:$(hex_digits()) {? u64::from_str_radix(n, 16).or(Err("value too big")) }
            / "0" { 0 };

        rule ll() = "ll" / "LL";
        rule int_suffix() -> IntSuffix
            = ['u' | 'U']ll() { IntSuffix::ULL }
            / ['u' | 'U']['l' | 'L'] { IntSuffix::UL }
            / ['u' | 'U'] { IntSuffix::U }
            / ll()['u' | 'U'] { IntSuffix::ULL }
            / ll() { IntSuffix::LL }
            / ['l' | 'L']['u' | 'U'] { IntSuffix::UL }
            / ['l' | 'L'] { IntSuffix::L };

        // 6.4.4.4 character constant
        rule char_const() -> CharConst
            = quiet!{ _ "'" ch:c_char() "'" _ { CharConst { ch, wide: false } } }
            / quiet!{ _ "L'" ch:c_char() "'" _ { CharConst { ch, wide: true } } }
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
            / r"\" n:$(['0'..='7']*<1,3>) { u32::from_str_radix(n, 8).unwrap() }
            / r"\x" n:$(hex_digits()) {? u32::from_str_radix(n, 16).or(Err("value too big")) };

        // 6.4.5 string literal
        rule string_lit() -> StringLit
            = quiet!{ _ "\"" string:s_char()* "\"" _ { StringLit { string, wide: false } } }
            / quiet!{ _ "L\"" string:s_char()* "\"" _ { StringLit { string, wide: true } } }
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
            = quiet!{ e:(assign_expr() ++ (_ "," _)) { if e.len() == 1 {
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
            l:position!() "sizeof" wsm() x:(@) { Node { span: l..x.span.end, node: Expr::SizeOfE(Box::new(x)) } }
            l:position!() "sizeof" _ "(" _ t:type_name() _ ")" r:position!() { Node { span: l..r, node: Expr::SizeOfT(t) } }
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

        // 6.7.2 type specifiers
        rule type_spec() -> TypeSpec<'input>
            = "void" { TypeSpec::Void }
            / "char" { TypeSpec::Char }
            / "short" { TypeSpec::Short }
            / "int" { TypeSpec::Int }
            / "long" { TypeSpec::Long }
            / "float" { TypeSpec::Float }
            / "double" { TypeSpec::Double }
            / "signed" { TypeSpec::Signed }
            / "unsigned" { TypeSpec::Unsigned }
            / "struct" i:ident() { TypeSpec::Struct(i) }
            / "union" i:ident() { TypeSpec::Union(i) }
            / "enum" i:ident() { TypeSpec::Enum(i) }
            / i:ident() { TypeSpec::TypedefName(i) };

        rule type_spec_or_qual() -> TypeSpecOrQual<'input>
            = q:type_qual() { TypeSpecOrQual::Qual(q) }
            / s:type_spec() { TypeSpecOrQual::Spec(s) };
        rule spec_qual_list() -> TypeSpecQual<'input>
            = sq:(type_spec_or_qual() ++ (wsm() !ident())) {?
                Ok(TypeSpecQual {
                    base_type: BaseType::from_type_specs(sq.iter().filter_map(|f|
                        if let TypeSpecOrQual::Spec(s) = f { Some(*s) } else { None })
                    )?,
                    qual: sq.iter().fold(TypeQual::default(), |a, q| if let TypeSpecOrQual::Qual(q) = q {
                        a | *q
                    } else {
                        a
                    }),
                })
            };

        // 6.7.3 type qualifiers
        rule type_qual() -> TypeQual
            = "const" { TypeQual::CONST }
            / "restrict" { TypeQual::RESTRICT }
            / "volatile" { TypeQual::VOLATILE };

        // 6.7.6 type names
        // TODO: abstract-decl
        rule type_name() -> Node<TypeName<'input>>
            = quiet!{ l:position!() type_sq:spec_qual_list() r:position!() { Node { node: TypeName { type_sq }, span: l..r } } }
            / expected!("type");

        // 6.7
        rule declaration() -> Node<Declaration<'input>>
            = s:position!() typ:type_name() _ inits:(decl_item() ** (_ "," _)) _ ";" e:position!() {
                Node { node: Declaration { typ, inits }, span: s..e }
            };
        rule decl_item() -> (&'input str, Option<Node<Expr<'input>>>)
            = quiet!{ v:ident() _ "=" _ i:assign_expr() { (v, Some(i)) } }
            / quiet!{ v:ident() { (v, None) } };
    }
}
