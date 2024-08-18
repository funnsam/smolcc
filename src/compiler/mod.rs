pub mod ast;

peg::parser! {
    pub grammar parser() for str {
        use super::ast::*;

        // temp testing thing
        pub rule numbers() -> Vec<Node<Expr<'input>>>
            = l:(expr() ** "$") "\n"? { l }

        // utilities
        rule hex_digits()
            = ['0'..='9' | 'a'..='f' | 'A'..='F']+ {};

        rule ws()
            = quiet!{[' ' | '\n' | '\t']* ("/*" b_comment_c()* "*/" ws())? ("//" [^ '\n']* ws())? }
        rule b_comment_c()
            = [^ '*']
            / "*" !"/";

        rule ident() -> &'input str
            = quiet!{ ws() i:$(['_' | 'a'..='z' | 'A'..='Z']['_' | '0'..='9' | 'a'..='z' | 'A'..='Z']*) ws() { i } }
            / expected!("identifier");

        // 6.4.4.1 integer constant
        rule int_const() -> IntConst
            = quiet!{ ws() value:dec_const() suffix:int_suffix() ws() { IntConst { value, suffix, infer_unsigned: false } } }
            / quiet!{ ws() value:dec_const() ws() { IntConst { value, suffix: IntSuffix::default(), infer_unsigned: false } } }
            / quiet!{ ws() value:oct_or_hex_const() suffix:int_suffix() ws() { IntConst { value, suffix, infer_unsigned: true } } }
            / quiet!{ ws() value:oct_or_hex_const() ws() { IntConst { value, suffix: IntSuffix::default(), infer_unsigned: true } } }
            / expected!("integer constant");

        rule dec_const() -> u64
            = n:$(['1'..='9']['0'..='9']*) {? n.parse().or(Err("value too big")) };
        rule oct_or_hex_const() -> u64
            = "0" n:$(['0'..='7']*) {? u64::from_str_radix(n, 8).or(Err("value too big")) }
            / "0x" n:$(hex_digits()) {? u64::from_str_radix(n, 16).or(Err("value too big")) };

        rule int_suffix() -> IntSuffix
            = ['u' | 'U']"ll" { IntSuffix::ULL }
            / ['u' | 'U']"LL" { IntSuffix::ULL }
            / ['u' | 'U']['l' | 'L'] { IntSuffix::UL }
            / ['u' | 'U'] { IntSuffix::U }
            / "ll"['u' | 'U'] { IntSuffix::ULL }
            / "LL"['u' | 'U'] { IntSuffix::ULL }
            / "ll" { IntSuffix::LL }
            / "LL" { IntSuffix::LL }
            / ['l' | 'L']['u' | 'U'] { IntSuffix::UL }
            / ['l' | 'L'] { IntSuffix::L };

        // 6.4.4.4 character constant
        rule char_const() -> CharConst
            = quiet!{ ws() "'" ch:c_char() "'" ws() { CharConst { ch, wide: false } } }
            / quiet!{ ws() "L'" ch:c_char() "'" ws() { CharConst { ch, wide: true } } }
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
            = quiet!{ ws() "\"" string:s_char()* "\"" ws() { StringLit { string, wide: false } } }
            / quiet!{ ws() "L\"" string:s_char()* "\"" ws() { StringLit { string, wide: true } } }
            / expected!("string literal");

        rule s_char() -> u32
            = ch:$([^ '"' | '\\' | '\n']) { ch.chars().next().unwrap() as u32 }
            / ch:esc_seq() { ch }

        // 6.5 expressions
        rule prim_expr() -> Node<Expr<'input>>
            = ws() s:position!() node:_prim_expr() e:position!() ws() { Node { node, span: s..e } };
        rule _prim_expr() -> Expr<'input>
            = id:ident() { Expr::Ident(id) }
            / ic:int_const() { Expr::IntConst(ic) }
            / cc:char_const() { Expr::CharConst(cc) }
            / sl:string_lit() { Expr::StringLit(sl) }
            / "(" ex: expr() ")" { ex.node };

        rule expr() -> Node<Expr<'input>> = precedence! {
            l:(@) ws() "||" ws() r:@ { Node { span: l.span.start..r.span.end, node: Expr::LcOr(Box::new(l), Box::new(r)) } }
            --
            l:(@) ws() "&&" ws() r:@ { Node { span: l.span.start..r.span.end, node: Expr::LcAnd(Box::new(l), Box::new(r)) } }
            --
            l:(@) ws() "|" ws() r:@ { Node { span: l.span.start..r.span.end, node: Expr::BwOr(Box::new(l), Box::new(r)) } }
            --
            l:(@) ws() "^" ws() r:@ { Node { span: l.span.start..r.span.end, node: Expr::BwXor(Box::new(l), Box::new(r)) } }
            --
            l:(@) ws() "&" ws() r:@ { Node { span: l.span.start..r.span.end, node: Expr::BwAnd(Box::new(l), Box::new(r)) } }
            --
            l:(@) ws() "==" ws() r:@ { Node { span: l.span.start..r.span.end, node: Expr::Eq(Box::new(l), Box::new(r)) } }
            l:(@) ws() "!=" ws() r:@ { Node { span: l.span.start..r.span.end, node: Expr::Ne(Box::new(l), Box::new(r)) } }
            --
            l:(@) ws() "<" ws() r:@ { Node { span: l.span.start..r.span.end, node: Expr::Lt(Box::new(l), Box::new(r)) } }
            l:(@) ws() "<=" ws() r:@ { Node { span: l.span.start..r.span.end, node: Expr::Le(Box::new(l), Box::new(r)) } }
            l:(@) ws() ">" ws() r:@ { Node { span: l.span.start..r.span.end, node: Expr::Gt(Box::new(l), Box::new(r)) } }
            l:(@) ws() ">=" ws() r:@ { Node { span: l.span.start..r.span.end, node: Expr::Ge(Box::new(l), Box::new(r)) } }
            --
            l:(@) ws() "<<" ws() r:@ { Node { span: l.span.start..r.span.end, node: Expr::BwLsh(Box::new(l), Box::new(r)) } }
            l:(@) ws() ">>" ws() r:@ { Node { span: l.span.start..r.span.end, node: Expr::BwRsh(Box::new(l), Box::new(r)) } }
            --
            l:(@) ws() "+" ws() r:@ { Node { span: l.span.start..r.span.end, node: Expr::Add(Box::new(l), Box::new(r)) } }
            l:(@) ws() "-" ws() r:@ { Node { span: l.span.start..r.span.end, node: Expr::Sub(Box::new(l), Box::new(r)) } }
            --
            l:(@) ws() "*" ws() r:@ { Node { span: l.span.start..r.span.end, node: Expr::Mul(Box::new(l), Box::new(r)) } }
            l:(@) ws() "/" ws() r:@ { Node { span: l.span.start..r.span.end, node: Expr::Div(Box::new(l), Box::new(r)) } }
            l:(@) ws() "%" ws() r:@ { Node { span: l.span.start..r.span.end, node: Expr::Mod(Box::new(l), Box::new(r)) } }
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

        // 6.7.3 type qualifiers
        rule type_qual() -> TypeQual
            = "const" { TypeQual::Const }
            / "restrict" { TypeQual::Restrict }
            / "volatile" { TypeQual::Volatile };
    }
}
