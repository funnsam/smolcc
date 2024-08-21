mod args;
mod compiler;

fn main() {
    let args = <args::Args as clap::Parser>::parse();

    let gcc = std::process::Command::new("gcc")
        .args(["-E", "-std=c99", "-U__GNUC__", "-U__GNUC_MINOR__", "-U__GNUC_PATCHLEVEL__"])
        .args(&args.inputs)
        .output()
        .expect("failed to run gcc preprocessor");
    assert!(gcc.status.success());
    let src = String::from_utf8(gcc.stdout).unwrap();
    println!("gcc done");

    let lines = compiler::lines::find_lines(&src);
    let r = compiler::parser::translation_unit(&src);

    match r {
        Ok(r) => {
            println!("{r:#?}");

            if let Err(e) = compiler::codegen::Codegen::new(&r).compile() {
                for e in e {
                    report((e.0.to_string(), e.1), &src, &lines);
                }
            }
        },
        Err(e) => report((format!("expected {}", e.expected), e.location.offset..e.location.offset + 1), &src, &lines),
    }
}

fn report<'a>(e: (String, compiler::ast::Span), s: &'a str, lines: &[compiler::lines::LineAttr<'a>]) {
    let line_idx = bsearch(e.1.start, lines);
    let line = &lines[line_idx];
    let col = e.1.start - line.start + 1;

    eprintln!("\x1b[1m{line}:{}: \x1b[31merror:\x1b[0m {}", col, e.0);
    for l in line_idx.. {
        let line = &lines[l];
        let nl_byte = lines.get(l + 1).map_or(s.len(), |l| l.start);

        eprintln!("    {} | {}", line.line, s[line.start..nl_byte].trim_end());
        eprintln!("    {0:<1$}  |{2:<3$}\x1b[1;31m{4:^<5$}\x1b[0m", "", (line.line + 1).ilog10() as usize, "", col, "", (e.1.end.min(nl_byte - 1) - e.1.start.max(line.start)).max(1));

        if e.1.end < nl_byte { break; }
    }
}

fn bsearch(byte: usize, lines: &[compiler::lines::LineAttr<'_>]) -> usize {
    let mut l = 0;
    let mut r = lines.len();
    while l < r {
        let m = (l + r) / 2;
        if lines[m].start > byte {
            r = m;
        } else {
            l = m + 1;
        }
    }

    r - 1
}
