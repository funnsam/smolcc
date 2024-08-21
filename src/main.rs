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
        Ok(r) => println!("{r:#?}"),
        Err(e) => report(e, &src, &lines),
    }
}

fn report<'a>(e: peg::error::ParseError<peg::str::LineCol>, s: &'a str, lines: &[compiler::lines::LineAttr<'a>]) {
    let line = &lines[e.location.line - 1];
    let nl_byte = lines.get(e.location.line).map_or(s.len(), |l| l.start);

    eprintln!("\x1b[1m{line}:{}: \x1b[31merror:\x1b[0m expected {}", e.location.column, e.expected);
    eprintln!("    {} | {}", line.line, s[line.start..nl_byte].trim_end());
    eprintln!("    {0:<1$}  |{2:<3$}\x1b[1;31m^\x1b[0m", "", (line.line + 1).ilog10() as usize, "", e.location.column);
}
