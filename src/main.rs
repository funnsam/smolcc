mod compiler;

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    let r = compiler::parser::numbers(&src);

    match r {
        Ok(r) => println!("{r:#?}"),
        Err(e) => report(e, &src),
    }
}

fn report(e: peg::error::ParseError<peg::str::LineCol>, s: &str) {
    eprintln!("\x1b[1;31merror:\x1b[0m expected {}", e.expected);
    eprintln!("    {} | {}", e.location.line, s.split('\n').nth(e.location.line - 1).unwrap());
    eprintln!("    {0:<1$}  |{2:<3$}\x1b[1;31m^\x1b[0m", "", (e.location.line + 1).ilog10() as usize, "", e.location.column);
}
