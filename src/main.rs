mod compiler;

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    let r = compiler::parser::numbers(&src);
    println!("{r:#?}");
}
