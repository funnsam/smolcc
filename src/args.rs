use clap::*;

#[derive(Parser, Debug, Clone)]
pub struct Args {
    pub inputs: Vec<String>,
}
