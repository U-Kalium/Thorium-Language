use std::{env, fs, process::Command};

use crate::{parser::parse, tokenizer::tokanize, wasm_compile::gen_wasm};

mod tokenizer;
mod parser;
mod wasm_compile;

fn main() {
    let command_line_args: Vec<String> = env::args().collect();

    let file_content = fs::read_to_string(command_line_args[1].clone())
        .expect("Should have been able to read the file");

    let tokens = tokanize(file_content);

    println!("tokens:\n{:?}", tokens);

    let syntax_tree = parse(tokens);

    let wat_content = gen_wasm(syntax_tree);

    fs::write("test.wat", wat_content).unwrap();
    
    Command::new("wasmtime")
            .arg("test.wat")
            .spawn().unwrap();
}
