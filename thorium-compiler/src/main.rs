use std::{env, fs, process::Command};

use crate::{parser::parse, tokenizer::{tokanize, TokenIter}};

mod parser;
mod tokenizer;
// mod wasm_compile;

fn main() {
    let command_line_args: Vec<String> = env::args().collect();
    // let file_name = command_line_args[1].clone();
    let file_name = "examples/arrays.th".to_string();

    let file_content =
        fs::read_to_string(file_name.clone()).expect("Should have been able to read the file");

    let tokens = tokanize(file_content);

    // println!("tokens:\n{:?}", tokens);

    let byte_code = parse(&mut TokenIter::new(tokens));
    let mut byte_code_filename = file_name.clone();
    byte_code_filename.push('b');

    // let wat_content = gen_wasm(syntax_tree);

    fs::write(byte_code_filename, byte_code).unwrap();

    // Command::new("wasmtime")
    //         .arg("test.wat")
    //         .spawn().unwrap();
}
