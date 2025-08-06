// use std::env;
use std::{env, fs};

use crate::parser::parse_tokens;
use crate::runner::State;
use crate::tokenizer::tokenize;

mod parser;
mod runner;
mod syntax_tree;
mod tokenizer;
mod tests;

fn main() {
    let command_line_args: Vec<String> = env::args().collect();

    let file_name = &command_line_args[1];

    // let file_content = fs::read_to_string(command_line_args[1].clone())
    //     .expect("Should have been able to read the file");



    let file_content =
        fs::read_to_string(file_name).expect("Should have been able to read the file");

    let tokens = tokenize(file_content).unwrap();
    // println!("tokens: \n {:?}", tokens);
    let syntax_tree = parse_tokens(&tokens);
    // println!("tree: \n {:?}", syntax_tree);

    let mut state = State::new(syntax_tree);

    println!("returned: {:?}", state.run())
}
