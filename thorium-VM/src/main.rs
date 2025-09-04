// use std::env;
use std::{env, fs};

use hashbrown::HashMap;

// use crate::parser::parse_tokens;
use crate::run_byte_code::{run, MemoryErrors, RuntimeError, SemanticError, StackValue, SyntaxError};
// use crate::runner::State;
use crate::tokenizer::tokenize;

// mod parser;
// mod runner;
// mod syntax_tree;
mod tokenizer;
// #[cfg(test)]
// mod tests;
mod run_byte_code;
mod cranelift_jit;
mod dynasm_jit;

#[derive(Debug)]
pub enum Error {
    MemoryError(MemoryErrors), 
    SyntacError(SyntaxError),
    SemanticError(SemanticError),
    RuntimeError(RuntimeError),
}

pub enum RunOptions {
    CraneliftJit,
    DynasmJit,
    Interpret
}

impl From<SyntaxError> for Error {
    fn from(value: SyntaxError) -> Self {
        Self::SyntacError(value)
    }
}
impl From<MemoryErrors> for Error {
    fn from(value: MemoryErrors) -> Self {
        Self::MemoryError(value)
    }
}
impl From<SemanticError> for Error {
    fn from(value: SemanticError) -> Self {
        Self::SemanticError(value)
    }
}
impl From<RuntimeError> for Error {
    fn from(value: RuntimeError) -> Self {
        Self::RuntimeError(value)
    }
}


fn main() -> Result<(), Error>{
    let command_line_args: Vec<String> = env::args().collect();

    let file_name = &command_line_args[1];
    // let file_name = "examples/return.thb";
    let file_content =
        fs::read_to_string(file_name).expect("Should have been able to read the file");

    // let mut tokens = tokenize(file_content).unwrap();

    // match run(&mut tokens) {
    //     Ok(_) => Ok(()),
    //     Err(err) => Err(err),
    // }
    tokenize_and_run(file_content, RunOptions::DynasmJit)
    // println!("tokens: \n {:?}", tokens);
    // let syntax_tree = parse_tokens(&tokens);
    // // println!("tree: \n {:?}", syntax_tree);

    // let mut state = State::new(syntax_tree);

    // println!("returned: {:?}", state.run())
}

pub fn tokenize_and_run(byte_code: String, run_option: RunOptions) -> Result<(),Error> {
    let mut tokens = tokenize(byte_code).unwrap();
    match run_option {
        RunOptions::CraneliftJit => unsafe { cranelift_jit::run(&mut tokens); },
        RunOptions::Interpret => {run_byte_code::run(&mut tokens);},
        RunOptions::DynasmJit => {dynasm_jit::run(&mut tokens);}
    };
    Ok(())
}