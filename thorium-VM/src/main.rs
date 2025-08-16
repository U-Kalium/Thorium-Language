// use std::env;
use std::{env, fs};

// use crate::parser::parse_tokens;
use crate::run_byte_code::{run, MemoryErrors, RuntimeError, SemanticError, StackValue, SyntaxError};
// use crate::runner::State;
use crate::tokenizer::tokenize;

// mod parser;
// mod runner;
// mod syntax_tree;
mod tokenizer;
#[cfg(test)]
mod tests;
mod run_byte_code;

#[derive(Debug)]
pub enum Error {
    MemoryError(MemoryErrors), 
    SyntacError(SyntaxError),
    SemanticError(SemanticError),
    RuntimeError(RuntimeError),
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

    // let file_content = fs::read_to_string(command_line_args[1].clone())
    //     .expect("Should have been able to read the file");



    let file_content =
        fs::read_to_string(file_name).expect("Should have been able to read the file");

    let tokens = tokenize(file_content).unwrap();

    match run(&tokens) {
        Ok(_) => Ok(()),
        Err(err) => Err(err),
    }
    // println!("tokens: \n {:?}", tokens);
    // let syntax_tree = parse_tokens(&tokens);
    // // println!("tree: \n {:?}", syntax_tree);

    // let mut state = State::new(syntax_tree);

    // println!("returned: {:?}", state.run())
}

pub fn tokenize_and_run(byte_code: String) -> Result<Vec<StackValue>, Error> {
    let tokens = tokenize(byte_code).unwrap();
    run(&tokens)
}
