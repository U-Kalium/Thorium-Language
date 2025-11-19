use thiserror::Error;

use crate::compiler::{ tokenizer::{tokenize, TokenIter}};

mod parser;
mod tokenizer;
mod syntax_tree;
mod code_gen;
mod expression_tree;

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error(transparent)]
    ParserError(#[from] parser::ParserError),
    #[error(transparent)]
    TokenError(#[from] tokenizer::TokenError),
}

pub fn compile(content: &str) -> String {
    // let tokens = tokanize(content.to_string());
    // let program = parse(&mut TokenIter::new(tokens));
    // program.byte_code()
    todo!()
}