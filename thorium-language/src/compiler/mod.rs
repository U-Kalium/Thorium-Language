use crate::compiler::{parser::parse, tokenizer::{tokanize, TokenIter}};

mod parser;
mod tokenizer;
mod syntax_tree;
mod code_gen;

pub fn compile(content: &str) -> String {
    let tokens = tokanize(content.to_string());
    let program = parse(&mut TokenIter::new(tokens));
    program.byte_code()
}