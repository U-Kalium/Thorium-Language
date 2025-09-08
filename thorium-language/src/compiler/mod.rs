use crate::compiler::{parser::parse, tokenizer::{tokanize, TokenIter}};

mod parser;
mod tokenizer;

pub fn compile(content: &str) -> String {
    let tokens = tokanize(content.to_string());
    let byte_code = parse(&mut TokenIter::new(tokens));
    byte_code
}