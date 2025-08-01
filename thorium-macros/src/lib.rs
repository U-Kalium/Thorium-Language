extern crate proc_macro;
use std::str::FromStr;

use proc_macro::{Ident, Literal, TokenStream, TokenTree};
use quote::{quote, ToTokens};


#[proc_macro_attribute]
pub fn tokenize_words(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut tokens = TokenStream::new();
    tokens.extend(item.clone());
    let mut words_to_tokanize: Vec<(String, String)> = Vec::new();


    for token in item {
        if let TokenTree::Group(group) = token {
            words_to_tokanize = group.stream().into_iter().filter_map(|token| {
                if let TokenTree::Ident(ident) = token {
                    let literal = ident.to_string().to_lowercase();
                    // syntax_of_words.push(literal);
                    Some((ident.to_string(), literal))
                } else {
                    None
                }
            }).collect()
        }
    }
    let function_beginning = format!("
pub fn tokenize_word(tokens: &mut Vec<Token>, buffer: &mut String) -> Result<(), String> {{
    match buffer.as_str() {{
    ");

    let mut match_body = String::new();
    for (varient, name) in words_to_tokanize {
        match_body.push_str(
            format!("\"{name}\" => {{
                tokens.push(Token::Word(WordToken::{varient}));
            }}
            ").as_str()
        );
    }
    let function_end = format!("
            unkown => return Err(format!(\"unkown key word {{}}\", unkown).to_string())
}}
    Ok(())
}}
");

    let final_function = format!("{function_beginning}{match_body}{function_end}");

    tokens.extend(TokenStream::from_str(&final_function).unwrap());
    tokens
}