extern crate proc_macro;
use std::str::FromStr;

use proc_macro::{TokenStream, TokenTree};
use proc_macro2::Literal;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::TokenTree as TokenTree2;

use quote::{ToTokens, quote};
use syn::Ident;
use syn::{DeriveInput, LitChar, LitStr, Variant, parse_macro_input};

#[proc_macro_attribute]
pub fn tokenize_words(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut tokens = TokenStream::new();
    tokens.extend(item.clone());
    let mut words_to_tokanize: Vec<(String, String)> = Vec::new();

    for token in item {
        if let TokenTree::Group(group) = token {
            words_to_tokanize = group
                .stream()
                .into_iter()
                .filter_map(|token| {
                    if let TokenTree::Ident(ident) = token {
                        let literal = ident.to_string().to_lowercase();
                        // syntax_of_words.push(literal);
                        Some((ident.to_string(), literal))
                    } else {
                        None
                    }
                })
                .collect()
        }
    }
    let function_beginning = format!(
        "
pub fn tokenize_word(tokens: &mut Vec<Token>, buffer: &mut String, file_line: u32, column: u32) -> Result<(), String> {{
    match buffer.as_str() {{
    "
    );

    let mut match_body = String::new();
    for (varient, name) in words_to_tokanize {
        match_body.push_str(
            format!(
                "\"{name}\" => {{
                tokens.push(Token {{
                    token_type: TokenType::Word(WordToken::{varient}),
                    line: file_line,
                    column: column - buffer.len() as u32
                }});
            }}
            "
            )
            .as_str(),
        );
    }
    let function_end = format!(
        "
            unkown => return Err(format!(\"unkown key word {{}}\", unkown).to_string())
}}
    Ok(())
}}
"
    );

    let final_function = format!("{function_beginning}{match_body}{function_end}");

    tokens.extend(TokenStream::from_str(&final_function).unwrap());
    tokens
}

#[derive(Debug)]
enum TokenBehaviour {
    Ident,
    NumberLit,
    EOF,
}
#[derive(Debug)]
struct TokensInfo {
    symbolised: Vec<(Ident, TokenTree2)>,
    plain: Vec<(Ident, TokenTree2)>,
}

#[proc_macro_derive(Tokenize, attributes(symbol))]
pub fn derive_tokenize(item: TokenStream) -> TokenStream {
    dbg!("hello");
    let tokens = item.clone();
    let ast = parse_macro_input!(tokens as DeriveInput);
    let enum_: syn::DataEnum = match ast.data {
        syn::Data::Enum(data) => data,
        _ => panic!("Failed to parse HelperAttr, didn't parse enum"),
    };
    let mut symbolised = Vec::new();
    let mut plain = Vec::new();
    for variant in enum_.variants.iter() {
        if variant.attrs.iter().len() > 1 {
            panic!("Expected only one attribute per token")
        }
        if let Some(attr) = variant.attrs.iter().next() {
            if attr.path().is_ident("symbol") {
            } else {
                panic!("Expected path to be symbol")
            }

            let char_lit: LitStr = attr.parse_args().unwrap();
            symbolised.push((
                variant.ident.clone(),
                TokenTree2::Literal(Literal::string(&char_lit.value().to_string())),
            ));
        } else {
            match variant.ident.to_string().as_str() {
                "Ident" => {}
                "NumberLit" => {}
                "Eof" => {}
                "NewLine" => {}
                otherwise => {
                    plain.push((
                        variant.ident.clone(),
                        TokenTree2::Literal(Literal::string(&otherwise.to_lowercase())),
                    ));
                }
            }
        }
    }
    let tokens_info = TokensInfo { symbolised, plain };
    create_tokenize_fn(tokens_info)
}

fn create_tokenize_fn(info: TokensInfo) -> TokenStream {
    let (plains_variant, plains_str_lit): (Vec<Ident>, Vec<TokenTree2>) =
        info.plain.clone().into_iter().unzip();
    let (symbol_variant, symbol_str_lit): (Vec<Ident>, Vec<TokenTree2>) =
        info.symbolised.clone().into_iter().unzip();
    quote! {
        pub fn tokenize(content: String) -> Vec<Token> {
            let mut tokens = Vec::new();
            let mut content_chars = content.chars().peekable();
            let mut file_offset = 0;

            while content_chars.peek().is_some() {
                let peeked = content_chars.peek().unwrap();
                let mut buffer = String::new();
                if peeked.is_whitespace() && peeked != &'\n' {
                    content_chars.next();
                    file_offset += 1;
                } else if peeked.is_alphabetic() {
                    while content_chars
                        .peek()
                        .is_some_and(|char| char.is_alphanumeric() || *char == '_')
                    {
                        buffer.push(content_chars.next().unwrap());
                        file_offset += 1
                    }
                    match buffer.as_str() {
                        #(#plains_str_lit => {
                            tokens.push(Token {
                                token_type: TokenType::#plains_variant,
                                file_offset: file_offset
                            });
                        }),*
                        otherwise => {
                            tokens.push(Token {
                                token_type: TokenType::Ident(buffer.clone()),
                                file_offset: file_offset
                            });
                        }
                    }
                } else if !peeked.is_alphanumeric() {
                    while content_chars
                        .peek()
                        .is_some_and(|char| char.is_ascii_punctuation())
                    {
                        buffer.push(content_chars.next().unwrap());
                        file_offset += 1
                    }
                    match buffer.as_str() {
                        #(#symbol_str_lit => {
                            tokens.push(Token {
                                token_type: TokenType::#symbol_variant,
                                file_offset: file_offset
                            });
                        }),*
                        otherwise => panic!("unknown symbols: {}", otherwise)
                    }
                } else if peeked.is_ascii_digit() {
                    while  content_chars
                        .peek()
                        .is_some_and(|char| char.is_ascii_digit())
                    {
                        buffer.push(content_chars.next().unwrap());
                        file_offset += 1
                    }
                    if *content_chars.peek().unwrap() == '.' {
                        buffer.push(content_chars.next().unwrap());
                        file_offset += 1;
                        while content_chars
                            .peek()
                            .is_some_and(|char| char.is_ascii_digit())
                        {
                            buffer.push(content_chars.next().unwrap());
                            file_offset += 1
                        };
                        tokens.push(Token {
                            token_type: TokenType::NumberLit(buffer.clone()),
                            file_offset: file_offset
                        });
                    } else {
                        tokens.push(Token {
                            token_type: TokenType::NumberLit(buffer.clone()),
                            file_offset: file_offset
                        });
                    }
                } else if *peeked == '\n' {
                    content_chars.next();
                    tokens.push(Token {
                        token_type: TokenType::NewLine,
                        file_offset: file_offset
                    });
                };
buffer.clear();
            }
            

            
        tokens.push(Token {
            token_type: TokenType::Eof,
            file_offset: file_offset
        });

        tokens
        }
    }
    .into()
}

#[proc_macro_attribute]
pub fn symbol(attr: TokenStream, item: TokenStream) -> TokenStream {
    let attr: TokenStream2 = attr.into();
    dbg!("hello");
    dbg!(attr);
    let item: TokenStream2 = item.into();
    item.into()
}
