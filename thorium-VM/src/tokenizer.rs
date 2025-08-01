use std::fmt::format;

#[derive(PartialEq)]
#[derive(Debug)]
pub enum Token {
    Func,
    FuncIdent(String),
    I32,
    I64,
    I16,
    I8,
    F32,
    F64,
    Colon,
    FullStop,
    Push,
    Pop,
    Integer(i128),
    Return,
    Export,
    StringLit(String),
    Call,
    EOF,
    Declare,
    Set,
    Get,
    VarIdent(String),
    Add,
    Sub,
    Mul,
    Div,
    Dash,
}
pub fn tokenize(content: String) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut content_chars = content.chars().peekable();

    while content_chars.peek().is_some() {
        let peeked = content_chars.peek().unwrap();
        let mut buffer = String::new();

        if peeked.is_whitespace() {
            content_chars.next();
        } else if peeked.is_alphabetic() {
            while content_chars.peek().is_some_and(|char| char.is_alphanumeric()) {
                buffer.push(content_chars.next().unwrap());
            }

            match buffer.as_str() {
                "func" => {
                    tokens.push(Token::Func);
                }
                "export" => {
                    tokens.push(Token::Export);
                }
                "call" => {
                    tokens.push(Token::Call);
                }
                "return" => {
                    tokens.push(Token::Return);
                }
                "i32" => {
                    tokens.push(Token::I32);
                }
                "i64" => {
                    tokens.push(Token::I64);
                }
                "i16" => {
                    tokens.push(Token::I16);
                }
                "i8" => {
                    tokens.push(Token::I8);
                }
                "f32" => {
                    tokens.push(Token::F32);
                }
                "f64" => {
                    tokens.push(Token::F64);
                }
                "push" => {
                    tokens.push(Token::Push);
                }
                "pop" => {
                    tokens.push(Token::Pop);
                }
                "declare" => {
                    tokens.push(Token::Declare);
                }
                "set" => {
                    tokens.push(Token::Set);
                }
                "get" => {
                    tokens.push(Token::Get);
                }
                "add" => {
                    tokens.push(Token::Add);
                }
                "sub" => {
                    tokens.push(Token::Sub);
                }
                "mul" => {
                    tokens.push(Token::Mul);
                }
                "div" => {
                    tokens.push(Token::Div);
                }   
                unkown => return Err(format!("unkown key word {}", unkown).to_string())
            }
        } else if peeked.is_numeric() {
            while content_chars.peek().is_some_and(|char| char.is_alphanumeric()) {
                buffer.push(content_chars.next().unwrap());
            }
            tokens.push(Token::Integer(buffer.parse().expect("could not parse integer")));
        } else {
            match content_chars.peek().unwrap() {
                ':' => {
                    tokens.push(Token::Colon);
                    content_chars.next();
                }
                '.' => {
                    tokens.push(Token::FullStop);
                    content_chars.next();
                }
                '$' => {
                    content_chars.next();
                    while content_chars.peek().is_some_and(|char| char.is_alphanumeric()) {
                        buffer.push(content_chars.next().unwrap());
                    }

                    tokens.push(Token::FuncIdent(buffer.clone()));
                }
                '%' => {
                    content_chars.next();
                    while content_chars.peek().is_some_and(|char| char.is_alphanumeric()) {
                        buffer.push(content_chars.next().unwrap());
                    }

                    tokens.push(Token::VarIdent(buffer.clone()));
                }
                '"' => {
                    content_chars.next();
                    while content_chars.peek().is_some_and(|char| *char != '"') {
                        buffer.push(content_chars.next().unwrap());
                    }
                    content_chars.next();

                    tokens.push(Token::StringLit(buffer.clone()));
                }
                '-' => {
                    tokens.push(Token::Dash);
                    content_chars.next();
                }
                unkown => return Err(format!("unkown punctuation word {}", unkown).to_string())
            }
        }
        buffer.clear();
    }
    tokens.push(Token::EOF);

    Ok(tokens)
}