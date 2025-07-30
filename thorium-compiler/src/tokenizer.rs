#[derive(Debug)]
pub enum Token {
    Fn,
    Ident(String),
    I32,
    OpenBracket,
    CloseBracket,
    OpenCurlyBrack,
    CloseCurlyBrack,
    Return,
    IntLit(i32),
    Var,
    Equal,
    Colon,
    Add,
    Semi,
    NewLine,
}

pub fn tokanize(content: String) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut content_chars = content.chars().peekable();

    while content_chars.peek().is_some() {
        let peeked = content_chars.peek().unwrap(); 
        let mut buffer = String::new();
        if peeked.is_whitespace() && peeked != &'\n' {
            content_chars.next();
        } else if peeked.is_alphabetic() {
            while content_chars.peek().is_some_and(|char| char.is_alphanumeric()) {
                buffer.push(content_chars.next().unwrap());
            }

            if &buffer == &String::from("fn") {
                tokens.push(Token::Fn);
            } else if &buffer == &String::from("return") {
                tokens.push(Token::Return); 
            } else if &buffer == &String::from("i32") {
                tokens.push(Token::I32);
            } else if &buffer == &String::from("var") {
                tokens.push(Token::Var);
            } else {
                tokens.push(Token::Ident(buffer.clone()));
            }
        } else if !peeked.is_alphanumeric() {
            match peeked {
                '(' => {
                    tokens.push(Token::OpenBracket);
                    content_chars.next();
                },
                ')' => {
                    tokens.push(Token::CloseBracket);
                    content_chars.next();
                },
                '{' => {
                    tokens.push(Token::OpenCurlyBrack);
                    content_chars.next();
                },
                '}' => {
                    tokens.push(Token::CloseCurlyBrack);
                    content_chars.next();
                },
                '=' => {
                    tokens.push(Token::Equal);
                    content_chars.next();
                },
                ':' => {
                    tokens.push(Token::Colon);
                    content_chars.next();
                }
                '+' => {
                    tokens.push(Token::Add);
                    content_chars.next();
                }
                ';' => {
                    tokens.push(Token::Semi);
                    content_chars.next();
                    
                }
                '\n' => {
                    tokens.push(Token::NewLine);
                    content_chars.next();

                }
                _ => panic!("unkown character: {}", peeked)
            }
        } else if peeked.is_ascii_digit() {
            while content_chars.peek().is_some_and(|char| char.is_ascii_digit()) {
                buffer.push(content_chars.next().unwrap());
            }
            tokens.push(Token::IntLit(buffer.parse().unwrap()));
        }
        buffer.clear();
    }
    
    tokens
}

