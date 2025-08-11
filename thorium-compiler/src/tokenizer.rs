use std::fs::File;

#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
    Fn,
    Ident(String),
    I32,
    OpenBracket,
    CloseBracket,
    OpenCurlyBracket,
    CloseCurlyBracket,
    OpenAngleBracket,
    CloseAngleBracket,
    Return,
    IntLit(i32),
    Var,
    Let,
    Equal,
    Colon,
    Add,
    SemiColon,
    NewLine,
}
#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub line: u32,
    pub column: u32,
}

pub fn tokanize(content: String) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut content_chars = content.chars().peekable();
    let mut file_line = 1;
    let mut column = 1;

    while content_chars.peek().is_some() {
        let peeked = content_chars.peek().unwrap(); 
        let mut buffer = String::new();
        if peeked.is_whitespace() && peeked != &'\n' {
            content_chars.next();
            column += 1
        } else if peeked.is_alphabetic() {
            while content_chars.peek().is_some_and(|char| char.is_alphanumeric()) {
                buffer.push(content_chars.next().unwrap());
                column += 1
            }

            if &buffer == &String::from("fn") {
                tokens.push(Token {
                    token_type: TokenType::Fn,
                    line: file_line,
                    column: column - buffer.len() as u32
                });
            } else if &buffer == &String::from("return") {
                tokens.push(Token {
                    token_type: TokenType::Return,
                    line: file_line,
                    column: column - buffer.len() as u32
                }); 
            } else if &buffer == &String::from("i32") {
                tokens.push(Token {
                    token_type: TokenType::I32,
                    line: file_line,
                    column: column - buffer.len() as u32
                });
            } else if &buffer == &String::from("var") {
                tokens.push(Token {
                    token_type: TokenType::Var,
                    line: file_line,
                    column: column - buffer.len() as u32
                });
            } else if &buffer == &String::from("let") {
                tokens.push(Token {
                    token_type: TokenType::Let,
                    line: file_line,
                    column: column - buffer.len() as u32
                });
            } else {
                tokens.push(Token {
                    token_type: TokenType::Ident(buffer.clone()),
                    line: file_line,
                    column: column - buffer.len() as u32
                });        
            }
            
        } else if !peeked.is_alphanumeric() {
            column += 1;
            match peeked {
                '(' => {
                    tokens.push(Token {
                        token_type: TokenType::OpenBracket,
                        line: file_line,
                        column: column - 1
                    });
                    content_chars.next();
                },
                ')' => {
                    tokens.push(Token {
                        token_type: TokenType::CloseBracket,
                        line: file_line,
                        column: column - 1
                    });
                    content_chars.next();
                },
                '<' => {
                    tokens.push(Token {
                        token_type: TokenType::OpenAngleBracket,
                        line: file_line,
                        column: column - 1
                    });
                    content_chars.next();
                },
                '>' => {
                    tokens.push(Token {
                        token_type: TokenType::CloseAngleBracket,
                        line: file_line,
                        column: column - 1
                    });
                    content_chars.next();
                },
                '{' => {
                    tokens.push(Token {
                        token_type: TokenType::OpenCurlyBracket,
                        line: file_line,
                        column: column - 1
                    });
                    content_chars.next();
                },
                '}' => {
                    tokens.push(Token {
                        token_type: TokenType::CloseCurlyBracket,
                        line: file_line,
                        column: column - 1
                    });
                    content_chars.next();
                },
                '=' => {
                    tokens.push(Token {
                        token_type: TokenType::Equal,
                        line: file_line,
                        column: column - 1
                    });
                    content_chars.next();
                },
                ':' => {
                    tokens.push(Token {
                        token_type: TokenType::Colon,
                        line: file_line,
                        column: column - 1
                    });
                    content_chars.next();
                }
                '+' => {
                    tokens.push(Token {
                        token_type: TokenType::Add,
                        line: file_line,
                        column: column - 1
                    });
                    content_chars.next();
                }
                ';' => {
                    tokens.push(Token {
                        token_type: TokenType::SemiColon,
                        line: file_line,
                        column: column - 1
                    });
                    content_chars.next();
                    
                }
                '\n' => {
                    tokens.push(Token {
                        token_type: TokenType::NewLine,
                        line: file_line,
                        column: column - 1
                    });
                    content_chars.next();
                    file_line += 1;
                    column = 1;
                }
                _ => panic!("unkown character: {}", peeked)
            }
        } else if peeked.is_ascii_digit() {
            while content_chars.peek().is_some_and(|char| char.is_ascii_digit()) {
                buffer.push(content_chars.next().unwrap());
                column += 1
            }
            tokens.push(Token {
                token_type: TokenType::IntLit(buffer.parse().unwrap()),
                line: file_line,
                column: column - buffer.len() as u32
            });
        }
        buffer.clear();
    }
    
    tokens
}
