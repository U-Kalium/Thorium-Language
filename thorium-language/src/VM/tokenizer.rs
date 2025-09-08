
// extern crate thorium_macros;
use thorium_macros::tokenize_words;

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: u32,
    pub column: u32,
}

#[derive(PartialEq, Debug, Clone)]
pub enum TokenType {
    Word(WordToken),
    FuncIdent(String),
    LabelIdent(String),
    TokenIndex(usize),
    Colon,
    FullStop,
    Number(i128),
    StringLit(String),
    CharLit(char),
    EOF,
    VarIdent(String),
    Minus,
    Comma,
    Plus,
}

#[derive(PartialEq, Debug, Clone)]
#[tokenize_words]
pub enum WordToken {
    Func,
    EndFunc,
    I32,
    I64,
    I128,
    I16,
    I8,
    F32,
    F64,
    Push,
    Pop,
    Return,
    Export,
    Call,
    Declare,
    Set,
    Get,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Xor,
    Eq,
    Neq,
    Gte,
    Gt,
    Lte,
    Lt,
    Max,
    Min,
    Jmp,
    Jpz,
    Cast,
    Insert,
    Grow,
    Remove,
    Mem,
    Cpy,
    Stack,
    Top,
}

pub fn tokenize(content: String) -> Result<Vec<Token>, String> {
    let mut tokens: Vec<Token> = Vec::new();
    let binding = content.to_lowercase();
    let mut content_chars = binding.chars().peekable();
    let mut file_line = 1;
    let mut column = 1;

    while content_chars.peek().is_some() {
        let peeked = content_chars.peek().unwrap();
        let mut buffer = String::new();

        if peeked.is_whitespace() {
            if peeked.clone() == '\n' {
                file_line += 1;
                column = 1;
            }
            content_chars.next();
            column += 1;
        } else if peeked.is_alphabetic() {
            while content_chars
                .peek()
                .is_some_and(|char| char.is_alphanumeric())
            {
                buffer.push(content_chars.next().unwrap());
                column += 1;
            }

            if let Err(error) = tokenize_word(&mut tokens, &mut buffer, file_line, column) {
                return Err(error);
            }
        } else if peeked.is_numeric() {
            while content_chars
                .peek()
                .is_some_and(|char| char.is_alphanumeric())
            {
                buffer.push(content_chars.next().unwrap());
                column += 1;
            }
                // dbg!(&buffer);
            tokens.push(Token {
                token_type: TokenType::Number(buffer.parse().expect("could not parse integer")),
                line: file_line,
                column: column - buffer.len() as u32
            });
        } else {
            column += 1;
            match content_chars.peek().unwrap() {
                ':' => {
                    tokens.push(Token {
                        token_type: TokenType::Colon,
                        line: file_line,
                        column: column - 1
                    });
                    content_chars.next();
                }
                '.' => {
                    tokens.push(Token {
                        token_type: TokenType::FullStop,
                        line: file_line,
                        column: column - 1
                    });
                    content_chars.next(); 
                }
                '$' => {
                    content_chars.next();
                    while content_chars
                        .peek()
                        .is_some_and(|char| char.is_alphanumeric())
                    {
                        buffer.push(content_chars.next().unwrap());
                        column += 1;
                    }

                    tokens.push(Token {
                        token_type: TokenType::FuncIdent(buffer.clone()),
                        line: file_line,
                        column: column - buffer.len() as u32
                    });
                }
                '@' => {
                    content_chars.next(); 
                    while content_chars
                        .peek()
                        .is_some_and(|char| char.is_alphanumeric() || *char == '_')
                    {
                        buffer.push(content_chars.next().unwrap());
                        column += 1;

                    }
                    tokens.push(Token {
                        token_type: TokenType::LabelIdent(buffer.clone()),
                        line: file_line,
                        column: column - buffer.len() as u32
                    });
                }
                '%' => {
                    content_chars.next();
                    while content_chars
                        .peek()
                        .is_some_and(|char| char.is_alphanumeric() || *char == '_')
                    {
                        buffer.push(content_chars.next().unwrap());
                        column += 1;

                    }

                    tokens.push(Token {
                        token_type: TokenType::VarIdent(buffer.clone()),
                        line: file_line,
                        column: column - buffer.len() as u32
                    });
                }
                '"' => {
                    content_chars.next();
                    while content_chars.peek().is_some_and(|char| *char != '"') {
                        buffer.push(content_chars.next().unwrap());
                        column += 1;

                    }
                    content_chars.next();

                    tokens.push(Token {
                        token_type: TokenType::StringLit(buffer.clone()),
                        line: file_line,
                        column: column - buffer.len() as u32 - 1
                    });
                }
                '\'' => {
                    content_chars.next();
                    while content_chars.peek().is_some_and(|char| *char != '\'') {
                        buffer.push(content_chars.next().unwrap());
                        column += 1;
                    }
                    content_chars.next();

                    tokens.push(Token {
                        token_type: TokenType::CharLit(buffer.parse().unwrap()),
                        line: file_line,
                        column: column - buffer.len() as u32 -1
                    });
                }
                '-' => {
                    tokens.push(Token {
                        token_type: TokenType::Minus,
                        line: file_line,
                        column: column - 1
                    });
                    content_chars.next();
                }
                ',' => {
                    tokens.push(Token {
                        token_type: TokenType::Comma,
                        line: file_line,
                        column: column - 1
                    });
                    content_chars.next();
                }
                '+' => {
                    tokens.push(Token {
                        token_type: TokenType::Plus,
                        line: file_line,
                        column: column - 1
                    });
                    content_chars.next();
                }
                // detecting comments
                '/' => {
                    content_chars.next();
                    if let Some(char) = content_chars.peek() {
                        if *char == '/' {
                            content_chars.next();
                            while content_chars.peek().is_some_and(|char| *char != '\n') {
                                content_chars.next();
                                column += 1;
                            }
                        } else {
                            panic!("syntax error expected / for a comment but found {char}");
                        }
                    }
                }
                unkown => return Err(format!("unkown punctuation word {}", unkown).to_string()),
            }
        }
        buffer.clear();
    }
    tokens.push(Token {
        token_type: TokenType::EOF,
        line: file_line,
        column: column
    });

    Ok(tokens)
}


// struct Tokenizer {

// }