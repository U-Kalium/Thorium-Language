
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Ident(String),
    I64,
    I32,
    I16,
    I8,
    F64,
    F32,
    OpenBracket,
    CloseBracket,
    OpenCurlyBracket,
    CloseCurlyBracket,
    OpenAngleBracket,
    CloseAngleBracket,
    OpenSquareBracket,
    CloseSquareBracket,
    Return,
    Finish,
    NumberLit(String),
    Var,
    Let,
    If,
    Else,
    DoubleEqual,
    Bool,
    True,
    False,
    Equal,
    Colon,
    Add,
    Minus,
    Star,
    ForwardSlash,
    SemiColon,
    NewLine,
    Comma,
    Loop,
    LessEqual,
    GreatEqual,
    Eof,
}
#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: u32,
    pub column: u32,
}

impl Token {
    pub fn is_bin_op(&self) -> bool {
        match self.token_type {
            TokenType::Add => true,
            TokenType::Minus => true,
            TokenType::ForwardSlash => true,
            TokenType::Star => true,
            _ => false,
        }
    }
}

pub struct TokenIter {
    tokens: Vec<Token>,
    index: i32,
    just_initialized: bool,
}

impl TokenIter {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            index: 0,
            just_initialized: true,
        }
    }
    pub fn next(&mut self) -> Option<Token> {
        if self.just_initialized {
            self.just_initialized = false;
            self.tokens.get(self.index as usize).cloned()
        } else {
            self.index += 1;
            self.tokens.get(self.index as usize).cloned()
        }
    }
    pub fn next_if(&mut self, condition: impl Fn(&Token) -> bool) -> Option<Token> {
        if condition(&self.tokens[self.index as usize + 1]) {
            self.index += 1;
            self.tokens.get(self.index as usize).cloned()
        } else {
            None
        }
    }
    pub fn current(&mut self) -> Token {
        self.tokens[self.index as usize].clone()
    }
    pub fn reset(&mut self) {
        self.index = 0
    }
    pub fn peek(&mut self) -> Option<Token> {
        if self.just_initialized {
            // self.just_initialized = false;
            self.tokens.get(self.index as usize).cloned()
        } else {
            self.tokens.get(self.index as usize + 1).cloned()
        }
    }
    pub fn back(&mut self) -> Option<Token> {
        self.index -= 1;
        self.tokens.get(self.index as usize).cloned()
    }
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
            while content_chars
                .peek()
                .is_some_and(|char| char.is_alphanumeric() || *char == '_')
            {
                buffer.push(content_chars.next().unwrap());
                column += 1
            }
            if &buffer == &String::from("return") {
                tokens.push(Token {
                    token_type: TokenType::Return,
                    line: file_line,
                    column: column - buffer.len() as u32,
                });
            } else if &buffer == &String::from("finish") {
                tokens.push(Token {
                    token_type: TokenType::Finish,
                    line: file_line,
                    column: column - buffer.len() as u32,
                });
            } else if &buffer == &String::from("else") {
                tokens.push(Token {
                    token_type: TokenType::Else,
                    line: file_line,
                    column: column - buffer.len() as u32,
                });
            } else if &buffer == &String::from("if") {
                tokens.push(Token {
                    token_type: TokenType::If,
                    line: file_line,
                    column: column - buffer.len() as u32,
                });
            } else if &buffer == &String::from("bool") {
                tokens.push(Token {
                    token_type: TokenType::Bool,
                    line: file_line,
                    column: column - buffer.len() as u32,
                });
            } else if &buffer == &String::from("true") {
                tokens.push(Token {
                    token_type: TokenType::True,
                    line: file_line,
                    column: column - buffer.len() as u32,
                });
            } else if &buffer == &String::from("false") {
                tokens.push(Token {
                    token_type: TokenType::False,
                    line: file_line,
                    column: column - buffer.len() as u32,
                });
            } else if &buffer == &String::from("i32") {
                tokens.push(Token {
                    token_type: TokenType::I32,
                    line: file_line,
                    column: column - buffer.len() as u32,
                });
            } else if &buffer == &String::from("i64") {
                tokens.push(Token {
                    token_type: TokenType::I64,
                    line: file_line,
                    column: column - buffer.len() as u32,
                });
            } else if &buffer == &String::from("i16") {
                tokens.push(Token {
                    token_type: TokenType::I16,
                    line: file_line,
                    column: column - buffer.len() as u32,
                });
            } else if &buffer == &String::from("i8") {
                tokens.push(Token {
                    token_type: TokenType::I8,
                    line: file_line,
                    column: column - buffer.len() as u32,
                });
            } else if &buffer == &String::from("f64") {
                tokens.push(Token {
                    token_type: TokenType::F64,
                    line: file_line,
                    column: column - buffer.len() as u32,
                });
            } else if &buffer == &String::from("f32") {
                tokens.push(Token {
                    token_type: TokenType::F32,
                    line: file_line,
                    column: column - buffer.len() as u32,
                });
            } else if &buffer == &String::from("var") {
                tokens.push(Token {
                    token_type: TokenType::Var,
                    line: file_line,
                    column: column - buffer.len() as u32,
                });
            } else if &buffer == &String::from("let") {
                tokens.push(Token {
                    token_type: TokenType::Let,
                    line: file_line,
                    column: column - buffer.len() as u32,
                });
            } else if &buffer == &String::from("loop") {
                tokens.push(Token {
                    token_type: TokenType::Loop,
                    line: file_line,
                    column: column - buffer.len() as u32,
                });
            } else {
                tokens.push(Token {
                    token_type: TokenType::Ident(buffer.clone()),
                    line: file_line,
                    column: column - buffer.len() as u32,
                });
            }
        } else if !peeked.is_alphanumeric() {
            column += 1;
            match peeked {
                '(' => {
                    tokens.push(Token {
                        token_type: TokenType::OpenBracket,
                        line: file_line,
                        column: column - 1,
                    });
                    content_chars.next();
                }
                ')' => {
                    tokens.push(Token {
                        token_type: TokenType::CloseBracket,
                        line: file_line,
                        column: column - 1,
                    });
                    content_chars.next();
                }
                '[' => {
                    tokens.push(Token {
                        token_type: TokenType::OpenSquareBracket,
                        line: file_line,
                        column: column - 1,
                    });
                    content_chars.next();
                }
                ']' => {
                    tokens.push(Token {
                        token_type: TokenType::CloseSquareBracket,
                        line: file_line,
                        column: column - 1,
                    });
                    content_chars.next();
                }
                '<' => {
                    content_chars.next();
                    if *content_chars.peek().unwrap() == '=' {
                        content_chars.next();
                        tokens.push(Token {
                            token_type: TokenType::LessEqual,
                            line: file_line,
                            column: column - 2,
                        });
                    } else {
                        tokens.push(Token {
                            token_type: TokenType::OpenAngleBracket,
                            line: file_line,
                            column: column - 1,
                        });
                    }
                }
                '>' => {
                    content_chars.next();
                    if *content_chars.peek().unwrap() == '=' {
                        content_chars.next();
                        tokens.push(Token {
                            token_type: TokenType::GreatEqual,
                            line: file_line,
                            column: column - 2,
                        });
                    } else {
                        tokens.push(Token {
                            token_type: TokenType::CloseAngleBracket,
                            line: file_line,
                            column: column - 1,
                        });
                    }
                }
                '{' => {
                    tokens.push(Token {
                        token_type: TokenType::OpenCurlyBracket,
                        line: file_line,
                        column: column - 1,
                    });
                    content_chars.next();
                }
                '}' => {
                    tokens.push(Token {
                        token_type: TokenType::CloseCurlyBracket,
                        line: file_line,
                        column: column - 1,
                    });
                    content_chars.next();
                }
                '=' => {
                    content_chars.next();
                    if *content_chars.peek().unwrap() == '=' {
                        content_chars.next();
                        tokens.push(Token {
                            token_type: TokenType::DoubleEqual,
                            line: file_line,
                            column: column - 2,
                        });
                    } else if *content_chars.peek().unwrap() == '>' {
                        content_chars.next();
                        tokens.push(Token {
                            token_type: TokenType::GreatEqual,
                            line: file_line,
                            column: column - 2,
                        });
                    } else if *content_chars.peek().unwrap() == '<' {
                        content_chars.next();
                        tokens.push(Token {
                            token_type: TokenType::LessEqual,
                            line: file_line,
                            column: column - 2,
                        });
                    } else {
                        tokens.push(Token {
                            token_type: TokenType::Equal,
                            line: file_line,
                            column: column - 1,
                        });
                    }
                }
                ':' => {
                    tokens.push(Token {
                        token_type: TokenType::Colon,
                        line: file_line,
                        column: column - 1,
                    });
                    content_chars.next();
                }
                '+' => {
                    tokens.push(Token {
                        token_type: TokenType::Add,
                        line: file_line,
                        column: column - 1,
                    });
                    content_chars.next();
                }
                '-' => {
                    tokens.push(Token {
                        token_type: TokenType::Minus,
                        line: file_line,
                        column: column - 1,
                    });
                    content_chars.next();
                }
                '*' => {
                    tokens.push(Token {
                        token_type: TokenType::Star,
                        line: file_line,
                        column: column - 1,
                    });
                    content_chars.next();
                }
                '/' => {
                    tokens.push(Token {
                        token_type: TokenType::ForwardSlash,
                        line: file_line,
                        column: column - 1,
                    });
                    content_chars.next();
                }
                ';' => {
                    tokens.push(Token {
                        token_type: TokenType::SemiColon,
                        line: file_line,
                        column: column - 1,
                    });
                    content_chars.next();
                }
                '\n' => {
                    tokens.push(Token {
                        token_type: TokenType::NewLine,
                        line: file_line,
                        column: column - 1,
                    });
                    content_chars.next();
                    file_line += 1;
                    column = 1;
                }
                ',' => {
                    tokens.push(Token {
                        token_type: TokenType::Comma,
                        line: file_line,
                        column: column - 1,
                    });
                    content_chars.next();
                }
                _ => panic!("unkown character: {}", peeked),
            }
        } else if peeked.is_ascii_digit() {
            while content_chars
                .peek()
                .is_some_and(|char| char.is_ascii_digit())
            {
                buffer.push(content_chars.next().unwrap());
                column += 1
            }
            if *content_chars.peek().unwrap() == '.' {
                buffer.push(content_chars.next().unwrap());
                column += 1;
                while content_chars
                    .peek()
                    .is_some_and(|char| char.is_ascii_digit())
                {
                    buffer.push(content_chars.next().unwrap());
                    column += 1
                }
                tokens.push(Token {
                    token_type: TokenType::NumberLit(buffer.clone()),
                    line: file_line,
                    column: column - buffer.len() as u32,
                });
            } else {
                tokens.push(Token {
                    token_type: TokenType::NumberLit(buffer.clone()),
                    line: file_line,
                    column: column - buffer.len() as u32,
                });
            }
        }
        buffer.clear();
    }
    tokens.push(Token {
        token_type: TokenType::Eof,
        line: file_line,
        column: column,
    });

    tokens
}
