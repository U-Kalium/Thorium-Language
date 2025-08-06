use thorium_macros::tokenize_words;

#[derive(PartialEq, Debug)]
pub enum Token {
    Word(WordToken),
    FuncIdent(String),
    Colon,
    FullStop,
    Number(i128),
    StringLit(String),
    EOF,
    VarIdent(String),
    Dash,
}

#[derive(PartialEq, Debug)]
#[tokenize_words]
pub enum WordToken {
    Func,
    I32,
    I64,
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
}

pub fn tokenize(content: String) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let binding = content.to_lowercase();
    let mut content_chars = binding.chars().peekable();

    while content_chars.peek().is_some() {
        let peeked = content_chars.peek().unwrap();
        let mut buffer = String::new();

        if peeked.is_whitespace() {
            content_chars.next();
        } else if peeked.is_alphabetic() {
            while content_chars
                .peek()
                .is_some_and(|char| char.is_alphanumeric())
            {
                buffer.push(content_chars.next().unwrap());
            }

            if let Err(error) = tokenize_word(&mut tokens, &mut buffer) {
                return Err(error);
            }
        } else if peeked.is_numeric() {
            while content_chars
                .peek()
                .is_some_and(|char| char.is_alphanumeric())
            {
                buffer.push(content_chars.next().unwrap());
            }
            tokens.push(Token::Number(
                buffer.parse().expect("could not parse integer"),
            ));
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
                    while content_chars
                        .peek()
                        .is_some_and(|char| char.is_alphanumeric())
                    {
                        buffer.push(content_chars.next().unwrap());
                    }

                    tokens.push(Token::FuncIdent(buffer.clone()));
                }
                '%' => {
                    content_chars.next();
                    while content_chars
                        .peek()
                        .is_some_and(|char| char.is_alphanumeric())
                    {
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
                unkown => return Err(format!("unkown punctuation word {}", unkown).to_string()),
            }
        }
        buffer.clear();
    }
    tokens.push(Token::EOF);

    Ok(tokens)
}
