use std::{collections::HashSet, iter::Peekable};

use crate::tokenizer::Token;

pub struct NodeProgram {
    pub functions: Vec<NodeFunc>
}

pub struct NodeFunc {
    pub ident: String,
    pub parems: Option<NodeParems>,
    pub func_return: Option<NodeType>,
    pub body: NodeScope
}
impl NodeFunc {
    fn parse<'a, I: Iterator<Item = &'a Token>>(tokens: &mut Peekable<I>) -> Self {
        let mut ident = String::new();
        let mut func_return = None;
        let mut func_scope = NodeScope { statements: Vec::new(), variables: HashSet::new() };
        // consuming Fn token
        let mut prev_token = tokens.next().unwrap();

        'consume: while tokens.peek().is_some() {
            let current_token = tokens.next().unwrap();
            match prev_token {
                Token::Fn => {
                    match current_token {
                        Token::Ident(ident_name) => ident = ident_name.clone(),
                        _ => panic!("did not expect {:?} token after Fn token", current_token)
                    }
                },
                Token::Ident(ident) => {
                    match current_token {
                        Token::OpenBracket => {}
                        _ => panic!("did not expect {:?} token after Ident token", current_token)
                    }
                },
                Token::OpenBracket => {
                    match current_token {
                        Token::CloseBracket => {}
                        _ => panic!("did not expect {:?} token after LeftParenth token", current_token)
                    }
                },
                Token::CloseBracket => {
                    match current_token {
                        Token::I32 => {
                            func_return = Some(NodeType::I32)
                        }
                        _ => panic!("did not expect {:?} token after RightParenth token", current_token)
                    }
                },
                Token::I32 => {
                    match current_token {
                        Token::OpenCurlyBrack => {
                            func_scope = NodeScope::parse(tokens);
                            break 'consume;
                        }
                        _ => panic!("did not expect {:?} token after I32 token", current_token)
                    }
                }
                _ => panic!("did not expect {:?} token", prev_token)
            }

            prev_token = current_token;
        } 

        if ident.is_empty() {
            panic!("did not provide an identifier ")
        }

        Self {
            parems: None,
            func_return,
            ident,
            body: func_scope
        }
    }

}

struct NodeParems {

}
pub enum NodeType {
    I32
}
pub struct NodeScope {
    pub statements: Vec<NodeStatement>,
    pub variables: HashSet<String>
}

impl NodeScope {
    fn parse<'a, I: Iterator<Item = &'a Token>>(tokens: &mut Peekable<I>) -> Self {
        let mut statements = Vec::new();
        let mut variables = HashSet::new();

        'consume: while tokens.peek().is_some() {
            let peeked = tokens.peek().unwrap();
            match peeked {
                Token::Return => {
                    statements.push(NodeStatement::parse(tokens, &mut variables));
                }
                Token::Var =>{
                    statements.push(NodeStatement::parse(tokens, &mut variables));
                }
                Token::CloseCurlyBrack => {
                    tokens.next();
                    break 'consume;
                }
                Token::Ident(ident) => {
                    statements.push(NodeStatement::parse(tokens, &mut variables));
                }
                Token::NewLine => {
                    tokens.next();
                }
                _ => panic!("expected keywoard or ident token found {:?}", peeked)
            }
        }

        Self {
            statements,
            variables
        }
    }
}

pub enum NodeStatement {
    Return(NodeExpr),
    VariableDecleration{
        ident: String,
        var_type: NodeType,
        expression: NodeExpr
    },
    VariableAssignment{
        ident: String,
        expression: NodeExpr
    }
}

impl NodeStatement {
    fn parse<'a, I: Iterator<Item = &'a Token>>(tokens: &mut Peekable<I>, variables: &mut HashSet<String>) -> Self {
        let current_token = tokens.next().unwrap();
        let mut node_statement: NodeStatement;

        match current_token {
            Token::Return => {
                let peeked = tokens.peek().unwrap();
                match peeked {
                    Token::IntLit(int) => {
                        return NodeStatement::Return(NodeExpr::parse(tokens, variables));
                    }
                    Token::Ident(ident) => {
                        return NodeStatement::Return(NodeExpr::parse(tokens, variables));
                    }
                    _ => panic!("expected value token found {:?}", peeked)
                }
            }
            Token::Var => {
                let ident_token = tokens.next().unwrap();
                let mut ident = String::new();
                let mut var_type: NodeType;
                let mut expression: NodeExpr;
                match ident_token {
                    Token::Ident(name) => {
                        if !variables.contains(&name.clone()) {
                            ident = name.clone();
                            variables.insert(name.clone());
                        } else {
                            panic!("variable {} is already defined", name);
                        }
                        
                    }
                    _ => panic!("expected identifier token found {:?}", ident_token) 
                }
                let colon_or_equal = tokens.next().unwrap();
                match colon_or_equal {
                    Token::Equal => todo!("need to implement type infrence"),
                    Token::Colon => {}
                    _ => panic!("expected colon or equal found {:?}", colon_or_equal)
                }
                let var_type_token = tokens.next().unwrap();
                match var_type_token {
                    Token::I32 => var_type = NodeType::I32,
                    _ => panic!("expected type found {:?}", var_type_token)
                };
                let equal_token = tokens.next().unwrap();
                match equal_token {
                    Token::Equal => expression = NodeExpr::parse(tokens, variables),
                    _ => panic!("expected equal sign found {:?}", equal_token)
                }
                node_statement = Self::VariableDecleration { ident, var_type, expression }
            }
            Token::Ident(ident) => {
                let mut expression: NodeExpr;
                if !variables.contains(ident) {
                    panic!("variable {ident} not declared ")
                }
                let equal_token = tokens.next().unwrap(); 
                match equal_token {
                    Token::Equal => {
                        expression = NodeExpr::parse(tokens, variables)
                    }
                    _ => panic!("expected = found {equal_token:?}")
                }
                node_statement = Self::VariableAssignment{
                    ident: ident.clone(),
                    expression

                }
            }
            _ => panic!("expected statement token found {:?}", current_token)
        }
        let end_statement_token = tokens.next().unwrap();
        match end_statement_token {
            Token::Semi => {}
            Token::NewLine => {}
            _ => panic!("expected end of statement (either newline or ;) found {end_statement_token:?}")
        }
        node_statement
    }

}

pub enum NodeExpr {
    I32(i32),
    Variable(String)
}

impl NodeExpr {
    fn parse<'a, I: Iterator<Item = &'a Token>>(tokens: &mut Peekable<I>, variables: &mut HashSet<String>) -> Self {
        let peeked = tokens.peek().unwrap();
        match peeked {
            Token::Ident(name) => {
                if variables.contains(name) {
                    tokens.next();
                    NodeExpr::Variable(name.clone())
                } else {
                    panic!("variable {} has not been declared", name)
                }
            }
            Token::IntLit(val) => {
                tokens.next();
                NodeExpr::I32(*val)
            }
            _ => panic!("expected an ident or i32literal got {:?}", peeked)
        }
    }

}

pub fn parse(tokens: Vec<Token>) -> NodeProgram {
    let mut functions = Vec::new();
    let mut tokens_iter = tokens.iter().peekable();
    while tokens_iter.peek().is_some() {
        let peeked = tokens_iter.peek().unwrap();
        match peeked {
            Token::Fn => {
                functions.push(NodeFunc::parse(&mut tokens_iter));
            }
            _ => panic!("invalid token found at program base: {:?}", peeked)
        }
    }

    NodeProgram {
        functions
    }
}
