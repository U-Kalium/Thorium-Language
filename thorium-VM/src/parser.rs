use core::panic;
use std::iter::Peekable;

use crate::tokenizer::Token;
use crate::syntax_tree::*;


impl NodeFunc {
    fn parse<'a, I: Iterator<Item = &'a Token>>(tokens: &mut Peekable<I>) -> NodeFunc {
        let mut func_node = NodeFunc {
            ident: String::new(),
            export_name: None,
            intructions: Vec::new(),
        };
        tokens.next();
        match tokens.next().unwrap() {
            Token::FuncIdent(ident) => func_node.ident = ident.clone(),
            token => panic!("Syntax Error: expected FuncIdent found {:?}", token)
        }
        match tokens.next().unwrap() {
            Token::Export => {
                func_node.export_name = match tokens.next().unwrap() {
                    Token::StringLit(lit) => Some(lit.clone()),
                    token => panic!("Syntax Error: expected StringLit found {:?}", token)
                };
                func_node.intructions = match tokens.next().unwrap() {
                    Token::Colon => parse_instructions(tokens),
                    token => panic!("Syntax Error: expected Colon found {:?}", token)
                }
            }
            Token::Colon => {
                func_node.intructions = parse_instructions(tokens);
            }
            token => panic!("Syntax Error: expected Export or Colon found {:?}", token)
        }
        func_node
    }
}

fn parse_instructions<'a, I: Iterator<Item = &'a Token>>(tokens: &mut Peekable<I>) -> Vec<NodeIntruction> {
    let mut instrucions = Vec::new();

    while tokens.peek().is_some_and(|token| match **token {
        Token::I32 => true,
        Token::I16 => true,
        Token::I8 => true,
        Token::I64 => true,
        Token::Call => true,
        _ => false
    }) {
        instrucions.push(NodeIntruction::parse(tokens));
    }

    instrucions
}

fn parse_value_instruction<'a, I: Iterator<Item = &'a Token>>(tokens: &mut Peekable<I>, node_type: NodeType) -> NodeIntruction {
    let token = tokens.next().unwrap();
    if *token != Token::FullStop {
        panic!("Syntax Error: expected FUllstop found {:?}", token)
    }
    match tokens.next().unwrap() {
        Token::Const => {
            match tokens.next().unwrap() {

                Token::Integer(int) => match node_type {
                    NodeType::I32 => NodeIntruction::Const(NodeValue::I32(*int as i32)),
                    NodeType::I64 => NodeIntruction::Const(NodeValue::I64(*int as i64)),
                    NodeType::I16 => NodeIntruction::Const(NodeValue::I16(*int as i16)),
                    NodeType::I8 => NodeIntruction::Const(NodeValue::I8(*int as i8)),
                },
                token => panic!("Syntax Error: expected Value found {:?}", token)
            }
            // value);
        }
        Token::Return => {
            NodeIntruction::Return(node_type)
        },
        Token::Declare => {
            NodeIntruction::Declare {
                variable: NodeVariable::parse(tokens),
                node_type: node_type
            }
        }
        Token::Set => {
            NodeIntruction::Set {
                variable:NodeVariable::parse(tokens),
                node_type: node_type
            }
        }
        Token::Get => {
            NodeIntruction::Get {
                variable:NodeVariable::parse(tokens),
                node_type: node_type
            }
        }
        token => panic!("Syntax Error: this {:?} does not go after a type identifier", token)
    }
}

impl NodeIntruction {
    fn parse<'a, I: Iterator<Item = &'a Token>>(tokens: &mut Peekable<I>) -> NodeIntruction {
        match tokens.next().unwrap() {
            Token::I32 => {
                parse_value_instruction(tokens, NodeType::I32)
            },
            Token::I16 => {
                parse_value_instruction(tokens, NodeType::I16)
            },
            Token::I64 => {
                parse_value_instruction(tokens, NodeType::I64)
            },
            Token::I8 => {
                parse_value_instruction(tokens, NodeType::I8)
            },
            Token::Call => {
                match tokens.next().unwrap() {
                    Token::FuncIdent(ident) => NodeIntruction::Call(ident.clone()),
                    token => panic!("Syntax Error: expected FuncIdent found {:?}", token)
                }
            }
            token => panic!("Syntax Error: expected instruction found {:?}", token)
        }
    }
}

impl NodeVariable {
    fn parse<'a, I: Iterator<Item = &'a Token>>(tokens: &mut Peekable<I>) -> NodeVariable {
        match tokens.next().unwrap() {
            Token::VarIdent(ident) => {
                NodeVariable {
                    ident: ident.clone(),
                }
            }
            token => panic!("yntax Error: expected Variable Ident found {:?}", token)
        }
    }
}


pub fn parse_tokens(tokens: &Vec<Token>) -> NodeProgram {
    let mut functions = Vec::new();
    let mut tokens_iter = tokens.iter().peekable();

    while tokens_iter.peek().is_some() {
        let peeked = tokens_iter.peek().unwrap();
        match peeked {
            Token::Func => {
                functions.push(NodeFunc::parse(&mut tokens_iter));
            }
            Token::EOF => {break;}
            _ => panic!("invalid token found at program base: {:?}", peeked)
        }
    }

    NodeProgram { funcs: functions }
}

