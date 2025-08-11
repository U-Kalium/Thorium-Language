use core::panic;
use std::collections::HashMap;
use std::iter::Peekable;
use std::ops::Neg;

use crate::syntax_tree::*;
use crate::tokenizer::{TokenType, WordToken};

impl NodeFunc {
    fn parse<'a, I: Iterator<Item = &'a TokenType>>(tokens: &mut Peekable<I>) -> NodeFunc {
        let mut func_node = NodeFunc {
            ident: String::new(),
            export_name: None,
            intructions: Vec::new(),
            labels: HashMap::new()
        };
        tokens.next();
        match tokens.next().unwrap() {
            TokenType::FuncIdent(ident) => func_node.ident = ident.clone(),
            token => panic!("Syntax Error: expected FuncIdent found {:?}", token),
        }
        match tokens.next().unwrap() {
            TokenType::Word(WordToken::Export) => {
                func_node.export_name = match tokens.next().unwrap() {
                    TokenType::StringLit(lit) => Some(lit.clone()),
                    token => panic!("Syntax Error: expected StringLit found {:?}", token),
                };
                func_node.intructions = match tokens.next().unwrap() {
                    TokenType::Colon => parse_instructions(tokens, &mut func_node.labels),
                    token => panic!("Syntax Error: expected Colon found {:?}", token),
                }
            }
            TokenType::Colon => {
                func_node.intructions = parse_instructions(tokens, &mut func_node.labels);
            }
            token => panic!("Syntax Error: expected Export or Colon found {:?}", token),
        }
        func_node
    }
}

fn parse_instructions<'a, I: Iterator<Item = &'a TokenType>>(
    tokens: &mut Peekable<I>,
    labels: &mut HashMap<String, usize>
) -> Vec<NodeIntruction> {
    let mut instrucions = Vec::new();
    let mut index = 0;

    while tokens.peek().is_some_and(|(token)| match *token {
        TokenType::Word(WordToken::I32) => true,
        TokenType::Word(WordToken::I16) => true,
        TokenType::Word(WordToken::I8) => true,
        TokenType::Word(WordToken::I64) => true,
        TokenType::Word(WordToken::F32) => true,
        TokenType::Word(WordToken::F64) => true,
        TokenType::Word(WordToken::Call) => true,
        TokenType::Word(WordToken::Jmp) => true,
        TokenType::Word(WordToken::Jpz) => true,
        TokenType::Word(WordToken::Return) => true,
        TokenType::Word(WordToken::Get) => true,
        TokenType::Word(WordToken::Set) => true,
        TokenType::Word(WordToken::Declare) => true,
        TokenType::LabelIdent(label_name) => {
            // tokens.next();
            labels.insert(label_name.clone(), index);
            true
        }
        other_token => {
            false
        },
    }) {
        index += 1;
        if let Some(instruction) = NodeIntruction::parse(tokens) {
            instrucions.push(instruction);
        }
    }

    instrucions
}

fn parse_value_instruction<'a, I: Iterator<Item = &'a TokenType>>(
    tokens: &mut Peekable<I>,
    node_type: NodeType,
) -> NodeIntruction {
    let token = tokens.next().unwrap();
    if *token != TokenType::FullStop {
        panic!("Syntax Error: expected FUllstop found {:?}", token)
    }

    match tokens.next().unwrap() {
        TokenType::Word(WordToken::Push) => {
            let mut is_negative = false;
            match tokens.peek().unwrap() {
                TokenType::Dash => {
                    tokens.next();
                    is_negative = true
                }
                _ => {}
            }
            match tokens.next().unwrap() {
                TokenType::Number(int) => match node_type {
                    NodeType::F32 => {
                        let left_of_point = *int;
                        match tokens.next().unwrap() {
                            TokenType::FullStop => match tokens.next().unwrap() {
                                TokenType::Number(right_of_point) => {
                                    let float: f32 =
                                        format!("{}.{}", left_of_point, right_of_point)
                                            .parse()
                                            .expect("could not convert to float");
                                    if is_negative {
                                        NodeIntruction::Push(NodeValue::F32(-float))
                                    } else {
                                        NodeIntruction::Push(NodeValue::F32(float))
                                    }
                                }
                                token => {
                                    panic!("Syntax Error: expected . found {:?} for float", token)
                                }
                            },
                            token => panic!("Syntax Error: expected . found {:?} for float", token),
                        }
                    }
                    NodeType::F64 => {
                        let left_of_point = *int;
                        match tokens.next().unwrap() {
                            TokenType::FullStop => match tokens.next().unwrap() {
                                TokenType::Number(right_of_point) => {
                                    let float: f64 =
                                        format!("{}.{}", left_of_point, right_of_point)
                                            .parse()
                                            .expect("could not convert to float");
                                    if is_negative {
                                        NodeIntruction::Push(NodeValue::F64(-float))
                                    } else {
                                        NodeIntruction::Push(NodeValue::F64(float))
                                    }
                                }
                                token => {
                                    panic!("Syntax Error: expected . found {:?} for float", token)
                                }
                            },
                            token => panic!("Syntax Error: expected . found {:?} for float", token),
                        }
                    }
                    NodeType::I32 => {
                        if **tokens.peek().unwrap() == TokenType::FullStop {
                            panic!("Did not expect Fullstop after none float type")
                        }
                        if is_negative {
                            NodeIntruction::Push(NodeValue::I32((*int as i32).neg()))
                        } else {
                            NodeIntruction::Push(NodeValue::I32(*int as i32))
                        }
                    }
                    NodeType::I64 => {
                        if **tokens.peek().unwrap() == TokenType::FullStop {
                            panic!("Did not expect Fullstop after none float type")
                        }
                        if is_negative {
                            NodeIntruction::Push(NodeValue::I64((*int as i64).neg()))
                        } else {
                            NodeIntruction::Push(NodeValue::I64(*int as i64))
                        }
                    }
                    NodeType::I16 => {
                        if **tokens.peek().unwrap() == TokenType::FullStop {
                            panic!("Did not expect Fullstop after none float type")
                        }
                        if is_negative {
                            NodeIntruction::Push(NodeValue::I16((*int as i16).neg()))
                        } else {
                            NodeIntruction::Push(NodeValue::I16(*int as i16))
                        }
                    }
                    NodeType::I8 => {
                        if **tokens.peek().unwrap() == TokenType::FullStop {
                            panic!("Did not expect Fullstop after none float type")
                        }
                        if is_negative {
                            NodeIntruction::Push(NodeValue::I8((*int as i8).neg()))
                        } else {
                            NodeIntruction::Push(NodeValue::I8(*int as i8))
                        }
                    }
                },
                
                token => panic!("Syntax Error: expected Value found {:?}", token),
            }
            // value)

        }

        TokenType::Word(WordToken::Add) => NodeIntruction::Add(node_type),
        TokenType::Word(WordToken::Sub) => NodeIntruction::Sub(node_type),
        TokenType::Word(WordToken::Div) => NodeIntruction::Div(node_type),
        TokenType::Word(WordToken::Mul) => NodeIntruction::Mul(node_type),
        TokenType::Word(WordToken::Rem) => NodeIntruction::Rem(node_type), 
        TokenType::Word(WordToken::And) => NodeIntruction::And(node_type), 
        TokenType::Word(WordToken::Or) => NodeIntruction::Or(node_type), 
        TokenType::Word(WordToken::Xor) => NodeIntruction::Xor(node_type), 
        TokenType::Word(WordToken::Eq) => NodeIntruction::Eq(node_type), 
        TokenType::Word(WordToken::Neq) => NodeIntruction::Neq(node_type), 
        TokenType::Word(WordToken::Gte) => NodeIntruction::Gte(node_type), 
        TokenType::Word(WordToken::Gt) => NodeIntruction::Gt(node_type), 
        TokenType::Word(WordToken::Lte) => NodeIntruction::Lte(node_type), 
        TokenType::Word(WordToken::Lt) => NodeIntruction::Lt(node_type), 
        TokenType::Word(WordToken::Max) => NodeIntruction::Max(node_type), 
        TokenType::Word(WordToken::Min) => NodeIntruction::Min(node_type), 
        token => panic!(
            "Syntax Error: this {:?} does not go after a type identifier",
            token
        ),
    }
}

impl NodeIntruction {
    fn parse<'a, I: Iterator<Item = &'a TokenType>>(tokens: &mut Peekable<I>) -> Option<NodeIntruction> {
        match tokens.next().unwrap() {
            TokenType::Word(WordToken::I32) => Some(parse_value_instruction(tokens, NodeType::I32)),
            TokenType::Word(WordToken::I16) => Some(parse_value_instruction(tokens, NodeType::I16)),
            TokenType::Word(WordToken::I64) => Some(parse_value_instruction(tokens, NodeType::I64)),
            TokenType::Word(WordToken::I8) => Some(parse_value_instruction(tokens, NodeType::I8)),
            TokenType::Word(WordToken::F32) => Some(parse_value_instruction(tokens, NodeType::F32)),
            TokenType::Word(WordToken::F64) => Some(parse_value_instruction(tokens, NodeType::F64)),
            TokenType::Word(WordToken::Call) => match tokens.next().unwrap() {
                TokenType::FuncIdent(ident) => Some(NodeIntruction::Call(ident.clone())),
                token => panic!("Syntax Error: expected FuncIdent found {:?}", token),
            },
            TokenType::Word(WordToken::Pop) => Some(NodeIntruction::Pop),
            TokenType::Word(WordToken::Return) => Some(NodeIntruction::Return),
            TokenType::Word(WordToken::Declare) => Some(NodeIntruction::Declare {
                variable: NodeVariable::parse(tokens),
            }),
            TokenType::Word(WordToken::Set) => Some(NodeIntruction::Set {
                variable: NodeVariable::parse(tokens),
            }),
            TokenType::Word(WordToken::Get) => Some(NodeIntruction::Get {
                variable: NodeVariable::parse(tokens),
            }),
            TokenType::Word(WordToken::Jmp) => {
                match tokens.next().unwrap() {
                    TokenType::LabelIdent(ident) => {
                        Some(NodeIntruction::Jmp(ident.clone()))
                    }
                    token => panic!("Syntax Error: expected label ident found {:?}", token)
                }
            }
            TokenType::Word(WordToken::Jpz) => {
                match tokens.next().unwrap() {
                    TokenType::LabelIdent(ident) => {
                        Some(NodeIntruction::Jpz(ident.clone()))
                    }
                    token => panic!("Syntax Error: expected label ident found {:?}", token)
                }
            }
            TokenType::LabelIdent(_) => None,
            token => panic!("Syntax Error: expected instruction found {:?}", token),
        }
    }
}

impl NodeVariable {
    fn parse<'a, I: Iterator<Item = &'a TokenType>>(tokens: &mut Peekable<I>) -> NodeVariable {
        match tokens.next().unwrap() {
            TokenType::VarIdent(ident) => NodeVariable {
                ident: ident.clone(),
            },
            token => panic!("Syntax Error: expected Variable Ident found {:?}", token),
        }
    }
}

pub fn parse_tokens(tokens: &Vec<TokenType>) -> NodeProgram {
    let mut functions = Vec::new();
    let mut variables = HashMap::new();
    let mut tokens_iter = tokens.iter().peekable();

    while tokens_iter.peek().is_some() {
        let peeked = tokens_iter.peek().unwrap();
        match peeked {
            TokenType::Word(WordToken::Func) => {
                functions.push(NodeFunc::parse(&mut tokens_iter));
            }
            TokenType::EOF => {
                break;
            }
            _ => panic!("invalid token found at program base: {:?}", peeked),
        }
    }

    NodeProgram { funcs: functions, variables }
}
