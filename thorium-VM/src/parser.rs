use core::panic;
use std::iter::Peekable;
use std::ops::Neg;

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
        Token::F32 => true,
        Token::F64 => true,
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
        Token::Push => {
            let mut  is_negative = false;
            match tokens.peek().unwrap() {
                Token::Dash => {
                    tokens.next();
                    is_negative = true
                }
            _ => {}
            }
            match tokens.next().unwrap() {
                Token::Integer(int) => {
                    match node_type {
                        NodeType::F32 => {
                            let left_of_point = *int;
                            match tokens.next().unwrap() {
                                Token::FullStop => {
                                    match tokens.next().unwrap() {
                                        Token::Integer(right_of_point) => {
                                            let float: f32 = format!("{}.{}", left_of_point, right_of_point).parse().expect("could not convert to float");
                                            if is_negative {
                                                NodeIntruction::Push(NodeValue::F32(-float))
                                            } else {
                                                NodeIntruction::Push(NodeValue::F32(float))
                                            }
                                        }
                                        token => panic!("Syntax Error: expected . found {:?}", token)   
                                    }

                                }
                                token => panic!("Syntax Error: expected . found {:?}", token)
                            }
                        }
                        NodeType::F64 => {
                            
                            let left_of_point = *int;
                            match tokens.next().unwrap() {
                                Token::FullStop => {
                                    match tokens.next().unwrap() {
                                        Token::Integer(right_of_point) => {
                                            let float: f64 = format!("{}.{}", left_of_point, right_of_point).parse().expect("could not convert to float");
                                            if is_negative {
                                                NodeIntruction::Push(NodeValue::F64(-float))
                                            } else {
                                                NodeIntruction::Push(NodeValue::F64(float))
                                            }
                                        }
                                        token => panic!("Syntax Error: expected . found {:?}", token)   
                                    }

                                }
                                token => panic!("Syntax Error: expected . found {:?}", token)
                            }
                        }
                        NodeType::I32 => {
                            if **tokens.peek().unwrap() == Token::FullStop {
                                panic!("Did not expect Fullstop after none float type")
                            }
                            if is_negative {
                                NodeIntruction::Push(NodeValue::I32((*int as i32 ).neg()))
                            } else {
                                NodeIntruction::Push(NodeValue::I32(*int as i32))
                            }
                        },
                        NodeType::I64 => {
                            if **tokens.peek().unwrap() == Token::FullStop {
                                panic!("Did not expect Fullstop after none float type")
                            }
                            if is_negative {
                                NodeIntruction::Push(NodeValue::I64((*int as i64 ).neg()))
                            } else {
                                NodeIntruction::Push(NodeValue::I64(*int as i64))
                            }
                        },
                        NodeType::I16 => {
                            if **tokens.peek().unwrap() == Token::FullStop {
                                panic!("Did not expect Fullstop after none float type")
                            }
                            if is_negative {
                                NodeIntruction::Push(NodeValue::I16((*int as i16 ).neg()))
                            } else {
                                NodeIntruction::Push(NodeValue::I16(*int as i16))
                            }
                        },
                        NodeType::I8 => {
                            if **tokens.peek().unwrap() == Token::FullStop {
                                panic!("Did not expect Fullstop after none float type")
                            }
                            if is_negative {
                                NodeIntruction::Push(NodeValue::I8((*int as i8 ).neg()))
                            } else {
                                NodeIntruction::Push(NodeValue::I8(*int as i8))
                            }
                        },
                    }
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
        Token::Add => {
            NodeIntruction::Add(node_type)
        }
        Token::Sub => {
            NodeIntruction::Sub(node_type)
        }
        Token::Div => {
            NodeIntruction::Div(node_type)
        }
        Token::Mul => {
            NodeIntruction::Mul(node_type)
        }
        Token::Pop => {
            NodeIntruction::Pop(node_type)
        }
        token => panic!("Syntax Error: this {:?} does not go after a type identifier", token)
    }
}

impl NodeIntruction {
    fn parse<'a, I: Iterator<Item = &'a Token>>(tokens: &mut Peekable<I>) -> NodeIntruction {
        match tokens.next().unwrap() {
            Token::I32 => {
                parse_value_instruction(tokens, NodeType::I32)
            }
            Token::I16 => {
                parse_value_instruction(tokens, NodeType::I16)
            }
            Token::I64 => {
                parse_value_instruction(tokens, NodeType::I64)
            }
            Token::I8 => {
                parse_value_instruction(tokens, NodeType::I8)
            }
            Token::F32 => {
                parse_value_instruction(tokens, NodeType::F32)
            }
            Token::F64 => {
                parse_value_instruction(tokens, NodeType::F64)
            }
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

