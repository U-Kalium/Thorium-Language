use std::collections::HashMap;

use crate::tokenizer::{Token};
use crate::tokenizer::{TokenType::*, WordToken::*};

struct MachineState {
    functions: HashMap<String, usize>,
    exports: HashMap<String, usize>,
    function_stack: Vec<usize>,
    stack: Vec<StackValue>,
    variables: HashMap<String, StackValue>
}
#[derive(Debug, Clone, Copy)]
enum StackValue {
    I128(i128),
    I64(i64),
    I32(i32),
    I16(i16),
    I8(i8),
    F32(f32),
    F64(f64),
    Null
}

impl MachineState {
    fn new() -> Self {
        Self {
            functions: HashMap::new(),
            exports: HashMap::new(),
            function_stack: Vec::new(),
            stack: Vec::new(),
            variables: HashMap::new()
        }
    }
}

struct TokenIter {
    tokens: Vec<Token>,
    index: usize,
}
impl TokenIter {
    fn next(&mut self) -> Token {
        self.index += 1;
        self.tokens[self.index].clone()
    }
    fn current(&mut self) ->  Token {
        self.tokens[self.index].clone()
    }
    fn reset(&mut self) {
        self.index = 0
    }
    fn peek(&self) -> Token {
        self.tokens[self.index + 1].clone()
    }
}

pub fn run(tokens: &Vec<Token>) {
    let mut state = MachineState::new();
    let mut token_iter = TokenIter {
        tokens: tokens.clone(),
        index: 0
    };
    find_funcs(&mut token_iter, &mut state);
    // state.functions.get(k)
    println!("funcs: {:?}", state.functions);
    println!("exported funcs: {:?}", state.exports);
    let start_func_index = state.exports.get("_start").unwrap().clone();
    run_func(&mut token_iter, &mut state, start_func_index);
}

fn run_func(tokens: &mut TokenIter, state: &mut MachineState, func_index: usize) {
    tokens.index = func_index;
    while tokens.peek().token_type != Word(EndFunc) {
        let token = tokens.next();
        match token.token_type {
            Word(Call) => {
                run_call(tokens, state);
            }
            Word(Return) => {
                break;
            }
            Word(I128) => {
                run_numeric_instruction(tokens, state, NumericType::I128);
            }
            Word(I64) => {
                run_numeric_instruction(tokens, state, NumericType::I64);
            }
            Word(I32) => {
                run_numeric_instruction(tokens, state, NumericType::I32);
            }
            Word(I16) => {
                run_numeric_instruction(tokens, state, NumericType::I16);
            }
            Word(I8) => {
                run_numeric_instruction(tokens, state, NumericType::I8);
            }
            Word(F64) => {
                run_numeric_instruction(tokens, state, NumericType::F64);
            }
            Word(F32) => {
                run_numeric_instruction(tokens, state, NumericType::F32);
            }
            Word(Declare) => {
                run_variable_decleration(tokens, state);
            }
            Word(Set) => {
                run_variable_set(tokens, state);
            }
            Word(Get) => {
                run_variable_get(tokens, state);
            }
            wrong_token => panic!("Syntax Error: Did not expect {wrong_token:?} at {}:{}", token.line, token.column)
        }
    }
    if state.function_stack.len() >= 1 {
        tokens.index = state.function_stack.pop().unwrap() 
    } else {
        // let top_of_stack = state.stack.pop().unwrap();
        println!("value of stack is: {:?}", state.stack)
    }
}

enum NumericType {
    I128,
    I64,
    I32,
    I16,
    I8,
    F32,
    F64

}

fn run_variable_decleration(tokens: &mut TokenIter, state: &mut MachineState) {
    while match tokens.peek().token_type {
        VarIdent(_) => true,
        _ => false
    } {
        let token = tokens.next();
        if let VarIdent(ident) = token.token_type {
            state.variables.insert(ident.clone(), StackValue::Null);
        }
    }
}

fn run_variable_set(tokens: &mut TokenIter, state: &mut MachineState) {
    let token = tokens.next(); 
    match &token.token_type {
        VarIdent(ident) => {
            if let Some(value) = state.stack.pop() {
                if let Some(var) = state.variables.get_mut(ident) {
                    *var = value
                } else {
                    panic!("Error: Variable {ident} has not been declared found at {}:{}", token.line, token.column)
                }
            }
        }
        wrong_token => panic!("Syntax Error: Expected variable ident found {:?} at {}:{}",wrong_token,  token.line, token.column)
    }
}

fn run_variable_get(tokens: &mut TokenIter, state: &mut MachineState) {
    let token = tokens.next();
    match &token.token_type {
        VarIdent(ident) => {
            if let Some(var) = state.variables.get(ident) {
                state.stack.push(*var);
            } else {
                panic!("Error: Variable {ident} has not been declared found at {}:{}", token.line, token.column)
            }
        }
        wrong_token => panic!("Syntax Error: Expected variable ident found {:?} at {}:{}",wrong_token,  token.line, token.column)
    }
}

fn run_numeric_instruction(tokens: &mut TokenIter, state: &mut MachineState, num_type: NumericType) {
    let mut token = tokens.next();
    match token.token_type {
        Word(Push) => {
            token = tokens.next();
            match token.token_type {
                Number(num) => {
                    match num_type {
                        NumericType::I128 => state.stack.push(StackValue::I128(num)),
                        NumericType::I64 => state.stack.push(StackValue::I64(num as i64)),
                        NumericType::I32 => state.stack.push(StackValue::I32(num as i32)),
                        NumericType::I16 => state.stack.push(StackValue::I16(num as i16)),
                        NumericType::I8 => state.stack.push(StackValue::I8(num as i8)),
                        NumericType::F32 => {
                            token = tokens.next();
                            if token.token_type != FullStop {
                                panic!("Syntax Error: Expected full stop for float found {:?} at {}:{}",token.token_type,  token.line, token.column)
                            }
                            token = tokens.next();
                            match token.token_type {
                                Number(num_after_decimal) => {
                                    let float: f32 = format!("{num}.{num_after_decimal}").parse().unwrap();
                                    state.stack.push(StackValue::F32(float))
                                } 
                                wrong_token => panic!("Syntax Error: Expected number found {wrong_token:?} at {}:{}", token.line, token.column)
                            }
                        },
                        NumericType::F64 => {
                            token = tokens.next();
                            if token.token_type != FullStop {
                                panic!("Syntax Error: Expected full stop for float found {:?} at {}:{}",token.token_type,  token.line, token.column)
                            }
                            token = tokens.next();
                            match token.token_type {
                                Number(num_after_decimal) => {
                                    let float: f64 = format!("{num}.{num_after_decimal}").parse().unwrap();
                                    state.stack.push(StackValue::F64(float))
                                }
                                wrong_token => panic!("Syntax Error: Expected number found {wrong_token:?} at {}:{}", token.line, token.column)
                            }
                        },
                    }
                }
                wrong_token => panic!("Syntax Error: Expected number found {wrong_token:?} at {}:{}", token.line, token.column)
            }
            while match tokens.peek().token_type {
                Number(_) => true,
                _ => false
            } {
                token = tokens.next();
                match token.token_type {
                    Number(num) => {
                        match num_type {
                            NumericType::I128 => state.stack.push(StackValue::I128(num)),
                            NumericType::I64 => state.stack.push(StackValue::I64(num as i64)),
                            NumericType::I32 => state.stack.push(StackValue::I32(num as i32)),
                            NumericType::I16 => state.stack.push(StackValue::I16(num as i16)),
                            NumericType::I8 => state.stack.push(StackValue::I8(num as i8)),
                            NumericType::F32 => {
                                token = tokens.next();
                                if token.token_type != FullStop {
                                    panic!("Syntax Error: Expected full stop for float found {:?} at {}:{}",token.token_type,  token.line, token.column)
                                }
                                token = tokens.next();
                                match token.token_type {
                                    Number(num_after_decimal) => {
                                        let float: f32 = format!("{num}.{num_after_decimal}").parse().unwrap();
                                        state.stack.push(StackValue::F32(float))
                                    } 
                                    wrong_token => panic!("Syntax Error: Expected number found {wrong_token:?} at {}:{}", token.line, token.column)
                                }
                            },
                            NumericType::F64 => {
                                token = tokens.next();
                                if token.token_type != FullStop {
                                    panic!("Syntax Error: Expected full stop for float found {:?} at {}:{}",token.token_type,  token.line, token.column)
                                }
                                token = tokens.next();
                                match token.token_type {
                                    Number(num_after_decimal) => {
                                        let float: f64 = format!("{num}.{num_after_decimal}").parse().unwrap();
                                        state.stack.push(StackValue::F64(float))
                                    }
                                    wrong_token => panic!("Syntax Error: Expected number found {wrong_token:?} at {}:{}", token.line, token.column)
                                }
                            },
                        }
                    }
                    _ => panic!("This should be unreachable")
                }
            }
            

        }
        wrong_token => panic!("Syntax Error: Did not expect {wrong_token:?} at {}:{} expected numeric intruction", token.line, token.column)
    }
}

fn run_call(tokens: &mut TokenIter, state: &mut MachineState) {
    let token = tokens.next();
    let call_site_index = tokens.index;
    match token.token_type {
        FuncIdent(ident) => {
            let func_index = state.functions.get(&ident).unwrap();
            tokens.index = call_site_index + 1;
            state.function_stack.push(call_site_index);
            run_func(tokens, state, *func_index);
        }
        wrong_token => panic!("Syntax Error: Expected function ident found {wrong_token:?} at {}:{}", token.line, token.column)
    }
}

fn find_funcs(tokens: &mut TokenIter, state: &mut MachineState) {
    let mut token = tokens.current();
    match token.token_type {
        Word(Func) => {
            let func_index ;
            let mut func_export: Option<String> = None;
            let func_ident;
            token = tokens.next();
            match token.token_type {
                FuncIdent(ident) => {
                    func_ident = ident
                }
                wrong_token => panic!("Syntax Error: Expected function ident found {wrong_token:?} at {}:{}", token.line, token.column)
            }
            token = tokens.next();
            match token.token_type {
                StringLit(lit) => {
                    func_export = Some(lit);
                    token = tokens.next();
                    match token.token_type {
                        Colon => func_index = tokens.index,
                        wrong_token => panic!("Syntax Error: Expected colon or export found {wrong_token:?} at {}:{}", token.line, token.column)
                    }
                },
                Colon => func_index = tokens.index,
                wrong_token => panic!("Syntax Error: Expected colon or export found {wrong_token:?} at {}:{}", token.line, token.column)
            }
            state.functions.insert(func_ident.clone(), func_index);
            if let Some(export) = func_export {
                state.exports.insert(export, func_index);
            }
        }
        EOF => {
            tokens.reset();
            return;
        },
        _ => {}
    }
    tokens.next();
    find_funcs(tokens, state);
}