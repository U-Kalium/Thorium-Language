use core::panic;
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

impl StackValue {
    fn is_zero(&self) -> bool {
        match self {
            StackValue::I128(val) => *val == 0,
            StackValue::I64(val) => *val == 0,
            StackValue::I32(val) => *val == 0,
            StackValue::I16(val) => *val == 0,
            StackValue::I8(val) => *val == 0,
            StackValue::F32(val) => *val == 0.0,
            StackValue::F64(val) => *val == 0.0,
            StackValue::Null => false,
        }
    }
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
    let mut labels = HashMap::new();
    // finding labels
    while tokens.peek().token_type != Word(EndFunc) {
        let token = tokens.next();
        if let LabelIdent(ident) = token.token_type {
            labels.insert(ident.clone(), tokens.index);
        }
    }
    tokens.index = func_index;
    while tokens.peek().token_type != Word(EndFunc) {
        let mut token = tokens.next();
        match token.token_type {
            LabelIdent(_) => {

            }
            Word(Call) => {
                run_call(tokens, state);
            }
            Word(Return) => {
                break;
            }
            Word(Jmp) => {
                token = tokens.next();
                if let StringLit(ident) = token.token_type {
                    if let Some(index) = labels.get(&ident) {
                        let value = state.stack.pop().unwrap();
                        if !value.is_zero() {
                            tokens.index = *index
                        }
                    } else {
                        panic!("Error: label {ident} does not exist yet is reffered to at {}:{}", token.line, token.column)
                    }
                } else {
                    panic!("Error expected string lit found {:?} at {}:{}", token.token_type, token.line, token.column);
                }
            }
            Word(Jpz) => {
                token = tokens.next();
                if let StringLit(ident) = token.token_type {
                    if let Some(index) = labels.get(&ident) {
                        let value = state.stack.pop().unwrap();
                        if value.is_zero() {
                            tokens.index = *index
                        }
                    } else {
                        panic!("Error: label {ident} does not exist yet is reffered to at {}:{}", token.line, token.column)
                    }
                } else {
                    panic!("Error expected string lit found {:?} at {}:{}", token.token_type, token.line, token.column);
                }
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
        Word(Add) => process_numerical_op(state, num_type, NumericeOp::Add),
        Word(Sub) => process_numerical_op(state, num_type, NumericeOp::Sub),
        Word(Mul) => process_numerical_op(state, num_type, NumericeOp::Mul),
        Word(Div) => process_numerical_op(state, num_type, NumericeOp::Div),
        Word(Rem) => process_numerical_op(state, num_type, NumericeOp::Rem),
        Word(And) => process_numerical_op(state, num_type, NumericeOp::And),
        Word(Or) => process_numerical_op(state, num_type, NumericeOp::Or),
        Word(Xor) => process_numerical_op(state, num_type, NumericeOp::Xor),
        Word(Eq) => process_numerical_op(state, num_type, NumericeOp::Eq),
        Word(Neq) => process_numerical_op(state, num_type, NumericeOp::Neq),
        Word(Gte) => process_numerical_op(state, num_type, NumericeOp::Gte),
        Word(Gt) => process_numerical_op(state, num_type, NumericeOp::Gt),
        Word(Lte) => process_numerical_op(state, num_type, NumericeOp::Lte),
        Word(Lt) => process_numerical_op(state, num_type, NumericeOp::Lt),
        Word(Max) => process_numerical_op(state, num_type, NumericeOp::Max),
        Word(Min) => process_numerical_op(state, num_type, NumericeOp::Min),
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

#[derive(Debug)]
enum NumericeOp {
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



macro_rules! perform_operation {
    (for int $type:ty, $operation:ident, $rhs:ident, $lhs:ident) => {
        match $operation {
            NumericeOp::Add => $lhs + $rhs,
            NumericeOp::Sub => $lhs - $rhs,
            NumericeOp::Mul => $lhs * $rhs,
            NumericeOp::Div => $lhs / $rhs,
            NumericeOp::Rem => $lhs % $rhs,
            NumericeOp::And => $lhs & $rhs,
            NumericeOp::Or => $lhs | $rhs,
            NumericeOp::Xor => $lhs ^ $rhs,
            NumericeOp::Eq => ($lhs == $rhs) as $type,
            NumericeOp::Neq => ($lhs != $rhs) as $type,
            NumericeOp::Gte => ($lhs >= $rhs) as $type,
            NumericeOp::Gt => ($lhs > $rhs) as $type,
            NumericeOp::Lte => ($lhs <= $rhs) as $type,
            NumericeOp::Lt => ($lhs < $rhs) as $type,
            NumericeOp::Max => $lhs.max($rhs),
            NumericeOp::Min => $lhs.min($rhs),
        }
    };
    (for float $type:ty, $operation:ident, $rhs:ident, $lhs:ident) => {
        match $operation {
            NumericeOp::Add => $lhs + $rhs,
            NumericeOp::Sub => $lhs - $rhs,
            NumericeOp::Mul => $lhs * $rhs,
            NumericeOp::Div => $lhs / $rhs,
            NumericeOp::Rem => $lhs % $rhs,
            NumericeOp::Eq => ($lhs == $rhs) as i32 as $type,
            NumericeOp::Neq => ($lhs != $rhs) as i32 as $type,
            NumericeOp::Gte => ($lhs >= $rhs) as i32 as $type,
            NumericeOp::Gt => ($lhs > $rhs) as i32 as $type,
            NumericeOp::Lte => ($lhs <= $rhs) as i32 as $type,
            NumericeOp::Lt => ($lhs < $rhs) as i32 as $type,
            NumericeOp::Max => $lhs.max($rhs),
            NumericeOp::Min => $lhs.min($rhs),
            operation => panic!("operation {operation:?} can not be used on a float")
        }
    };
}

fn process_numerical_op(state: &mut MachineState, node_type: NumericType, operation: NumericeOp) {
    let rhs = state
        .stack
        .pop()
        .expect("Tried popping from empty stack during numerical operation");
    let lhs = state
        .stack
        .pop()
        .expect("Tried popping from empty stack during numerical operation");

    match node_type {
        NumericType::I32 => {
            if let (StackValue::I32(rhs_val), StackValue::I32(lhs_val)) = (rhs, lhs) {
                state.stack.push(StackValue::I32(perform_operation!(
                    for int i32,
                    operation, rhs_val, lhs_val
                )))
            } else {
                panic!("tried applying operation but rhs was {rhs:?} and lhs was {lhs:?}")
            }
        }
        NumericType::I64 => {
            if let (StackValue::I64(rhs_val), StackValue::I64(lhs_val)) = (rhs, lhs) {
                state.stack.push(StackValue::I64(perform_operation!(
                    for int i64,
                    operation, rhs_val, lhs_val
                )))
            } else {
                panic!("tried applying operation but rhs was {rhs:?} and lhs was {lhs:?}")
            }
        }
        NumericType::I16 => {
            if let (StackValue::I16(rhs_val), StackValue::I16(lhs_val)) = (rhs, lhs) {
                state.stack.push(StackValue::I16(perform_operation!(
                    for int i16,
                    operation, rhs_val, lhs_val
                )))
            } else {
                panic!("tried applying operation but rhs was {rhs:?} and lhs was {lhs:?}")
            }
        }
        NumericType::I8 => {
            if let (StackValue::I8(rhs_val), StackValue::I8(lhs_val)) = (rhs, lhs) {
                state.stack.push(StackValue::I8(perform_operation!(
                    for int i8,
                    operation, rhs_val, lhs_val
                )))
            } else {
                panic!("tried applying operation but rhs was {rhs:?} and lhs was {lhs:?}")
            }
        }
        NumericType::F32 => {
            if let (StackValue::F32(rhs_val), StackValue::F32(lhs_val)) = (rhs, lhs) {
                state.stack.push(StackValue::F32(perform_operation!(
                    for float f32,
                    operation, rhs_val, lhs_val
                )))
            } else {
                panic!("tried applying operation but rhs was {rhs:?} and lhs was {lhs:?}")
            }
        }
        NumericType::F64 => {
            if let (StackValue::F64(rhs_val), StackValue::F64(lhs_val)) = (rhs, lhs) {
                state.stack.push(StackValue::F64(perform_operation!(
                    for float f64,
                    operation, rhs_val, lhs_val
                )))
            } else {
                panic!("tried applying operation but rhs was {rhs:?} and lhs was {lhs:?}")
            }
        }
        NumericType::I128 => {
            if let (StackValue::I64(rhs_val), StackValue::I64(lhs_val)) = (rhs, lhs) {
                state.stack.push(StackValue::I64(perform_operation!(
                    for int i64,
                    operation, rhs_val, lhs_val
                )))
            } else {
                panic!("tried applying operation but rhs was {rhs:?} and lhs was {lhs:?}")
            }
        }
    }

}