use core::panic;
use std::collections::HashMap;

use crate::tokenizer::Token;
use crate::tokenizer::{TokenType::*, WordToken::*};
use crate::Error;

const PAGE_SIZE: usize = 8 * 64 * 1024;
const INITIAL_PAGE_AMOUNT: usize = 1;

struct MachineState {
    functions: HashMap<String, usize>,
    exports: HashMap<String, usize>,
    function_stack: Vec<usize>,
    stack: Vec<StackValue>,
    variables: HashMap<String, StackValue>,
    memory: Memory
}

#[derive(Debug)]
pub enum MemoryErrors {
    PageFull,
    NotEnoughPages
}

#[derive(Debug)]
pub enum SyntaxError {
    Expected {
        expected_token: String,
        found: Token
    }
}
#[derive(Debug)]
pub enum SemanticError {
    UndefinedVar {
        ident: Token
    },
    UndefindLabel {
        ident: Token
    },
    UndefinedFunc {
        ident: Token
    },
    IncorrectOperation {
        operation: NumericeOp,
        val_type: String
    }
}
#[derive(Debug)]
pub enum RuntimeError {
    CastFromNull {
        token: Token
    },
    MismatchLhsRhs {
        lhs: StackValue,
        rhs: StackValue
    },
    PoppingEmptyStack {
        popped_into: Token
    }
}


#[derive(Debug, Clone)]
struct Page {
    data: [u8; PAGE_SIZE],
    free_data_map: Vec<(usize, usize)>    // size and pointer
}
impl Page {
    fn new() -> Self {
        let free_data_map = vec![(PAGE_SIZE, 0)];
        let data = [0; PAGE_SIZE]; 
        Self {
            data,
            free_data_map
        }
    }
    fn insert(&mut self, data: &[u8]) -> Result<(), MemoryErrors> {
        for (size, pointer) in &mut self.free_data_map {
            let data_size = data.len(); 
            if data_size <= *size {
                self.data[*pointer..*pointer+data_size].copy_from_slice(data);
                *size = *size - data_size ;
                *pointer = *pointer+data_size;
                return Ok(());
            }
        }
        Err(MemoryErrors::PageFull)

    }
    fn remove(&mut self, pointer: usize, size: usize) {
        self.free_data_map.push((size, pointer));
    }
}

// impl Display for Page {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let mut data = Vec::new();
//         for (size, pointer) in &self.free_data_map {
//             data.push((pointer, &self.data[*pointer as usize ..(pointer+size) as usize]))
//         }
//         write!(f, "Page:{:?}", data)
//     }
// }

#[derive(Debug)]
struct Memory {
    pages: Vec<Page>,
    // capacity_of_pages: Vec<Vec<(usize, usize)>>     // TODO: This can be done way better
}

impl Memory {
    fn new(number_of_pages: usize) -> Self {
        Self {
            pages: vec![Page::new(); number_of_pages],
            // capacity_of_pages: vec![vec![(PAGE_SIZE, 0)]; number_of_pages]
        }
    }
    fn insert(&mut self, data: &[u8]) -> Result<(), MemoryErrors> {
        for (page_index, page) in self.pages.clone().iter().enumerate() {
            let free_data_map = &page.free_data_map;
            let data_size = data.len(); 
            for (size, _) in free_data_map.clone() {
                if data_size <= size {
                    // *capacity -= data_size;
                    self.pages[page_index].insert(data)?;
                    return Ok(());
                }
            }
        }
        Err(MemoryErrors::NotEnoughPages)
    }

    fn grow(&mut self, page_amount: usize) {
        let mut new_pages = vec![Page::new(); page_amount];
        self.pages.append(&mut new_pages);
    }

    fn remove(&mut self, pointer: usize, size: usize) {
        let amount_of_pages = 1 + size/PAGE_SIZE;
        let first_page_index = (pointer + 1)/PAGE_SIZE;
        let last_page_index = first_page_index + amount_of_pages-1;
        let in_page_pointer = PAGE_SIZE % (pointer + 1);
        let first_page_amount;
        let last_page_amount ;
        if amount_of_pages == 1 {
            first_page_amount = size;
            last_page_amount = 0
        } else {
            first_page_amount = PAGE_SIZE - in_page_pointer;
            last_page_amount = amount_of_pages * PAGE_SIZE - first_page_amount - (amount_of_pages - 2) * PAGE_SIZE
        }
        // first page 
        self.pages[first_page_index].remove(in_page_pointer, first_page_amount);
        // middle pages
        for i in 1..amount_of_pages-1 {
            self.pages[i].remove(0x0, PAGE_SIZE);
        }
        // last page
        if last_page_amount > 0 {
            self.pages[last_page_index].remove(0, last_page_amount);
        }

    }
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
    fn to_bytes(&self) -> Vec<u8> {
        match self {
            StackValue::I128(val) => {
                let bindind =  &val.to_be_bytes().clone();
                bindind.to_vec()
            }
            StackValue::I64(val) => {
                let bindind =  &val.to_be_bytes().clone();
                bindind.to_vec()
            }
            StackValue::I32(val) => {
                let bindind =  &val.to_be_bytes().clone();
                bindind.to_vec()
            }
            StackValue::I16(val) => {
                let bindind =  &val.to_be_bytes().clone();
                bindind.to_vec()
            }
            StackValue::I8(val) => {
                let bindind =  &val.to_be_bytes().clone();
                bindind.to_vec()
            }
            StackValue::F32(val) => {
                let bindind =  &val.to_be_bytes().clone();
                bindind.to_vec()
            }
            StackValue::F64(val) => {
                let bindind =  &val.to_be_bytes().clone();
                bindind.to_vec()
            }
            StackValue::Null => todo!(),
        }
    }
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
            variables: HashMap::new(),
            memory: Memory::new(INITIAL_PAGE_AMOUNT)
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

pub fn run(tokens: &Vec<Token>) -> Result<(), Error>{
    let mut state = MachineState::new();
    let mut token_iter = TokenIter {
        tokens: tokens.clone(),
        index: 0
    };
    find_funcs(&mut token_iter, &mut state)?;
    state.functions.insert("printmemory".to_string(), 0);
    // state.functions.get(k)
    println!("funcs: {:?}", state.functions);
    println!("exported funcs: {:?}", state.exports);
    let start_func_index = state.exports.get("_start").unwrap().clone();
    run_func(&mut token_iter, &mut state, start_func_index)
}

fn run_func(tokens: &mut TokenIter, state: &mut MachineState, func_index: usize) -> Result<(), Error> {
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
            Word(Insert) => {
                token = tokens.next();
                match &token.token_type {
                    StringLit(string) => state.memory.insert(string.as_bytes())?,
                    Number(number) => state.memory.insert(&number.to_be_bytes())?,
                    CharLit(char) => {
                        let mut chars = String::from(*char);
                        while tokens.peek().token_type == Comma {
                            tokens.next();
                            let char_token = tokens.next();
                            if let CharLit(next_char) = char_token.token_type {
                                chars.push(next_char);
                            } else {
                                return  Err(SyntaxError::Expected { expected_token: "char_lit".to_string(), found: char_token })?;
                            }
                        }
                        state.memory.insert(chars.as_bytes())?
                    }
                    _ => return Err(SyntaxError::Expected { expected_token: "number, string_lit of list of chars".to_string(), found: token })?
                };
            }
            Word(Grow) => {
                token = tokens.next();
                match &token.token_type {
                    Number(number) => state.memory.grow(*number as usize),
                    _ => return  Err(SyntaxError::Expected { expected_token: "number".to_string(), found: token })?
                }
            }
            Word(Remove) => {
                token = tokens.next();
                match &token.token_type {
                    Number(pointer) => {
                        let next_token = tokens.next();
                        match &next_token.token_type {
                            Number(size) => {
                                state.memory.remove(*pointer as usize, *size as usize);
                            }
                            _ => return Err(SyntaxError::Expected { expected_token: "number".to_string(), found: token })?
                        }
                    }
                    _ => return Err(SyntaxError::Expected { expected_token: "number".to_string(), found: token })?
                }
            }
            Word(Pop) => {
                token = tokens.next();
                match &token.token_type {
                    Word(Mem) => {
                        if let Some(value) = state.stack.pop() {
                            state.memory.insert(&value.to_bytes())?
                        }
                    }
                    _ => todo!("add ability to pop into variables")
                }
            }
            Word(Call) => {
                run_call(tokens, state)?
            }
            Word(Return) => {
                break;
            }
            Word(Jmp) => {
                token = tokens.next();
                if let StringLit(ref ident) = token.token_type {
                    if let Some(index) = labels.get(ident) {
                        let value = state.stack.pop().unwrap();
                        if !value.is_zero() {
                            tokens.index = *index
                        }
                    } else {
                        return Err(SyntaxError::Expected { expected_token: "string_lit".to_string(), found: token })?
                    }
                } else {
                    return Err(SyntaxError::Expected { expected_token: "string_lit".to_string(), found: token })?
                }
            }
            Word(Jpz) => {
                token = tokens.next();
                if let StringLit(ref ident) = token.token_type {
                    if let Some(index) = labels.get(ident) {
                        let value = state.stack.pop().unwrap();
                        if value.is_zero() {
                            tokens.index = *index
                        }
                    } else {
                        return Err(SemanticError::UndefindLabel { ident: token })?
                    }
                } else {
                    return Err(SyntaxError::Expected { expected_token: "string_lit".to_string(), found: token })?
                }
            }
            Word(I128) => {
                run_numeric_instruction(tokens, state, NumericType::I128)?
            }
            Word(I64) => {
                run_numeric_instruction(tokens, state, NumericType::I64)?
            }
            Word(I32) => {
                run_numeric_instruction(tokens, state, NumericType::I32)?
            }
            Word(I16) => {
                run_numeric_instruction(tokens, state, NumericType::I16)?
            }
            Word(I8) => {
                run_numeric_instruction(tokens, state, NumericType::I8)?
            }
            Word(F64) => {
                run_numeric_instruction(tokens, state, NumericType::F64)?
            }
            Word(F32) => {
                run_numeric_instruction(tokens, state, NumericType::F32)?
            }
            Word(Declare) => {
                run_variable_decleration(tokens, state)?
            }
            Word(Set) => {
                run_variable_set(tokens, state)?
            }
            Word(Get) => {
                run_variable_get(tokens, state)?
            }
            Word(Cast) => {
                token = tokens.next();
                match token.token_type {
                    Word(I32) => {
                        if let Some(value) = state.stack.pop() {
                            state.stack.push(StackValue::I32(match value {
                                StackValue::F32(val) => val as i32,
                                StackValue::F64(val) => val as i32,
                                StackValue::I128(val) => val as i32,
                                StackValue::I64(val) => val as i32,
                                StackValue::I32(val) => val as i32,
                                StackValue::I16(val) => val as i32,
                                StackValue::I8(val) => val as i32,
                                StackValue::Null => return Err(RuntimeError::CastFromNull { token: token })?
                            }));
                        }
                    },
                    Word(I64) => {
                        if let Some(value) = state.stack.pop() {
                            state.stack.push(StackValue::I64(match value {
                                StackValue::F32(val) => val as i64,
                                StackValue::F64(val) => val as i64,
                                StackValue::I128(val) => val as i64,
                                StackValue::I64(val) => val as i64,
                                StackValue::I32(val) => val as i64,
                                StackValue::I16(val) => val as i64,
                                StackValue::I8(val) => val as i64,
                                StackValue::Null => return Err(RuntimeError::CastFromNull { token: token })?
                            }));
                        }
                    },
                    Word(I128) => {
                        if let Some(value) = state.stack.pop() {
                            state.stack.push(StackValue::I128(match value {
                                StackValue::F32(val) => val as i128,
                                StackValue::F64(val) => val as i128,
                                StackValue::I128(val) => val as i128,
                                StackValue::I64(val) => val as i128,
                                StackValue::I32(val) => val as i128,
                                StackValue::I16(val) => val as i128,
                                StackValue::I8(val) => val as i128,
                                StackValue::Null => return Err(RuntimeError::CastFromNull { token: token })?
                            }));
                        }
                    },
                    Word(I16) => {
                        if let Some(value) = state.stack.pop() {
                            state.stack.push(StackValue::I16(match value {
                                StackValue::F32(val) => val as i16,
                                StackValue::F64(val) => val as i16,
                                StackValue::I128(val) => val as i16,
                                StackValue::I64(val) => val as i16,
                                StackValue::I32(val) => val as i16,
                                StackValue::I16(val) => val as i16,
                                StackValue::I8(val) => val as i16,
                                StackValue::Null => return Err(RuntimeError::CastFromNull { token: token })?
                            }));
                        }
                    },
                    Word(I8) => {
                        if let Some(value) = state.stack.pop() {
                            state.stack.push(StackValue::I8(match value {
                                StackValue::F32(val) => val as i8,
                                StackValue::F64(val) => val as i8,
                                StackValue::I128(val) => val as i8,
                                StackValue::I64(val) => val as i8,
                                StackValue::I32(val) => val as i8,
                                StackValue::I16(val) => val as i8,
                                StackValue::I8(val) => val as i8,
                                StackValue::Null => return Err(RuntimeError::CastFromNull { token: token })?
                            }));
                        }
                    },
                    Word(F32) => {
                        if let Some(value) = state.stack.pop() {
                            state.stack.push(StackValue::F32(match value {
                                StackValue::F32(val) => val as f32,
                                StackValue::F64(val) => val as f32,
                                StackValue::I128(val) => val as f32,
                                StackValue::I64(val) => val as f32,
                                StackValue::I32(val) => val as f32,
                                StackValue::I16(val) => val as f32,
                                StackValue::I8(val) => val as f32,
                                StackValue::Null => return Err(RuntimeError::CastFromNull { token: token })?
                            }));
                        }
                    },
                    Word(F64) => {
                        if let Some(value) = state.stack.pop() {
                            state.stack.push(StackValue::F64(match value {
                                StackValue::F32(val) => val as f64,
                                StackValue::F64(val) => val as f64,
                                StackValue::I128(val) => val as f64,
                                StackValue::I64(val) => val as f64,
                                StackValue::I32(val) => val as f64,
                                StackValue::I16(val) => val as f64,
                                StackValue::I8(val) => val as f64,
                                StackValue::Null => return Err(RuntimeError::CastFromNull { token: token })?
                            }));
                        }
                    },
                    _ =>return  Err(SyntaxError::Expected { expected_token: "type".to_string(), found: token })?
                }
            }
            _ => return Err(SyntaxError::Expected { expected_token: "function instruction".to_string(), found: token })?
        }
    }
    if state.function_stack.len() >= 1 {
        tokens.index = state.function_stack.pop().unwrap()
    } else {
        // let top_of_stack = state.stack.pop().unwrap();
        println!("value of stack is: {:?}", state.stack)
    }
    Ok(())
}

#[derive(Debug)]
enum NumericType {
    I128,
    I64,
    I32,
    I16,
    I8,
    F32,
    F64

}

fn run_variable_decleration(tokens: &mut TokenIter, state: &mut MachineState) -> Result<(), Error> {
    while match tokens.peek().token_type {
        VarIdent(_) => true,
        _ => false
    } {
        let token = tokens.next();
        if let VarIdent(ident) = token.token_type {
            state.variables.insert(ident.clone(), StackValue::Null);
        } else {
            return Err(SyntaxError::Expected { expected_token: "variable ident".to_string(), found: token })?
        }
    }
    Ok(())
}

fn run_variable_set(tokens: &mut TokenIter, state: &mut MachineState) -> Result<(), Error> {
    let token = tokens.next(); 
    match &token.token_type {
        VarIdent(ident) => {
            if let Some(value) = state.stack.pop() {
                if let Some(var) = state.variables.get_mut(ident) {
                    *var = value;
                    Ok(())
                } else {
                    Err(SemanticError::UndefinedVar { ident: token })?
                }
            } else {
                Err(RuntimeError::PoppingEmptyStack { popped_into: token })?
            }
        }
        _ => Err(SyntaxError::Expected { expected_token: "variable".to_string(), found: token })?
    }
}

fn run_variable_get(tokens: &mut TokenIter, state: &mut MachineState) -> Result<(), Error> {
    let token = tokens.next();
    match &token.token_type {
        VarIdent(ident) => {
            if let Some(var) = state.variables.get(ident) {
                state.stack.push(*var);
                Ok(())
            } else {
                return Err(SemanticError::UndefinedVar { ident: token })?
            }
        }
        _ => return Err(SemanticError::UndefinedVar { ident: token })?
    }
}

fn run_numeric_instruction(tokens: &mut TokenIter, state: &mut MachineState, num_type: NumericType) -> Result<(), Error> {
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
                                return Err(SyntaxError::Expected { expected_token: "full stop for float".to_string(), found: token })?
                            }
                            token = tokens.next();
                            match token.token_type {
                                Number(num_after_decimal) => {
                                    let float: f32 = format!("{num}.{num_after_decimal}").parse().unwrap();
                                    state.stack.push(StackValue::F32(float))
                                } 
                                _ => return Err(SyntaxError::Expected { expected_token: "number".to_string(), found: token })?
                            }
                        },
                        NumericType::F64 => {
                            token = tokens.next();
                            if token.token_type != FullStop {
                                return Err(SyntaxError::Expected { expected_token: "full stop for float".to_string(), found: token })?
                            }
                            token = tokens.next();
                            match token.token_type {
                                Number(num_after_decimal) => {
                                    let float: f64 = format!("{num}.{num_after_decimal}").parse().unwrap();
                                    state.stack.push(StackValue::F64(float))
                                }
                                _ => return Err(SyntaxError::Expected { expected_token: "number".to_string(), found: token })?
                            }
                        },
                    }
                }
                _ => return Err(SyntaxError::Expected { expected_token: "number".to_string(), found: token })?

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
                                    return Err(SyntaxError::Expected { expected_token: "full stop for float".to_string(), found: token })?
                                }
                                token = tokens.next();
                                match token.token_type {
                                    Number(num_after_decimal) => {
                                        let float: f32 = format!("{num}.{num_after_decimal}").parse().unwrap();
                                        state.stack.push(StackValue::F32(float))
                                    } 
                                    _ => return Err(SyntaxError::Expected { expected_token: "number".to_string(), found: token })?
                                }
                            },
                            NumericType::F64 => {
                                token = tokens.next();
                                if token.token_type != FullStop {
                                    return Err(SyntaxError::Expected { expected_token: "full stop for float".to_string(), found: token })?
                                }
                                token = tokens.next();
                                match token.token_type {
                                    Number(num_after_decimal) => {
                                        let float: f64 = format!("{num}.{num_after_decimal}").parse().unwrap();
                                        state.stack.push(StackValue::F64(float))
                                    }
                                    _ => return Err(SyntaxError::Expected { expected_token: "number".to_string(), found: token })?
                                }
                            },
                        }
                    }
                    _ => panic!("This should be unreachable")
                }
            }
            Ok(())
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
        _ => Err(SyntaxError::Expected { expected_token: "numeric operation".to_string(), found: token })?
    }
}

fn run_call(tokens: &mut TokenIter, state: &mut MachineState) -> Result<(), Error> {
    let token = tokens.next();
    let call_site_index = tokens.index;
    match token.token_type {
        FuncIdent(ref ident) => {
            if let Some(func_index) = state.functions.get(ident) {
                tokens.index = call_site_index;
                state.function_stack.push(call_site_index);
                if *ident == "printmemory".to_string() {
                    print_memory(state);
                    Ok(())
                } else {
                    run_func(tokens, state, *func_index)
                }
            } else {
                return Err(SemanticError::UndefinedFunc { ident: token })?
            }
        }
        _ => return Err(SyntaxError::Expected { expected_token: "function ident".to_string(), found: token })?
    }
}
fn print_memory(state: &mut MachineState) {
    println!("Memory: {:?}", state.memory)
}

fn find_funcs(tokens: &mut TokenIter, state: &mut MachineState) -> Result<(), Error> {
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
                // wrong_token => return Err(format!("Syntax Error: Expected function ident found {wrong_token:?} at {}:{}", token.line, token.column))
                _ => return Err(Error::SyntacError(SyntaxError::Expected { expected_token: "function ident".to_string(), found: token }))

            }
            token = tokens.next();
            match token.token_type {
                StringLit(lit) => {
                    func_export = Some(lit);
                    token = tokens.next();
                    match token.token_type {
                        Colon => func_index = tokens.index,
                        _ => return Err(SyntaxError::Expected { expected_token: "collon or export".to_string(), found: token })?
                    }
                },
                Colon => func_index = tokens.index,
                _ => return Err(SyntaxError::Expected { expected_token: "collon or export".to_string(), found: token })?

            }
            state.functions.insert(func_ident.clone(), func_index);
            if let Some(export) = func_export {
                state.exports.insert(export, func_index);
            }
        }
        EOF => {
            tokens.reset();
            return Ok(());
        },
        _ => {}
    }
    tokens.next();
    find_funcs(tokens, state)?;
    Ok(())
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
            operation => return Err(SemanticError::IncorrectOperation{
                operation: operation,
                val_type: "float".to_string()
            })?
        }
    };
}

fn process_numerical_op(state: &mut MachineState, node_type: NumericType, operation: NumericeOp) -> Result<(), Error> {
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
                return Err(RuntimeError::MismatchLhsRhs { lhs: lhs, rhs: rhs })?
            }
        }
        NumericType::I64 => {
            if let (StackValue::I64(rhs_val), StackValue::I64(lhs_val)) = (rhs, lhs) {
                state.stack.push(StackValue::I64(perform_operation!(
                    for int i64,
                    operation, rhs_val, lhs_val
                )))
            } else {
                return Err(RuntimeError::MismatchLhsRhs { lhs: lhs, rhs: rhs })?
            }
        }
        NumericType::I16 => {
            if let (StackValue::I16(rhs_val), StackValue::I16(lhs_val)) = (rhs, lhs) {
                state.stack.push(StackValue::I16(perform_operation!(
                    for int i16,
                    operation, rhs_val, lhs_val
                )))
            } else {
                return Err(RuntimeError::MismatchLhsRhs { lhs: lhs, rhs: rhs })?
            }
        }
        NumericType::I8 => {
            if let (StackValue::I8(rhs_val), StackValue::I8(lhs_val)) = (rhs, lhs) {
                state.stack.push(StackValue::I8(perform_operation!(
                    for int i8,
                    operation, rhs_val, lhs_val
                )))
            } else {
                return Err(RuntimeError::MismatchLhsRhs { lhs: lhs, rhs: rhs })?
            }
        }
        NumericType::F32 => {
            if let (StackValue::F32(rhs_val), StackValue::F32(lhs_val)) = (rhs, lhs) {
                state.stack.push(StackValue::F32(perform_operation!(
                    for float f32,
                    operation, rhs_val, lhs_val
                )))
            } else {
                return Err(RuntimeError::MismatchLhsRhs { lhs: lhs, rhs: rhs })?
            }
        }
        NumericType::F64 => {
            if let (StackValue::F64(rhs_val), StackValue::F64(lhs_val)) = (rhs, lhs) {
                state.stack.push(StackValue::F64(perform_operation!(
                    for float f64,
                    operation, rhs_val, lhs_val
                )))
            } else {
                return Err(RuntimeError::MismatchLhsRhs { lhs: lhs, rhs: rhs })?
            }
        }
        NumericType::I128 => {
            if let (StackValue::I64(rhs_val), StackValue::I64(lhs_val)) = (rhs, lhs) {
                state.stack.push(StackValue::I64(perform_operation!(
                    for int i64,
                    operation, rhs_val, lhs_val
                )))
            } else {
                return Err(RuntimeError::MismatchLhsRhs { lhs: lhs, rhs: rhs })?
            }
        }
    }
    Ok(())

}