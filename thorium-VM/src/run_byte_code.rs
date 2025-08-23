use core::{num, panic};
use hashbrown::HashMap;
use std::mem::transmute;

use crate::Error;
use crate::tokenizer::Token;
use crate::tokenizer::{TokenType::*, WordToken::*};

const PAGE_SIZE: usize = 8 * 64 * 1024;
const INITIAL_PAGE_AMOUNT: usize = 1;

struct MachineState {
    functions: HashMap<String, usize>,
    exports: HashMap<String, usize>,
    function_stack: Vec<usize>,
    stack: Vec<u8>,
    variable_map: HashMap<String, usize>,
    variables: Vec<i128>,
    memory: Memory,
}

#[derive(Debug)]
pub enum MemoryErrors {
    PageFull,
    NotEnoughPages,
}

#[derive(Debug)]
pub enum SyntaxError {
    Expected {
        expected_token: String,
        found: Token,
    },
}
#[derive(Debug)]
pub enum SemanticError {
    UndefinedVar {
        ident: Token,
    },
    UndefindLabel {
        ident: Token,
    },
    UndefinedFunc {
        ident: Token,
    },
    IncorrectOperation {
        operation: NumericeOp,
        val_type: String,
    },
}
#[derive(Debug)]
pub enum RuntimeError {
    CastFromNull { token: Token },
    MismatchLhsRhs { lhs: StackValue, rhs: StackValue },
    PoppingEmptyStack { popped_into: Token },
    NoValueFoundAtStackIndex { index: usize, token: Token },
    TriedIndexingWithFloat { token: Token },
}

#[derive(Debug, Clone)]
struct Page {
    data: Box<[u8; PAGE_SIZE]>,
    free_data_map: Vec<(usize, usize)>, // size and pointer
}
impl Page {
    fn new() -> Self {
        let free_data_map = vec![(PAGE_SIZE, 0)];
        let data = Box::new([0; PAGE_SIZE]);
        Self {
            data,
            free_data_map,
        }
    }
    fn insert(&mut self, data: &[u8]) -> Result<(), MemoryErrors> {
        for (size, pointer) in &mut self.free_data_map {
            let data_size = data.len();
            if data_size <= *size {
                self.data[*pointer..*pointer + data_size].copy_from_slice(data);
                *size = *size - data_size;
                *pointer = *pointer + data_size;
                return Ok(());
            }
        }
        Err(MemoryErrors::PageFull)
    }
    fn copy(&mut self, pointer: usize, size: usize) -> &[u8] {
        let data = &self.data[pointer..pointer + size];
        data
    }

    fn remove(&mut self, pointer: usize, size: usize) {
        self.free_data_map.push((size, pointer));
    }
}

#[derive(Debug)]
struct Memory {
    pages: Vec<Page>,
    // capacity_of_pages: Vec<Vec<(usize, usize)>>     // TODO: This can be done way better
}

impl Memory {
    fn new(number_of_pages: usize) -> Self {
        let pages = vec![Page::new(); number_of_pages];
        Self { pages }
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

    fn copy(&mut self, pointer: usize, size: usize) -> &[u8] {
        let amount_of_pages = 1 + size / PAGE_SIZE;
        let page_index = (pointer + 1) / PAGE_SIZE;
        let in_page_pointer = PAGE_SIZE % (pointer + 1);

        let data = self.pages[page_index].copy(in_page_pointer, size);
        data
    }

    fn remove(&mut self, pointer: usize, size: usize) {
        let amount_of_pages = 1 + size / PAGE_SIZE;
        let first_page_index = (pointer + 1) / PAGE_SIZE;
        let last_page_index = first_page_index + amount_of_pages - 1;
        let in_page_pointer = PAGE_SIZE % (pointer + 1);
        let first_page_amount;
        let last_page_amount;
        if amount_of_pages == 1 {
            first_page_amount = size;
            last_page_amount = 0
        } else {
            first_page_amount = PAGE_SIZE - in_page_pointer;
            last_page_amount =
                amount_of_pages * PAGE_SIZE - first_page_amount - (amount_of_pages - 2) * PAGE_SIZE
        }
        // first page
        self.pages[first_page_index].remove(in_page_pointer, first_page_amount);
        // middle pages
        for i in 1..amount_of_pages - 1 {
            self.pages[i].remove(0x0, PAGE_SIZE);
        }
        // last page
        if last_page_amount > 0 {
            self.pages[last_page_index].remove(0, last_page_amount);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StackValue {
    I128(i128),
    I64(i64),
    I32(i32),
    I16(i16),
    I8(i8),
    F32(f32),
    F64(f64),
    Null,
}

impl StackValue {
    fn to_bytes(&self) -> Vec<u8> {
        match self {
            StackValue::I128(val) => {
                let bindind = &val.to_be_bytes().clone();
                bindind.to_vec()
            }
            StackValue::I64(val) => {
                let bindind = &val.to_be_bytes().clone();
                bindind.to_vec()
            }
            StackValue::I32(val) => {
                let bindind = &val.to_be_bytes().clone();
                bindind.to_vec()
            }
            StackValue::I16(val) => {
                let bindind = &val.to_be_bytes().clone();
                bindind.to_vec()
            }
            StackValue::I8(val) => {
                let bindind = &val.to_be_bytes().clone();
                bindind.to_vec()
            }
            StackValue::F32(val) => {
                let bindind = &val.to_be_bytes().clone();
                bindind.to_vec()
            }
            StackValue::F64(val) => {
                let bindind = &val.to_be_bytes().clone();
                bindind.to_vec()
            }
            StackValue::Null => todo!(),
        }
    }
    fn as_usize(self) -> usize {
        match self {
            StackValue::I128(value) => value as usize,
            StackValue::I64(value) => value as usize,
            StackValue::I32(value) => value as usize,
            StackValue::I16(value) => value as usize,
            StackValue::I8(value) => value as usize,
            StackValue::F32(value) => value as usize,
            StackValue::F64(value) => value as usize,
            StackValue::Null => 0,
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
            variable_map: HashMap::new(),
            variables: Vec::new(),
            memory: Memory::new(INITIAL_PAGE_AMOUNT),
        }
    }
}

struct TokenIter {
    // tokens: Vec<Token>,
    index: usize,
}
impl TokenIter {
    fn next<'a>(&mut self, tokens: &'a Vec<Token>) -> &'a Token {
        self.index += 1;
        &tokens[self.index]
    }
    fn current<'a>(&mut self, tokens: &'a Vec<Token>) -> &'a Token {
        &tokens[self.index]
    }
    fn reset(&mut self) {
        self.index = 0
    }
    fn peek<'a>(&mut self, tokens: &'a Vec<Token>) -> &'a Token {
        &tokens[self.index + 1]
    }
}

pub fn run(tokens: &mut Vec<Token>) -> Result<(Vec<u8>, HashMap<String, usize>, Vec<i128>), Error> {
    let mut state = MachineState::new();
    let mut token_iter = TokenIter { index: 0 };
    find_funcs(&mut token_iter, &mut state, tokens)?;
    state.functions.insert("printmemory".to_string(), 0);
    println!("funcs: {:?}", state.functions);
    println!("exported funcs: {:?}", state.exports);
    let start_func_index = state.exports.get("_start").unwrap().clone();
    run_func(&mut token_iter, &mut state, start_func_index, tokens)?;
    Ok((state.stack, state.variable_map, state.variables))
}

fn run_func(
    tokens_iter: &mut TokenIter,
    state: &mut MachineState,
    func_index: usize,
    tokens: &mut Vec<Token>,
) -> Result<(), Error> {
    tokens_iter.index = func_index;
    let mut labels = HashMap::new();
    // finding labels
    while match tokens_iter.peek(tokens).token_type {
        Word(EndFunc) => false,
        _ => true,
    } {
        let token = tokens_iter.next(tokens);
        if let LabelIdent(ident) = &token.token_type {
            labels.insert(ident.clone(), tokens_iter.index);
        }
    }
    tokens_iter.index = func_index;
    while match tokens_iter.peek(tokens).token_type {
        Word(EndFunc) => false,
        _ => true,
    } {
        let mut token = tokens_iter.next(tokens);
        match token.token_type {
            LabelIdent(_) => {}
            Word(Insert) => {
                token = tokens_iter.next(tokens);
                match &token.token_type {
                    StringLit(string) => state.memory.insert(string.as_bytes())?,
                    // Number(number) => state.memory.insert(&number.parse().unwrap().to_be_bytes())?,
                    Number(num) => {
                        token = tokens_iter.next(tokens);
                        match token.token_type {
                            Word(I8) => state.memory.insert(&(*num as i8).to_be_bytes())?,
                            Word(I16) => state.memory.insert(&(*num as i16).to_be_bytes())?,
                            Word(I32) => state.memory.insert(&(*num as i32).to_be_bytes())?,
                            Word(I64) => state.memory.insert(&(*num as i64).to_be_bytes())?,
                            Word(F32) => state.memory.insert(&(*num as f32).to_be_bytes())?,
                            Word(F64) => state.memory.insert(&(*num as f64).to_be_bytes())?,
                            _ => panic!("somehow the tokenizer had an invalid number type"),
                        }
                        // num.
                        // if let Ok(int) = num.into() {
                        //     int8 = int;
                        //     state.memory.insert(&int8.to_be_bytes())?;
                        // } else if let Ok(int) = num.into() {
                        //     int16 = int;
                        //     state.memory.insert(&int16.to_be_bytes())?;
                        // } else if let Ok(int) = num.into() {
                        //     int32 = int;
                        //     state.memory.insert(&int32.to_be_bytes())?;
                        // } else if let Ok(int) = num.into() {
                        //     int64 = int;
                        //     state.memory.insert(&int64.to_be_bytes())?;
                        // } else if let Ok(float) = num.into() {
                        //     float32 = float;
                        //     state.memory.insert(&float32.to_be_bytes())?;
                        // } else if let Ok(float) = num.into() {
                        //     float64 = float;
                        //     state.memory.insert(&float64.to_be_bytes())?;
                        // } else {
                        //     panic!("somehow the tokenizer had an invalid number type")
                        // }
                    }
                    CharLit(char) => {
                        let mut chars = String::from(*char);
                        while tokens_iter.peek(tokens).token_type == Comma {
                            tokens_iter.next(tokens);
                            let char_token = tokens_iter.next(tokens);
                            if let CharLit(next_char) = char_token.token_type {
                                chars.push(next_char);
                            } else {
                                return Err(SyntaxError::Expected {
                                    expected_token: "char_lit".to_string(),
                                    found: char_token.clone(),
                                })?;
                            }
                        }
                        state.memory.insert(chars.as_bytes())?
                    }
                    _ => {
                        return Err(SyntaxError::Expected {
                            expected_token: "number, string_lit of list of chars".to_string(),
                            found: token.clone(),
                        })?;
                    }
                };
            }
            Word(Grow) => {
                token = tokens_iter.next(tokens);
                match &token.token_type {
                    Number(amount) => {
                        state.memory.grow(*amount as usize);
                    }
                    _ => {
                        return Err(SyntaxError::Expected {
                            expected_token: "number".to_string(),
                            found: token.clone(),
                        })?;
                    }
                }
            }
            Word(Remove) => {
                token = tokens_iter.next(tokens);
                match &token.token_type {
                    Number(pointer) => {
                        let next_token = tokens_iter.next(tokens);
                        match &next_token.token_type {
                            Number(size) => {
                                let parsed_pointer;
                                let parsed_size;
                                parsed_pointer = *pointer as usize;
                                parsed_size = *size as usize;
                                state.memory.remove(parsed_pointer, parsed_size);
                            }
                            _ => {
                                return Err(SyntaxError::Expected {
                                    expected_token: "number".to_string(),
                                    found: token.clone(),
                                })?;
                            }
                        }
                    }
                    _ => {
                        return Err(SyntaxError::Expected {
                            expected_token: "number".to_string(),
                            found: token.clone(),
                        })?;
                    }
                }
            }
            // Word(Pop) => {
            //     token = tokens_iter.next(tokens);
            //     match &token.token_type {
            //         Word(Mem) => {
            //             if let Some(value) = state.stack.pop() {
            //                 state.memory.insert(unsafe {
            //                     transmute(value)
            //                 })?
            //             }
            //         }
            //         _ => todo!("add ability to pop into variables"),
            //     }
            // }
            Word(Call) => run_call(tokens_iter, state, tokens)?,
            Word(Return) => {
                break;
            }
            Word(Jmp) => {
                token = tokens_iter.next(tokens);
                match token.token_type {
                    StringLit(ref ident) => {
                        if let Some(index) = labels.get(ident) {
                            let value = state.stack.pop().unwrap();
                            tokens[tokens_iter.index].token_type = TokenIndex(*index);

                            if value != 0 {
                                tokens_iter.index = *index
                            }
                        } else {
                            return Err(SyntaxError::Expected {
                                expected_token: "string_lit".to_string(),
                                found: token.clone(),
                            })?;
                        }
                    }
                    TokenIndex(index) => {
                        let value = state.stack.pop().unwrap();
                        if value != 0 {
                            tokens_iter.index = index
                        }
                    }
                    _ => {
                        return Err(SyntaxError::Expected {
                            expected_token: "string_lit".to_string(),
                            found: token.clone(),
                        })?;
                    }
                }
            }
            Word(Jpz) => {
                token = tokens_iter.next(tokens);
                match token.token_type {
                    StringLit(ref ident) => {
                        if let Some(index) = labels.get(ident) {
                            let value = state.stack.pop().unwrap();
                            tokens[tokens_iter.index].token_type = TokenIndex(*index);
                            if value == 0 {
                                tokens_iter.index = *index
                            }
                        } else {
                            return Err(SyntaxError::Expected {
                                expected_token: "string_lit".to_string(),
                                found: token.clone(),
                            })?;
                        }
                    }
                    TokenIndex(index) => {
                        let value = state.stack.pop().unwrap();
                        if value == 0 {
                            tokens_iter.index = index
                        }
                    }
                    _ => {
                        return Err(SyntaxError::Expected {
                            expected_token: "string_lit".to_string(),
                            found: token.clone(),
                        })?;
                    }
                }
            }
            Word(I128) => run_numeric_instruction(tokens_iter, state, NumericType::I128, tokens)?,
            Word(I64) => run_numeric_instruction(tokens_iter, state, NumericType::I64, tokens)?,
            Word(I32) => run_numeric_instruction(tokens_iter, state, NumericType::I32, tokens)?,
            Word(I16) => run_numeric_instruction(tokens_iter, state, NumericType::I16, tokens)?,
            Word(I8) => run_numeric_instruction(tokens_iter, state, NumericType::I8, tokens)?,
            Word(F64) => run_numeric_instruction(tokens_iter, state, NumericType::F64, tokens)?,
            Word(F32) => run_numeric_instruction(tokens_iter, state, NumericType::F32, tokens)?,
            Word(Declare) => run_variable_decleration(tokens_iter, state, tokens)?,
            // Word(Set) => {
            //     let peeked = tokens_iter.peek(tokens);
            //     if let Word(Stack) = peeked.token_type {
            //         todo!("need to redo this to account for variables of different lenghts");
            //         tokens_iter.next(tokens);

            //         token = tokens_iter.next(tokens);
            //         let mut pointer: usize;
            //         match &token.token_type {
            //             Number(num) => pointer = *num as usize,
            //             VarIdent(ident) => {
            //                 if let Some(index) = state.variable_map.get(ident) {
            //                     let value = state.variables[*index];
            //                     pointer = value as usize;
            //                 } else {
            //                     return Err(SemanticError::UndefinedVar {
            //                         ident: token.clone(),
            //                     })?;
            //                 }
            //             }
            //             Word(Top) => pointer = state.stack.len() - 1,
            //             _ => {
            //                 return Err(SyntaxError::Expected {
            //                     expected_token: "number for pointer".to_string(),
            //                     found: token.clone(),
            //                 })?;
            //             }
            //         }
            //         let peeked = tokens_iter.peek(tokens);
            //         match peeked.token_type {
            //             Plus => {
            //                 tokens_iter.next(tokens);
            //                 token = tokens_iter.next(tokens);
            //                 match &token.token_type {
            //                     Number(num) => pointer += *num as usize,
            //                     Word(Top) => {
            //                         let top_of_stack = state.stack[state.stack.len() - 1] as usize;
            //                         pointer += top_of_stack
            //                     }
            //                     _ => {
            //                         return Err(SyntaxError::Expected {
            //                             expected_token: "number, or top".to_string(),
            //                             found: token.clone(),
            //                         })?;
            //                     }
            //                 }
            //             }
            //             Minus => {
            //                 tokens_iter.next(tokens);
            //                 token = tokens_iter.next(tokens);
            //                 match &token.token_type {
            //                     Number(num) => pointer -= *num as usize,
            //                     Word(Top) => {
            //                         let top_of_stack = state.stack[state.stack.len() - 1] as usize;
            //                         pointer -= top_of_stack
            //                     }
            //                     _ => {
            //                         return Err(SyntaxError::Expected {
            //                             expected_token: "number, or top".to_string(),
            //                             found: token.clone(),
            //                         })?;
            //                     }
            //                 }
            //             }
            //             _ => {}
            //         }
            //         if let Some(value) = state.stack.pop() {
            //             state.stack[pointer] = value
            //         } else {
            //             return Err(RuntimeError::PoppingEmptyStack {
            //                 popped_into: token.clone(),
            //             })?;
            //         }
            //     } else {
            //         run_variable_set(tokens_iter, state, tokens)?
            //     }
            // }
            // Word(Get) => run_variable_get(tokens_iter, state, tokens)?,
            _ => {
                return Err(SyntaxError::Expected {
                    expected_token: "function instruction".to_string(),
                    found: token.clone(),
                })?;
            }
        }
    }
    if state.function_stack.len() >= 1 {
        tokens_iter.index = state.function_stack.pop().unwrap()
    } else {
        // let top_of_stack = state.stack.pop().unwrap();
        println!("variables: {:?}", state.variable_map);
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
    F64,
}

impl NumericType {
    fn size(&self) -> usize {
        match self {
            NumericType::I128 => size_of::<i128>(),
            NumericType::I64 => size_of::<i64>(),
            NumericType::I32 => size_of::<i32>(),
            NumericType::I16 => size_of::<i16>(),
            NumericType::I8 => size_of::<i8>(),
            NumericType::F32 => size_of::<f32>(),
            NumericType::F64 => size_of::<f64>(),
        }
    }
}

fn run_variable_decleration(
    tokens_iter: &mut TokenIter,
    state: &mut MachineState,
    tokens: &Vec<Token>,
) -> Result<(), Error> {
    while match tokens_iter.peek(tokens).token_type {
        VarIdent(_) => true,
        _ => false,
    } {
        let token = tokens_iter.next(tokens);
        if let VarIdent(ident) = &token.token_type {
            state.variables.push(0);
            state
                .variable_map
                .insert(ident.clone(), state.variables.len() - 1);
        } else {
            return Err(SyntaxError::Expected {
                expected_token: "variable ident".to_string(),
                found: token.clone(),
            })?;
        }
    }
    Ok(())
}

macro_rules! set_variable {
    ($state:ident, $token:ident, $tokens:ident, $tokens_iter:ident, $num_type:ty) => {{
        let value = $state
            .stack
            .last_chunk::<{ size_of::<$num_type>() }>()
            .unwrap()
            .clone();
        $state.stack.truncate(size_of::<$num_type>());
        match &$token.token_type {
            VarIdent(ident) => {
                if let Some(index) = $state.variable_map.get(ident) {
                    $tokens[$tokens_iter.index].token_type = TokenIndex(*index);

                    $state.variables[*index] = <$num_type>::from_ne_bytes(value) as i128;
                    Ok(())
                } else {
                    Err(SemanticError::UndefinedVar {
                        ident: $token.clone(),
                    }
                    .into())
                }
            }
            TokenIndex(index) => {
                $state.variables[*index] = <$num_type>::from_ne_bytes(value) as i128;
                Ok(())
            }
            _ => Err(SyntaxError::Expected {
                expected_token: "variable".to_string(),
                found: $token.clone(),
            }
            .into()),
        }
    }};
}

fn run_variable_set(
    tokens_iter: &mut TokenIter,
    state: &mut MachineState,
    tokens: &mut Vec<Token>,
    num_type: NumericType,
) -> Result<(), Error> {
    let token = tokens_iter.next(tokens);
    match num_type {
        NumericType::I128 => set_variable!(state, token, tokens, tokens_iter, i128),
        NumericType::I64 => set_variable!(state, token, tokens, tokens_iter, i64),
        NumericType::I32 => set_variable!(state, token, tokens, tokens_iter, i32),
        NumericType::I16 => set_variable!(state, token, tokens, tokens_iter, i16),
        NumericType::I8 => set_variable!(state, token, tokens, tokens_iter, i8),
        NumericType::F32 => set_variable!(state, token, tokens, tokens_iter, f32),
        NumericType::F64 => set_variable!(state, token, tokens, tokens_iter, f64),
    }
    // let value = state
    //     .stack
    //     .pop()
    //     .ok_or_else(|| RuntimeError::PoppingEmptyStack {
    //         popped_into: token.clone(),
    //     })?;
    // let value = state.stack.last_chunk::<{size}>().unwrap();
    // match &token.token_type {
    //     VarIdent(ident) => {
    //         if let Some(index) = state.variable_map.get(ident) {
    //             tokens[tokens_iter.index].token_type = TokenIndex(*index);
    //             state.variables[*index] = value;
    //             Ok(())
    //         } else {
    //             Err(SemanticError::UndefinedVar {
    //                 ident: token.clone(),
    //             }
    //             .into())
    //         }
    //     }
    //     TokenIndex(index) => {
    //         state.variables[*index] = value;
    //         Ok(())
    //     }
    //     _ => Err(SyntaxError::Expected {
    //         expected_token: "variable".to_string(),
    //         found: token.clone(),
    //     }
    //     .into()),
    // }
}

fn run_variable_get(
    tokens_iter: &mut TokenIter,
    state: &mut MachineState,
    tokens: &mut Vec<Token>,
    num_type: &NumericType
) -> Result<(), Error> {
    let token = tokens_iter.next(tokens);
    let index = match &token.token_type {
        VarIdent(ident) => {
            if let Some(index) = state.variable_map.get(ident) {
                tokens[tokens_iter.index].token_type = TokenIndex(*index);
                // state.stack.push(state.variables[*index]);
                index
                // Ok(())
            } else {
                return Err(SemanticError::UndefinedVar {
                    ident: token.clone(),
                })?;
            }
        }
        TokenIndex(index) => {
            // state.stack.push(state.variables[*index]);
            index
            // Ok(())
        }
        _ => {
            return Err(SemanticError::UndefinedVar {
                ident: token.clone(),
            })?;
        }
    };
    match num_type {
        NumericType::I128 => {let value = (state.variables[*index] as i128).to_ne_bytes();
        for i in value {
            state.stack.push(i);
        }
        } ,
        NumericType::I64 => {let value = (state.variables[*index] as i64).to_ne_bytes();
        for i in value {
            state.stack.push(i);
        }
        } ,
        NumericType::I32 => {let value = (state.variables[*index] as i32).to_ne_bytes();
        for i in value {
            state.stack.push(i);
        }
        } ,
        NumericType::I16 => {let value = (state.variables[*index] as i16).to_ne_bytes();
        for i in value {
            state.stack.push(i);
        }
        } ,
        NumericType::I8 => {let value = (state.variables[*index] as i8).to_ne_bytes();
        for i in value {
            state.stack.push(i);
        }
        } ,
        NumericType::F32 => {let value = (state.variables[*index] as f32).to_ne_bytes();
        for i in value {
            state.stack.push(i);
        }
        } ,
        NumericType::F64 => {let value = (state.variables[*index] as f64).to_ne_bytes();
        for i in value {
            state.stack.push(i);
        }
        } ,
    };
    // state.stack.push(state.variables[*index]);

    Ok(())
}

macro_rules! push_numeric {
    ($state:ident, $num:ident, $num_type:ty) => {
        $state.stack.extend_from_slice(&($num as $num_type).to_ne_bytes());
    };
}

macro_rules! insert_memory {
    ($state:ident, $num_type:ident) => {
    match $num_type {
            NumericType::I128 => {
                let popped = $state.stack.last_chunk::<{size_of::<i128>()}>().unwrap();
                $state.memory.insert(popped)?;
                $state.stack.truncate($state.stack.len() - size_of::<i128>())
            }
            NumericType::I64 => {
                let popped = $state.stack.last_chunk::<{size_of::<i64>()}>().unwrap();
                $state.memory.insert(popped)?;
                $state.stack.truncate($state.stack.len() - size_of::<i64>())
            }
            NumericType::I32 => {
                let popped = $state.stack.last_chunk::<{size_of::<i32>()}>().unwrap();
                $state.memory.insert(popped)?;
                $state.stack.truncate($state.stack.len() - size_of::<i32>())
            }
            NumericType::I16 => {
                let popped = $state.stack.last_chunk::<{size_of::<i16>()}>().unwrap();
                $state.memory.insert(popped)?;
                $state.stack.truncate($state.stack.len() - size_of::<i16>())
            }
            NumericType::I8 => {
                let popped = $state.stack.last_chunk::<{size_of::<i8>()}>().unwrap();
                $state.memory.insert(popped)?;
                $state.stack.truncate($state.stack.len() - size_of::<i8>())
            }
            NumericType::F32 => {
                let popped = $state.stack.last_chunk::<{size_of::<f32>()}>().unwrap();
                $state.memory.insert(popped)?;
                $state.stack.truncate($state.stack.len() - size_of::<f32>())
            }
            NumericType::F64 => {
                let popped = $state.stack.last_chunk::<{size_of::<f64>()}>().unwrap();
                $state.memory.insert(popped)?;
                $state.stack.truncate($state.stack.len() - size_of::<f64>())
            }
        }
    };
}
fn run_numeric_instruction(
    tokens_iter: &mut TokenIter,
    state: &mut MachineState,
    num_type: NumericType,
    tokens: &mut Vec<Token>,
) -> Result<(), Error> {
    let mut token = tokens_iter.next(tokens);
    match token.token_type {
        Word(Set) => {
            let peeked = tokens_iter.peek(tokens);
            if let Word(Stack) = peeked.token_type {
                todo!("need to redo this to account for variables of different lenghts");
                tokens_iter.next(tokens);

                token = tokens_iter.next(tokens);
                let mut pointer: usize;
                match &token.token_type {
                    Number(num) => pointer = *num as usize,
                    VarIdent(ident) => {
                        if let Some(index) = state.variable_map.get(ident) {
                            let value = state.variables[*index];
                            pointer = value as usize;
                        } else {
                            return Err(SemanticError::UndefinedVar {
                                ident: token.clone(),
                            })?;
                        }
                    }
                    Word(Top) => pointer = state.stack.len() - 1,
                    _ => {
                        return Err(SyntaxError::Expected {
                            expected_token: "number for pointer".to_string(),
                            found: token.clone(),
                        })?;
                    }
                }
                let peeked = tokens_iter.peek(tokens);
                match peeked.token_type {
                    Plus => {
                        tokens_iter.next(tokens);
                        token = tokens_iter.next(tokens);
                        match &token.token_type {
                            Number(num) => pointer += *num as usize,
                            Word(Top) => {
                                let top_of_stack = state.stack[state.stack.len() - 1] as usize;
                                pointer += top_of_stack
                            }
                            _ => {
                                return Err(SyntaxError::Expected {
                                    expected_token: "number, or top".to_string(),
                                    found: token.clone(),
                                })?;
                            }
                        }
                    }
                    Minus => {
                        tokens_iter.next(tokens);
                        token = tokens_iter.next(tokens);
                        match &token.token_type {
                            Number(num) => pointer -= *num as usize,
                            Word(Top) => {
                                let top_of_stack = state.stack[state.stack.len() - 1] as usize;
                                pointer -= top_of_stack
                            }
                            _ => {
                                return Err(SyntaxError::Expected {
                                    expected_token: "number, or top".to_string(),
                                    found: token.clone(),
                                })?;
                            }
                        }
                    }
                    _ => {}
                }
                if let Some(value) = state.stack.pop() {
                    state.stack[pointer] = value;
                    Ok(())
                } else {
                    return Err(RuntimeError::PoppingEmptyStack {
                        popped_into: token.clone(),
                    })?;
                }
            } else {
                run_variable_set(tokens_iter, state, tokens, num_type)
            }
        }
        Word(Get) => run_variable_get(tokens_iter, state, tokens, &num_type),
        Word(Push) => {
            // todo!("int lit notation");
            while matches!(tokens_iter.peek(tokens).token_type, Number(_)) {
                token = tokens_iter.next(tokens);
                if let Number(num) = token.token_type {
                    match num_type {
                        NumericType::I128 => {
                            push_numeric!(state, num, i128);
                        }
                        NumericType::I64 => {
                            push_numeric!(state, num, i64);
                        }
                        NumericType::I32 => {
                            push_numeric!(state, num, i32);
                        }
                        NumericType::I16 => {
                            push_numeric!(state, num, i16);
                        }
                        NumericType::I8 => {
                            push_numeric!(state, num, i8);
                        }
                        NumericType::F32 => {
                            let float = parse_float32(tokens_iter, num, tokens)?;
                            state.stack.extend_from_slice(&float.to_ne_bytes());
                        }
                        NumericType::F64 => {
                            let float = parse_float64(tokens_iter, num, tokens)?;
                            state.stack.extend_from_slice(&float.to_ne_bytes());
                        }
                    }
                }
            }
            Ok(())
        }
        Word(Pop) => {
            token = tokens_iter.next(tokens);
            match &token.token_type {
                Word(Mem) => {
                    // let stack_size = state.stack.len();
                    // // let popped  = &state.stack[stack_size - 1- get_num_size(&num_type)..];
                    // let popped = state.stack.last_chunk::<{size_of::<u32>()}>().unwrap();
                    // // state.stack.truncate(stack_size - 1- get_num_size(&num_type));
                    // state.memory.insert(popped)?
                    // // if let Some(value) = state.stack.pop() {
                    // //     state.memory.insert(unsafe {
                    // //         transmute(value)
                    // //     })?
                    // // }
                    insert_memory!(state, num_type)
                }
                _ => todo!("add ability to pop into variables"),
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
        // Word(Cpy) => {
        //     token = tokens_iter.next(tokens);
        //     let pointer: usize;
        //     if let Number(num) = &token.token_type {
        //         pointer = *num as usize
        //     } else {
        //         return Err(SyntaxError::Expected {
        //             expected_token: "number".to_string(),
        //             found: token.clone(),
        //         })?;
        //     }
        //     match num_type {
        //         NumericType::I128 => {
        //             let bytes = state.memory.copy(pointer, size_of::<i128>());
        //             state.stack.push(StackValue::I128(i128::from_be_bytes(
        //                 bytes.try_into().unwrap(),
        //             )));
        //             Ok(())
        //         }
        //         NumericType::I64 => {
        //             let bytes = state.memory.copy(pointer, size_of::<i64>());
        //             state.stack.push(StackValue::I64(i64::from_be_bytes(
        //                 bytes.try_into().unwrap(),
        //             )));
        //             Ok(())
        //         }
        //         NumericType::I32 => {
        //             let bytes = state.memory.copy(pointer, size_of::<i32>());
        //             state.stack.push(StackValue::I32(i32::from_be_bytes(
        //                 bytes.try_into().unwrap(),
        //             )));
        //             Ok(())
        //         }
        //         NumericType::I16 => {
        //             let bytes = state.memory.copy(pointer, size_of::<i16>());
        //             state.stack.push(StackValue::I16(i16::from_be_bytes(
        //                 bytes.try_into().unwrap(),
        //             )));
        //             Ok(())
        //         }
        //         NumericType::I8 => {
        //             let bytes = state.memory.copy(pointer, size_of::<i8>());
        //             state
        //                 .stack
        //                 .push(StackValue::I8(i8::from_be_bytes(bytes.try_into().unwrap())));
        //             Ok(())
        //         }
        //         NumericType::F32 => {
        //             let bytes = state.memory.copy(pointer, size_of::<f32>());
        //             state.stack.push(StackValue::F32(f32::from_be_bytes(
        //                 bytes.try_into().unwrap(),
        //             )));
        //             Ok(())
        //         }
        //         NumericType::F64 => {
        //             let bytes = state.memory.copy(pointer, size_of::<f64>());
        //             state.stack.push(StackValue::F64(f64::from_be_bytes(
        //                 bytes.try_into().unwrap(),
        //             )));
        //             Ok(())
        //         }
        //     }
        // }
        _ => Err(SyntaxError::Expected {
            expected_token: "numeric operation".to_string(),
            found: token.clone(),
        })?,
    }
}

fn parse_float32(
    token_iter: &mut TokenIter,
    whole_part: i128,
    tokens: &Vec<Token>,
) -> Result<f32, Error> {
    let token = token_iter.next(tokens);
    if matches!(token.token_type, FullStop) {
        return Err(SyntaxError::Expected {
            expected_token: "full stop for float".to_string(),
            found: token.clone(),
        }
        .into());
    }

    let token = token_iter.next(tokens);
    if let Number(decimal_part) = token.token_type {
        let decimal_places = count_digits(decimal_part);
        let divisor = 10_f32.powi(decimal_places as i32);
        Ok(whole_part as f32 + (decimal_part as f32) / divisor)
    } else {
        return Err(SyntaxError::Expected {
            expected_token: "number".to_string(),
            found: token.clone(),
        }
        .into());
    }
}
fn parse_float64(
    token_iter: &mut TokenIter,
    whole_part: i128,
    tokens: &Vec<Token>,
) -> Result<f64, Error> {
    let token = token_iter.next(tokens);
    if matches!(token.token_type, FullStop) {
        return Err(SyntaxError::Expected {
            expected_token: "full stop for float".to_string(),
            found: token.clone(),
        }
        .into());
    }

    let token = token_iter.next(tokens);
    if let Number(decimal_part) = token.token_type {
        let decimal_places = count_digits(decimal_part);
        let divisor = 10_f64.powi(decimal_places as i32);
        Ok(whole_part as f64 + (decimal_part as f64) / divisor)
    } else {
        return Err(SyntaxError::Expected {
            expected_token: "number".to_string(),
            found: token.clone(),
        }
        .into());
    }
}

fn count_digits(mut n: i128) -> u32 {
    if n == 0 {
        return 1;
    }
    let mut count = 0;
    if n < 0 {
        n = -n;
    }
    while n > 0 {
        n /= 10;
        count += 1;
    }
    count
}
fn run_call(
    tokens_iter: &mut TokenIter,
    state: &mut MachineState,
    tokens: &mut Vec<Token>,
) -> Result<(), Error> {
    let token = tokens_iter.next(tokens);
    let call_site_index = tokens_iter.index;
    match token.token_type {
        FuncIdent(ref ident) => {
            if let Some(func_index) = state.functions.get(ident) {
                tokens_iter.index = call_site_index;
                state.function_stack.push(call_site_index);
                if *ident == "printmemory".to_string() {
                    print_memory(state);
                    Ok(())
                } else {
                    run_func(tokens_iter, state, *func_index, tokens)
                }
            } else {
                return Err(SemanticError::UndefinedFunc {
                    ident: token.clone(),
                })?;
            }
        }
        _ => {
            return Err(SyntaxError::Expected {
                expected_token: "function ident".to_string(),
                found: token.clone(),
            })?;
        }
    }
}
fn print_memory(state: &mut MachineState) {
    println!("Memory: {:?}", state.memory)
}

fn find_funcs(
    tokens_iter: &mut TokenIter,
    state: &mut MachineState,
    tokens: &Vec<Token>,
) -> Result<(), Error> {
    let mut token = tokens_iter.current(tokens);
    match token.token_type {
        Word(Func) => {
            let func_index;
            let mut func_export: Option<String> = None;
            let func_ident;
            token = &tokens_iter.next(tokens);
            match &token.token_type {
                FuncIdent(ident) => func_ident = ident,
                // wrong_token => return Err(format!("Syntax Error: Expected function ident found {wrong_token:?} at {}:{}", token.line, token.column))
                _ => {
                    return Err(Error::SyntacError(SyntaxError::Expected {
                        expected_token: "function ident".to_string(),
                        found: token.clone(),
                    }));
                }
            }
            token = &tokens_iter.next(tokens);
            match &token.token_type {
                StringLit(lit) => {
                    func_export = Some(lit.to_string());
                    token = &tokens_iter.next(tokens);
                    match token.token_type {
                        Colon => func_index = tokens_iter.index,
                        _ => {
                            return Err(SyntaxError::Expected {
                                expected_token: "collon or export".to_string(),
                                found: token.clone(),
                            })?;
                        }
                    }
                }
                Colon => func_index = tokens_iter.index,
                _ => {
                    return Err(SyntaxError::Expected {
                        expected_token: "collon or export".to_string(),
                        found: token.clone(),
                    })?;
                }
            }
            state.functions.insert(func_ident.to_string(), func_index);
            if let Some(export) = func_export {
                state.exports.insert(export, func_index);
            }
        }
        EOF => {
            tokens_iter.reset();
            return Ok(());
        }
        _ => {}
    }
    tokens_iter.next(tokens);
    find_funcs(tokens_iter, state, tokens)?;
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
    (for int $type:ty, $operation:ident, $state:ident) => {
        match $operation {
            NumericeOp::Add => {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = (<$type>::from_ne_bytes(lhs) + <$type>::from_ne_bytes(rhs)).to_ne_bytes();
                $state.stack.extend_from_slice(&result)
            }
            NumericeOp::Sub =>  {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = (<$type>::from_ne_bytes(lhs) - <$type>::from_ne_bytes(rhs)).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Mul =>  {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = (<$type>::from_ne_bytes(lhs) * <$type>::from_ne_bytes(rhs)).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Div =>  {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = (<$type>::from_ne_bytes(lhs) / <$type>::from_ne_bytes(rhs)).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Rem =>  {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = (<$type>::from_ne_bytes(lhs) % <$type>::from_ne_bytes(rhs)).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::And =>  {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = (<$type>::from_ne_bytes(lhs) & <$type>::from_ne_bytes(rhs)).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Or =>  {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = (<$type>::from_ne_bytes(lhs) | <$type>::from_ne_bytes(rhs)).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Xor =>  {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = (<$type>::from_ne_bytes(lhs) ^ <$type>::from_ne_bytes(rhs)).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Eq => {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = ((<$type>::from_ne_bytes(lhs) == <$type>::from_ne_bytes(rhs)) as u8).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Neq => {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = ((<$type>::from_ne_bytes(lhs) != <$type>::from_ne_bytes(rhs)) as u8).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Gte => {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = ((<$type>::from_ne_bytes(lhs) >= <$type>::from_ne_bytes(rhs)) as u8).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Gt => {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = ((<$type>::from_ne_bytes(lhs) > <$type>::from_ne_bytes(rhs)) as u8).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Lte => {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = ((<$type>::from_ne_bytes(lhs) <= <$type>::from_ne_bytes(rhs)) as u8).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Lt => {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = ((<$type>::from_ne_bytes(lhs) < <$type>::from_ne_bytes(rhs)) as u8).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Max => {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = (<$type>::from_ne_bytes(lhs).max(<$type>::from_ne_bytes(rhs))).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Min => {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = (<$type>::from_ne_bytes(lhs).min(<$type>::from_ne_bytes(rhs))).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
        }
    };
    (for float $type:ty, $operation:ident, $state:ident) => {
        match $operation {
            NumericeOp::Add => {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = (<$type>::from_ne_bytes(lhs) + <$type>::from_ne_bytes(rhs)).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Sub =>  {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = (<$type>::from_ne_bytes(lhs) - <$type>::from_ne_bytes(rhs)).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Mul =>  {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = (<$type>::from_ne_bytes(lhs) * <$type>::from_ne_bytes(rhs)).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Div =>  {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = (<$type>::from_ne_bytes(lhs) / <$type>::from_ne_bytes(rhs)).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Rem =>  {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = (<$type>::from_ne_bytes(lhs) % <$type>::from_ne_bytes(rhs)).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            // NumericeOp::And =>  {
            //     let result = <$type>::from_ne_bytes($lhs.try_into().unwrap()) & <$type>::from_ne_bytes($rhs.try_into().unwrap());
            //     result.to_ne_bytes().to_vec()
            // }
            // NumericeOp::Or =>  {
            //     let result = <$type>::from_ne_bytes($lhs.try_into().unwrap()) | <$type>::from_ne_bytes($rhs.try_into().unwrap());
            //     result.to_ne_bytes().to_vec()
            // }
            // NumericeOp::Xor =>  {
            //     let result = <$type>::from_ne_bytes($lhs.try_into().unwrap()) ^ <$type>::from_ne_bytes($rhs.try_into().unwrap());
            //     result.to_ne_bytes().to_vec()
            // }
            NumericeOp::Eq => {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = ((<$type>::from_ne_bytes(lhs) == <$type>::from_ne_bytes(rhs)) as u8).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Neq => {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = ((<$type>::from_ne_bytes(lhs) != <$type>::from_ne_bytes(rhs)) as u8).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Gte => {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = ((<$type>::from_ne_bytes(lhs) >= <$type>::from_ne_bytes(rhs)) as u8).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Gt => {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = ((<$type>::from_ne_bytes(lhs) > <$type>::from_ne_bytes(rhs)) as u8).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Lte => {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = ((<$type>::from_ne_bytes(lhs) <= <$type>::from_ne_bytes(rhs)) as u8).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Lt => {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = ((<$type>::from_ne_bytes(lhs) < <$type>::from_ne_bytes(rhs)) as u8).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Max => {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = (<$type>::from_ne_bytes(lhs).max(<$type>::from_ne_bytes(rhs))).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            NumericeOp::Min => {
                let rhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let lhs = $state.stack.last_chunk::<{size_of::<$type>()}>().unwrap().clone();
                $state.stack.truncate($state.stack.len() - size_of::<$type>());
                let result = (<$type>::from_ne_bytes(lhs).min(<$type>::from_ne_bytes(rhs))).to_ne_bytes();
                for i in result {
                    $state.stack.push(i);
                }
            }
            operation => {
                return Err(SemanticError::IncorrectOperation {
                    operation: operation,
                    val_type: "float".to_string(),
                })?
            }
        }
        // match $operation {
        //     NumericeOp::Add => unsafe {
        //         let result = transmute::<&[u8], $type>($lhs) + transmute::<&[u8], $type>($rhs);
        //         transmute::<$type, &[u8]>(result)
        //     }
        //     NumericeOp::Sub =>  unsafe {
        //         let result = transmute::<&[u8], $type>($lhs) - transmute::<&[u8], $type>($rhs);
        //         transmute::<$type, &[u8]>(result)
        //     }
        //     NumericeOp::Mul =>  unsafe {
        //         let result = transmute::<&[u8], $type>($lhs) * transmute::<&[u8], $type>($rhs);
        //         transmute::<$type, &[u8]>(result)
        //     }
        //     NumericeOp::Div =>  unsafe {
        //         let result = transmute::<&[u8], $type>($lhs) / transmute::<&[u8], $type>($rhs);
        //         transmute::<$type, &[u8]>(result)
        //     }
        //     NumericeOp::Rem =>  unsafe {
        //         let result = transmute::<&[u8], $type>($lhs) % transmute::<&[u8], $type>($rhs);
        //         transmute::<$type, &[u8]>(result)
        //     }
        //     // NumericeOp::Eq => ($lhs == $rhs) as i32 as $type,
        //     // NumericeOp::Neq => ($lhs != $rhs) as i32 as $type,
        //     // NumericeOp::Gte => ($lhs >= $rhs) as i32 as $type,
        //     // NumericeOp::Gt => ($lhs > $rhs) as i32 as $type,
        //     // NumericeOp::Lte => ($lhs <= $rhs) as i32 as $type,
        //     // NumericeOp::Lt => ($lhs < $rhs) as i32 as $type,
        //     // NumericeOp::Max => $lhs.max($rhs),
        //     // NumericeOp::Min => $lhs.min($rhs),
        //     operation => {
        //         return Err(SemanticError::IncorrectOperation {
        //             operation: operation,
        //             val_type: "float".to_string(),
        //         })?
        //     }
        // }
    };
}

fn process_numerical_op(
    state: &mut MachineState,
    num_type: NumericType,
    operation: NumericeOp,
) -> Result<(), Error> {
    match num_type {
        NumericType::I32 => {
            perform_operation!(for int i32, operation, state)
        }
        NumericType::I64 => {
            perform_operation!(for int i64, operation, state)
        }
        NumericType::I16 => {
            perform_operation!(for int i16, operation, state)
        }
        NumericType::I8 => {
            perform_operation!(for int i8, operation, state)
        }
        NumericType::F32 => {
            perform_operation!(for float f32, operation, state)
        }
        NumericType::F64 => {
            perform_operation!(for float f64, operation, state)
        }
        NumericType::I128 => {
            perform_operation!(for int i128, operation, state)
        }
    }
    Ok(())
}
