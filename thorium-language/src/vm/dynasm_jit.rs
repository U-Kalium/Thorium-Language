use crate::vm::run_byte_code::{
    NumericeOp, SemanticError, SyntaxError, parse_float32, parse_float64,
};
use crate::vm::tokenizer::{TokenType::*, WordToken::*};
use dynasmrt::x64::X64Relocation;
use dynasmrt::*;
use hashbrown::HashMap;
use std::mem::{self, transmute};

use crate::vm::{Error, run_byte_code::TokenIter, tokenizer::Token};

enum StackType {
    I128,
    I64,
    I32,
    I16,
    I8,
    F32,
    F64,
}

impl StackType {
    fn size(&self) -> usize {
        match self {
            StackType::I128 => size_of::<i128>(),
            StackType::I64 => size_of::<i64>(),
            StackType::I32 => size_of::<i32>(),
            StackType::I16 => size_of::<i16>(),
            StackType::I8 => size_of::<i8>(),
            StackType::F32 => size_of::<f64>(),
            StackType::F64 => size_of::<f64>(),
        }
    }
}

struct Variable {
    ident: String,
    stack_offset: usize,
    type_: StackType,
}

#[derive(Debug, Clone)]
struct Function {
    token_index: usize,
    dyn_label: DynamicLabel,
}

macro_rules! function_prologue {
    ($ops:ident) => {
        dynasm!($ops
            ; push rbp
            ; mov rbp, rsp
        )
    };
}
macro_rules! function_epiloge {
    ($ops:ident) => {
        dynasm!($ops
            ; mov rsp, rbp
            ; pop rbp
        )
    };
}

macro_rules! compile_operation {
    (for int $operation:ident, $ops:ident) => {
        match $operation {
            NumericeOp::Add => dynasm!($ops
                ; pop r10
                ; pop r11
                ; add r10, r11
                ; push r10

            ),
            NumericeOp::Sub => dynasm!($ops
                ; pop r10
                ; pop r11
                ; sub r10, r11
                ; push r10

            ),
            NumericeOp::Mul => dynasm!($ops
                ; pop r10
                ; pop r11
                ; imul r10, r11
                ; push r10

            ),
            NumericeOp::Div => dynasm!($ops
                ; pop rax
                ; pop rbx
                ; cdq
                ; idiv rbx
                ; push rax

            ),
            NumericeOp::Rem => dynasm!($ops
                ; pop rax
                ; pop rbx
                ; cdq
                ; idiv rbx
                ; push rdx

            ),
            NumericeOp::And => dynasm!($ops
                ; pop r10
                ; pop r11
                ; and r10, r11
                ; push r10

            ),
            NumericeOp::Or => dynasm!($ops
                ; pop r10
                ; pop r11
                ; or r10, r11
                ; push r10

            ),
            NumericeOp::Xor => dynasm!($ops
                ; pop r10
                ; pop r11
                ; xor r10, r11
                ; push r10

            ),
            NumericeOp::Eq => dynasm!($ops
                ; pop r11
                ; pop r10
                ; xor r9, r9
                ; cmp r10, r11
                ; sete r9b
                ; push r9
            ),
            NumericeOp::Neq => dynasm!($ops
                ; pop r11
                ; pop r10
                ; xor r9, r9
                ; cmp r10, r11
                ; setne r9b
                ; push r9
            ),
            NumericeOp::Gte => dynasm!($ops
                ; pop r11
                ; pop r10
                ; xor r9, r9
                ; cmp r10, r11
                ; setge r9b
                ; push r9
            ),
            NumericeOp::Gt => dynasm!($ops
                ; pop r11
                ; pop r10
                ; xor r9, r9
                ; cmp r10, r11
                ; setg r9b
                ; push r9
            ),
            NumericeOp::Lte => dynasm!($ops
                ; pop r11
                ; pop r10
                ; xor r9, r9
                ; cmp r10, r11
                ; setle r9b
                ; push r9
            ),
            NumericeOp::Lt => dynasm!($ops
                ; pop r11
                ; pop r10
                ; xor r9, r9
                ; cmp r10, r11
                ; setl r9b
                ; push r9
            ),
            NumericeOp::Max => dynasm!($ops
                ; pop r10
                ; pop r11
                ; cmp r10, r11
                ; cmovl r10,r11
                ; push r10
            ),
            NumericeOp::Min => dynasm!($ops
                ; pop r10
                ; pop r11
                ; cmp r10, r11
                ; cmovg r10,r11
                ; push r10
            ),
        }
    };
}

pub struct JIT {
    // ops: &'a mut Assembler<x64::X64Relocation>,
    functions: HashMap<String, Function>,
    exports: HashMap<String, usize>,
    variables: HashMap<String, Variable>,
    start_offset: Option<AssemblyOffset>,
    lables: HashMap<String, DynamicLabel>,
}

impl JIT {
    // pub fn new(mut ops: Assembler<X64Relocation>) -> Self {
    //     Self {
    //         ops: &mut ops,
    //         functions: HashMap::new(),
    //         exports: HashMap::new(),
    //         variables: HashMap::new(),
    //         start_offset: None
    //     }
    // }
    fn compile_and_run<O>(
        &mut self,
        tokens: &mut Vec<Token>,
        tokens_iter: &mut TokenIter,
        mut ops: Assembler<X64Relocation>,
    ) -> Result<O, Error> {
        self.collect_funcs(tokens, tokens_iter, &mut ops)?;
        self.compile_funcs(tokens, tokens_iter, &mut ops)?;
        let start_offset = self.start_offset.unwrap();
        // let ops = self.ops;

        let buf = ops.finalize().unwrap();
        let start_fn: extern "C" fn() -> O = unsafe { mem::transmute(buf.ptr(start_offset)) };
        let result = (start_fn)();
        Ok(result)
    }

    fn collect_funcs(
        &mut self,
        tokens: &mut Vec<Token>,
        tokens_iter: &mut TokenIter,
        ops: &mut Assembler<X64Relocation>,
    ) -> Result<(), Error> {
        let mut token = tokens_iter.current(tokens);
        match token.token_type {
            Word(Func) => {
                let func_index;
                let mut func_export = None;
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
                        func_export = Some(lit);
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
                let func_label = ops.new_dynamic_label();
                let func = Function {
                    token_index: func_index,
                    dyn_label: func_label,
                };
                self.functions.insert(func_ident.to_string(), func);
                // self.functions.insert(func_ident.to_string(), func_index);
                if let Some(ident) = func_export {
                    self.exports.insert(ident.to_owned(), func_index);
                }
            }
            EOF => {
                tokens_iter.reset();
                return Ok(());
            }
            _ => {}
        }
        tokens_iter.next(tokens);
        self.collect_funcs(tokens, tokens_iter, ops)?;
        Ok(())
    }
    fn compile_funcs(
        &mut self,
        tokens: &mut Vec<Token>,
        tokens_iter: &mut TokenIter,
        ops: &mut Assembler<X64Relocation>,
    ) -> Result<(), Error> {
        for (ident, func) in self.functions.clone() {
            if ident == "start".to_string() {
                self.start_offset = Some(ops.offset())
            }
            tokens_iter.index = func.token_index;
            self.compile_func(tokens, tokens_iter, &ident, ops)?;
        }
        Ok(())
    }

    fn compile_func(
        &mut self,
        tokens: &mut Vec<Token>,
        tokens_iter: &mut TokenIter,
        func_ident: &String,
        ops: &mut Assembler<X64Relocation>,
    ) -> Result<(), Error> {
        let func = self.functions.get(func_ident).unwrap().clone();
        let variable_space = self.collect_variable_declerations(tokens, tokens_iter)?;

        tokens_iter.index = func.token_index;
        dynasm!(ops
            ; .arch x64
            ; => func.dyn_label
            ;; function_prologue!(ops)
            ; mov rax, QWORD variable_space as _
            ; sub rsp,  rax
        );

        while !matches!(tokens_iter.peek(tokens).token_type, Word(EndFunc)) {
            let mut token = tokens_iter.next(tokens);
            match &token.token_type {
                LabelIdent(ident) => {
                    let label = ops.new_dynamic_label();
                    self.lables.insert(ident.to_owned(), label);
                }
                _ => {}
            }
        }
        tokens_iter.index = func.token_index;

        while !matches!(tokens_iter.peek(tokens).token_type, Word(EndFunc)) {
            let mut token = tokens_iter.next(tokens);
            match &token.token_type {
                Word(Return) => {
                    dynasm!(ops
                        ; pop rax
                        ; movq xmm0, rax
                        ;; function_epiloge!(ops)
                        ; ret
                    );
                }
                Word(Call) => {
                    token = tokens_iter.next(tokens);
                    if let FuncIdent(ident) = &token.token_type {
                        if let Some(func) = self.functions.get(ident) {
                            dynasm!(ops
                                ; call =>func.dyn_label
                                ; push rax
                            )
                        } else {
                            return Err(SemanticError::UndefinedFunc {
                                ident: token.clone(),
                            })?;
                        }
                    }
                }
                LabelIdent(ident) => {
                    let label = self.lables.get(ident).unwrap();
                    dynasm!(ops
                        ; => *label
                    )
                }
                Word(Set) => {
                    token = tokens_iter.next(tokens);
                    match &token.token_type {
                        VarIdent(ident) => {
                            if let Some(var) = self.variables.get(ident) {
                                let offset = var.stack_offset as i32;
                                match var.type_ {
                                    StackType::I128 => todo!(),
                                    StackType::I64 => dynasm!(ops
                                        ; pop r10
                                        ; mov [rbp - offset], r10
                                    ),
                                    StackType::I32 => dynasm!(ops
                                        ; mov r10d, [rsp]
                                        ; sub rsp, var.type_.size() as i32
                                        ; mov [rbp - offset], r10d
                                    ),
                                    StackType::I16 => dynasm!(ops
                                        ; mov r10w, [rsp]
                                        ; sub rsp, var.type_.size() as i32
                                        ; mov [rbp - offset], r10w
                                    ),
                                    StackType::I8 => dynasm!(ops
                                        ; mov r10b, [rsp]
                                        ; sub rsp, var.type_.size() as i32
                                        ; mov [rbp - offset], r10b
                                    ),
                                    StackType::F32 => dynasm!(ops
                                        ; pop r10
                                        ; mov [rbp - offset], r10
                                    ),
                                    StackType::F64 => dynasm!(ops
                                        ; pop r10
                                        ; mov [rbp - offset], r10
                                    ),
                                }
                            } else {
                                return Err(SemanticError::UndefinedVar {
                                    ident: token.clone(),
                                })?;
                            }
                        }
                        Word(Stack) => {
                            token = tokens_iter.next(tokens);
                            match &token.token_type {
                                Number(num) => {
                                    dynasm!(ops
                                        ; mov r10, *num as _
                                    )
                                }
                                VarIdent(ident) => {
                                    if let Some(var) = self.variables.get(ident) {
                                        dynasm!(ops
                                            ; mov r10, [rbp - var.stack_offset as i32]
                                        )
                                    } else {
                                        return Err(SemanticError::UndefinedVar {
                                            ident: token.clone(),
                                        })?;
                                    }
                                }
                                Word(Top) => {
                                    dynasm!(ops
                                        ; mov r10, rsp
                                    )
                                }
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
                                        Number(num) => {
                                            let number = num * 8;
                                            dynasm!(ops
                                                ; sub r10, number as _
                                            )
                                        }
                                        Word(Top) => {
                                            dynasm!(ops
                                                ; mov r11, [rsp]
                                                ; mov r9, 8
                                                ; imul r11, r9
                                                ; sub r10, r11
                                            )
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
                                        Number(num) => {
                                            let number = num * 8;
                                            dynasm!(ops
                                                ; add r10, number as _
                                            )
                                        }
                                        Word(Top) => {
                                            dynasm!(ops
                                                ; mov r11, [rsp]
                                                ; mov r9, 8
                                                ; imul r11, r9
                                                ; add r10, r11
                                            )
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
                            dynasm!(ops
                                ; pop r11
                                ; mov [r10], r11
                            )
                        }
                        _ => {
                            return Err(SyntaxError::Expected {
                                expected_token: "variable ident, or stack keyword".to_owned(),
                                found: token.clone(),
                            })?;
                        }
                    }
                }
                Word(Get) => {
                    token = tokens_iter.next(tokens);
                    if let VarIdent(ident) = &token.token_type {
                        if let Some(var) = self.variables.get(ident) {
                            dynasm!(ops
                                ; mov rax, [rbp - var.stack_offset as i32]
                                ; push rax
                            );
                        } else {
                            return Err(SemanticError::UndefinedVar {
                                ident: token.clone(),
                            })?;
                        }
                    } else {
                        return Err(SyntaxError::Expected {
                            expected_token: "variable ident".to_owned(),
                            found: token.clone(),
                        })?;
                    }
                }
                Word(Cast) => {
                    tokens_iter.next(tokens);
                }
                Word(Cpy) => {
                    token = tokens_iter.next(tokens);
                    match &token.token_type {
                        Number(num) => {
                            dynasm!(ops
                                ; mov r10, *num as _
                            )
                        }
                        VarIdent(ident) => {
                            if let Some(var) = self.variables.get(ident) {
                                dynasm!(ops
                                    ; mov r10, [rbp - var.stack_offset as i32]
                                )
                            } else {
                                return Err(SemanticError::UndefinedVar {
                                    ident: token.clone(),
                                })?;
                            }
                        }
                        Word(Top) => {
                            dynasm!(ops
                                ; mov r10, rsp
                            )
                        }
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
                                Number(num) => {
                                    let number = num * 8;
                                    dynasm!(ops
                                        ; sub r10, number as _
                                    )
                                }
                                Word(Top) => {
                                    let byte = 8;
                                    dynasm!(ops
                                        ; mov r11, [rsp]
                                        ; mov r9, 8
                                        ; imul r11, r9
                                        ; sub r10, r11
                                    )
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
                                Number(num) => {
                                    let number = num * 8;
                                    dynasm!(ops
                                        ; add r10, number as _
                                    )
                                }
                                Word(Top) => {
                                    dynasm!(ops
                                        ; mov r11, [rsp]
                                        ; mov r9, 8
                                        ; imul r11, r9
                                        ; add r10, r11
                                    )
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
                    token = tokens_iter.next(tokens);
                    match &token.token_type {
                        Word(Mem) => todo!(),
                        VarIdent(ident) => {
                            if let Some(var) = self.variables.get(ident) {
                                dynasm!(ops
                                    ; mov r11, [r10]
                                    ; mov [rbp - var.stack_offset as i32], r11
                                )
                            } else {
                                return Err(SemanticError::UndefinedVar {
                                    ident: token.clone(),
                                })?;
                            }
                        }
                        Word(Top) => {
                            dynasm!(ops
                                ; mov r11, [r10]
                                ; mov [rsp], r11
                            )
                        }
                        _ => {
                            return Err(SyntaxError::Expected {
                                expected_token: "mem or var ident".to_string(),
                                found: token.clone(),
                            })?;
                        }
                    }
                }
                Word(I128) => {
                    self.compile_numeric_instruction(tokens, tokens_iter, StackType::I128, ops)?
                }
                Word(I64) => {
                    self.compile_numeric_instruction(tokens, tokens_iter, StackType::I64, ops)?
                }
                Word(I32) => {
                    self.compile_numeric_instruction(tokens, tokens_iter, StackType::I32, ops)?
                }
                Word(I16) => {
                    self.compile_numeric_instruction(tokens, tokens_iter, StackType::I16, ops)?
                }
                Word(I8) => {
                    self.compile_numeric_instruction(tokens, tokens_iter, StackType::I8, ops)?
                }
                Word(F64) => {
                    self.compile_numeric_instruction(tokens, tokens_iter, StackType::F64, ops)?
                }
                Word(F32) => {
                    self.compile_numeric_instruction(tokens, tokens_iter, StackType::F32, ops)?
                }
                _ => {
                    return Err(SyntaxError::Expected {
                        expected_token: "function instruction".to_string(),
                        found: token.clone(),
                    })?;
                }
            }
        }

        Ok(())
    }

    fn compile_numeric_instruction(
        &mut self,
        tokens: &mut Vec<Token>,
        tokens_iter: &mut TokenIter,
        num_type: StackType,
        ops: &mut Assembler<X64Relocation>,
    ) -> Result<(), Error> {
        let mut token = tokens_iter.next(tokens);
        match token.token_type {
            Word(Push) => {
                while matches!(tokens_iter.peek(tokens).token_type, Number(_) | Word(Top)) {
                    token = tokens_iter.next(tokens);
                    match token.token_type {
                        Number(num) => {
                            match num_type {
                                StackType::I128 => {
                                    todo!()
                                }
                                StackType::I64 => {
                                    let int = num as i64;
                                    dynasm!(ops
                                        ; .arch x64
                                        ; mov rax, QWORD int
                                        ; push rax
                                    )
                                }
                                StackType::I32 => {
                                    let int = num as i32;
                                    dynasm!(ops
                                        ; .arch x64
                                        ; mov eax, DWORD int
                                        ; push rax
                                    )
                                }
                                StackType::I16 => {
                                    let int = num as i64;
                                    dynasm!(ops
                                        ; .arch x64
                                        ; mov rax, QWORD int
                                        ; push rax
                                    )
                                }
                                StackType::I8 => {
                                    let int = num as i64;
                                    dynasm!(ops
                                        ; .arch x64
                                        ; mov rax, QWORD int
                                        ; push rax
                                    )
                                }
                                StackType::F32 => {
                                    let float = parse_float32(tokens_iter, num, tokens)?;
                                    let as_int = f32::to_bits(float).cast_signed();
                                    dynasm!(ops
                                        ; .arch x64
                                        ; mov rax, as_int
                                        ; push rax
                                    )
                                }
                                StackType::F64 => {
                                    let float = parse_float64(tokens_iter, num, tokens)?;
                                    dbg!(float);
                                    let as_int = f64::to_bits(float).cast_signed();
                                    dbg!(as_int);
                                    
                                    dynasm!(ops
                                        ; .arch x64
                                        ; mov rax, QWORD as_int
                                        ; push rax
                                    )
                                }
                            }
                        }
                        Word(Top) => match num_type {
                            StackType::I128 => todo!(),
                            StackType::I64 => {
                                dynasm!(ops
                                    ; .arch x64
                                    ; push rsp
                                )
                            }
                            StackType::I32 => {
                                dynasm!(ops
                                    ; .arch x64
                                    ; push rsp
                                )
                            }
                            StackType::I16 => {
                                dynasm!(ops
                                    ; .arch x64
                                    ; push rsp
                                )
                            }
                            StackType::I8 => {
                                dynasm!(ops
                                    ; .arch x64
                                    ; push rsp
                                )
                            }
                            StackType::F32 => {
                                todo!()
                            }
                            StackType::F64 => {
                                todo!()
                            }
                        },
                        _ => {
                            return Err(SyntaxError::Expected {
                                expected_token: "top or number".to_owned(),
                                found: token.clone(),
                            })?;
                        }
                    }
                }
            }
            Word(Jpz) => {
                token = tokens_iter.next(tokens);
                if let StringLit(lit) = &token.token_type {
                    if let Some(label) = self.lables.get(lit) {
                        match num_type {
                            StackType::I128 => todo!(),
                            StackType::I64 => dynasm!(ops
                                ; pop r10
                                ; test r10, r10
                                ; jz => *label
                            ),
                            StackType::I32 => dynasm!(ops
                                ; pop r10
                                ; test r10d, r10d
                                ; jz => *label
                            ),
                            StackType::I16 => dynasm!(ops
                                ; pop r10
                                ; test r10w, r10w
                                ; jz => *label
                            ),
                            StackType::I8 => dynasm!(ops
                                ; pop r10
                                ; test r10b, r10b
                                ; jz => *label
                            ),
                            StackType::F32 => todo!(),
                            StackType::F64 => todo!(),
                        }
                    } else {
                        return Err(SemanticError::UndefindLabel {
                            ident: token.clone(),
                        })?;
                    }
                } else {
                    return Err(SyntaxError::Expected {
                        expected_token: "string lit for label".to_owned(),
                        found: token.clone(),
                    })?;
                }
            }
            Word(Jmp) => {
                token = tokens_iter.next(tokens);
                if let StringLit(lit) = &token.token_type {
                    if let Some(label) = self.lables.get(lit) {
                        dynasm!(ops
                            ; pop r10
                            ; test r10, r10
                            ; jnz =>*label
                        )
                    } else {
                        return Err(SemanticError::UndefindLabel {
                            ident: token.clone(),
                        })?;
                    }
                } else {
                    return Err(SyntaxError::Expected {
                        expected_token: "string lit for label".to_owned(),
                        found: token.clone(),
                    })?;
                }
            }
            Word(Add) => {
                let operation = NumericeOp::Add;

                compile_operation!(for int
                    operation,
                    ops
                )
            }
            Word(Sub) => {
                let operation = NumericeOp::Sub;

                compile_operation!(for int
                    operation,
                    ops
                )
            }
            Word(Mul) => {
                let operation = NumericeOp::Mul;

                compile_operation!(for int
                    operation,
                    ops
                )
            }
            Word(Div) => {
                let operation = NumericeOp::Div;

                compile_operation!(for int
                    operation,
                    ops
                )
            }
            Word(Rem) => {
                let operation = NumericeOp::Rem;

                compile_operation!(for int
                    operation,
                    ops
                )
            }
            Word(And) => {
                let operation = NumericeOp::And;

                compile_operation!(for int
                    operation,
                    ops
                )
            }
            Word(Or) => {
                let operation = NumericeOp::Or;

                compile_operation!(for int
                    operation,
                    ops
                )
            }
            Word(Xor) => {
                let operation = NumericeOp::Xor;

                compile_operation!(for int
                    operation,
                    ops
                )
            }
            Word(Eq) => {
                let operation = NumericeOp::Eq;

                compile_operation!(for int
                    operation,
                    ops
                )
            }
            Word(Neq) => {
                let operation = NumericeOp::Neq;

                compile_operation!(for int
                    operation,
                    ops
                )
            }
            Word(Gte) => {
                let operation = NumericeOp::Gte;

                compile_operation!(for int
                    operation,
                    ops
                )
            }
            Word(Gt) => {
                let operation = NumericeOp::Gt;

                compile_operation!(for int
                    operation,
                    ops
                )
            }
            Word(Lte) => {
                let operation = NumericeOp::Lte;

                compile_operation!(for int
                    operation,
                    ops
                )
            }
            Word(Lt) => {
                let operation = NumericeOp::Lt;

                compile_operation!(for int
                    operation,
                    ops
                )
            }
            Word(Max) => {
                let operation = NumericeOp::Max;

                compile_operation!(for int
                    operation,
                    ops
                )
            }
            Word(Min) => {
                let operation = NumericeOp::Min;

                compile_operation!(for int
                    operation,
                    ops
                )
            }
            Word(Declare) => {
                tokens_iter.next(tokens);
            }

            _ => todo!("{:?}", token),
        }
        Ok(())
    }

    fn collect_variable_declerations(
        &mut self,
        tokens: &mut Vec<Token>,
        tokens_iter: &mut TokenIter,
    ) -> Result<usize, Error> {
        let mut total_variable_offset = 0;
        while !matches!(tokens_iter.peek(tokens).token_type, Word(EndFunc)) {
            let mut token = tokens_iter.next(tokens);
            if let Word(Declare) = &tokens_iter.peek(tokens).token_type {
            } else {
                continue;
            }
            match &token.token_type {
                Word(I128) => todo!(),
                Word(I64) => {
                    tokens_iter.next(tokens);
                    token = tokens_iter.next(tokens);
                    total_variable_offset += size_of::<i64>();
                    if let VarIdent(ident) = &token.token_type {
                        let var = Variable {
                            ident: ident.clone(),
                            stack_offset: total_variable_offset,
                            type_: StackType::I64,
                        };
                        self.variables.insert(ident.to_owned(), var);
                    } else {
                        return Err(SyntaxError::Expected {
                            expected_token: "variable_ident".to_string(),
                            found: token.clone(),
                        })?;
                    }
                }
                Word(I32) => {
                    tokens_iter.next(tokens);
                    token = tokens_iter.next(tokens);
                    total_variable_offset += size_of::<i32>();
                    if let VarIdent(ident) = &token.token_type {
                        let var = Variable {
                            ident: ident.clone(),
                            stack_offset: total_variable_offset,
                            type_: StackType::I32,
                        };
                        self.variables.insert(ident.to_owned(), var);
                    } else {
                        return Err(SyntaxError::Expected {
                            expected_token: "variable_ident".to_string(),
                            found: token.clone(),
                        })?;
                    }
                }
                Word(I16) => {
                    tokens_iter.next(tokens);
                    token = tokens_iter.next(tokens);
                    total_variable_offset += size_of::<i16>();
                    if let VarIdent(ident) = &token.token_type {
                        let var = Variable {
                            ident: ident.clone(),
                            stack_offset: total_variable_offset,
                            type_: StackType::I16,
                        };
                        self.variables.insert(ident.to_owned(), var);
                    } else {
                        return Err(SyntaxError::Expected {
                            expected_token: "variable_ident".to_string(),
                            found: token.clone(),
                        })?;
                    }
                }
                Word(I8) => {
                    tokens_iter.next(tokens);
                    token = tokens_iter.next(tokens);
                    total_variable_offset += size_of::<i8>();
                    if let VarIdent(ident) = &token.token_type {
                        let var = Variable {
                            ident: ident.clone(),
                            stack_offset: total_variable_offset,
                            type_: StackType::I8,
                        };
                        self.variables.insert(ident.to_owned(), var);
                    } else {
                        return Err(SyntaxError::Expected {
                            expected_token: "variable_ident".to_string(),
                            found: token.clone(),
                        })?;
                    }
                }
                Word(F64) => {
                    tokens_iter.next(tokens);
                    token = tokens_iter.next(tokens);
                    total_variable_offset += size_of::<f64>();
                    if let VarIdent(ident) = &token.token_type {
                        let var = Variable {
                            ident: ident.clone(),
                            stack_offset: total_variable_offset,
                            type_: StackType::F64,
                        };
                        self.variables.insert(ident.to_owned(), var);
                    } else {
                        return Err(SyntaxError::Expected {
                            expected_token: "variable_ident".to_string(),
                            found: token.clone(),
                        })?;
                    }
                }
                Word(F32) => {
                    tokens_iter.next(tokens);
                    token = tokens_iter.next(tokens);
                    total_variable_offset += size_of::<f64>();
                    if let VarIdent(ident) = &token.token_type {
                        let var = Variable {
                            ident: ident.clone(),
                            stack_offset: total_variable_offset,
                            type_: StackType::F32,
                        };
                        self.variables.insert(ident.to_owned(), var);
                    } else {
                        return Err(SyntaxError::Expected {
                            expected_token: "variable_ident".to_string(),
                            found: token.clone(),
                        })?;
                    }
                }
                _ => {}
            }
        }

        Ok(total_variable_offset)
    }
}

pub fn run<O>(tokens: &mut Vec<Token>) -> Result<O, Error> {
    let mut ops = Assembler::new().unwrap();
    let mut jit = JIT {
        functions: HashMap::new(),
        exports: HashMap::new(),
        variables: HashMap::new(),
        start_offset: None,
        lables: HashMap::new(),
    };
    let mut tokens_iter = TokenIter { index: 0 };
    let result = jit.compile_and_run(tokens, &mut tokens_iter, ops).unwrap();
    // println!("dynasm jit result: {result}");
    Ok(result)
}
