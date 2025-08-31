use crate::Error;
use crate::run_byte_code::{
    NumericType, NumericeOp, RuntimeError, SemanticError, SyntaxError, parse_float32, parse_float64,
};
use crate::tokenizer::{TokenType::*, WordToken::*};
use crate::{
    run_byte_code::{self, TokenIter},
    tokenizer::Token,
};
use core::mem;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, FuncId, Linkage, Module};
use hashbrown::HashMap;

pub struct JIT {
    // builder_context: FunctionBuilderContext,
    // ctx: codegen::Context,
    // function_builder: FunctionBuilder<'a>,
    module: JITModule,
    stack: Vec<Value>,
    functions: HashMap<String, FuncId>,
    func_index: HashMap<String, usize>,
    variables: HashMap<String, Variable>,
    variable_index: usize,
}

impl JIT {
    pub fn new() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder()
            .unwrap_or_else(|msg| panic!("host machine is not supported: {}", msg));
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        // let function_builder = FunctionBuilder::new(func, func_ctx)

        let module = JITModule::new(builder);

        Self {
            // builder_context: FunctionBuilderContext::new(),
            // ctx: module.make_context(),
            // data_description: DataDescription::new(),
            module,
            stack: Vec::new(),
            functions: HashMap::new(),
            func_index: HashMap::new(),
            variables: HashMap::new(),
            variable_index: 0,
        }
    }
    fn compile_funcs(
        &mut self,
        tokens: &mut Vec<Token>,
        tokens_iter: &mut TokenIter,
    ) -> Result<(), Error> {
        let mut token = tokens_iter.current(tokens);
        match token.token_type {
            Word(Func) => {
                let func_index;
                let mut func_export = false;
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
                        func_export = true;
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
                self.func_index.insert(func_ident.to_string(), func_index);
                // let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
                let mut func_translater = FunctionTranslater {
                    module: &mut self.module,
                    functions: &mut self.functions,
                    func_index: &mut self.func_index,
                    variable_index: &mut self.variable_index,
                    variables: &mut self.variables,
                    stack: &mut self.stack,
                };
                func_translater
                    .compile_func(tokens, tokens_iter, func_ident, func_export)
                    .unwrap();
                // builder.finalize();
                // self.module.
                //     define_function(*self.functions.get(func_ident).unwrap(), &mut self.ctx)
                //     .unwrap();
                // self.module.clear_context(&mut self.ctx);
            }
            EOF => {
                tokens_iter.reset();
                return Ok(());
            }
            _ => {}
        }
        tokens_iter.next(tokens);
        self.compile_funcs(tokens, tokens_iter)?;
        // for (ident, id) in &self.functions {
        //     self.module
        //         .define_function(*id, &mut self.ctx)
        //         .unwrap()
        // }
        self.module.finalize_definitions().unwrap();
        Ok(())
    }

    fn compile(
        &mut self,
        tokens: &mut Vec<Token>,
        tokens_iter: &mut TokenIter,
    ) -> Result<*const u8, Error> {
        // find funcs
        self.compile_funcs(tokens, tokens_iter)?;
        let start_func_id = self.functions.get(&"start".to_string()).unwrap();
        let code = self.module.get_finalized_function(*start_func_id);
        Ok(code)
    }

    pub fn run_code<O>(
        &mut self,
        tokens: &mut Vec<Token>,
        tokens_iter: &mut TokenIter,
    ) -> Result<O, String> {
        unsafe {
            let code_ptr = self.compile(tokens, tokens_iter).unwrap();
            let code_fn = mem::transmute::<_, fn() -> O>(code_ptr);
            Ok(code_fn())
        }
    }

    // pub fn run_code<I, O>(jit: &mut JIT, )
    // pub fn compile(&mut self, tokens: &mut Vec<Token>, token_iter: &mut TokenIter) -> Result<*const u8, String> {

    // }
}

struct FunctionTranslater<'a> {
    module: &'a mut JITModule,
    functions: &'a mut HashMap<String, FuncId>,
    func_index: &'a mut HashMap<String, usize>,
    variables: &'a mut HashMap<String, Variable>,
    variable_index: &'a mut usize,
    stack: &'a mut Vec<Value>,
}
macro_rules! compile_operation {
    (for int $type:ty, $operation:ident, $rhs:ident, $lhs:ident, $builder:ident) => {
        match $operation {
            NumericeOp::Add => $builder.ins().iadd($lhs, $rhs),
            NumericeOp::Sub => $builder.ins().isub($lhs, $rhs),
            NumericeOp::Mul => $builder.ins().imul($lhs, $rhs),
            NumericeOp::Div => $builder.ins().sdiv($lhs, $rhs),
            NumericeOp::Rem => $builder.ins().srem($lhs, $rhs),
            NumericeOp::And => $builder.ins().band($lhs, $rhs),
            NumericeOp::Or => $builder.ins().bor($lhs, $rhs),
            NumericeOp::Xor => $builder.ins().bxor($lhs, $rhs),
            NumericeOp::Eq => $builder.ins().icmp(IntCC::Equal, $lhs, $rhs),
            NumericeOp::Neq => $builder.ins().icmp(IntCC::NotEqual, $lhs, $rhs),
            NumericeOp::Gte => $builder
                .ins()
                .icmp(IntCC::SignedGreaterThanOrEqual, $lhs, $rhs),
            NumericeOp::Gt => $builder.ins().icmp(IntCC::SignedGreaterThan, $lhs, $rhs),
            NumericeOp::Lte => $builder
                .ins()
                .icmp(IntCC::SignedLessThanOrEqual, $lhs, $rhs),
            NumericeOp::Lt => $builder.ins().icmp(IntCC::UnsignedLessThan, $lhs, $rhs),
            NumericeOp::Max => $builder.ins().smax($lhs, $rhs),
            NumericeOp::Min => $builder.ins().smin($lhs, $rhs),
        }
    };
}
impl<'a> FunctionTranslater<'a> {
    fn compile_func(
        &mut self,
        // builder: &mut FunctionBuilder,
        tokens: &Vec<Token>,
        tokens_iter: &mut TokenIter,
        func_ident: &String,
        func_export: bool,
    ) -> Result<(), Error> {
        // println!("func ident: {:?}", &func_ident);
        let mut context = self.module.make_context();
        let mut builder_context = FunctionBuilderContext::new();

        context
            .func
            .signature
            .returns
            .push(AbiParam::new(types::I64));

        let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        tokens_iter.index = *self.func_index.get(func_ident).unwrap();
        let mut blocks: HashMap<String, Block> = HashMap::new();
        let mut current_block = func_ident;
        // blocks.insert(current_block.to_string(), entry_block);
        // let mut current_func = builder.func.clone();

        let chekpoint = tokens_iter.index;
        while !matches!(tokens_iter.peek(tokens).token_type, Word(EndFunc)) {
            let token = tokens_iter.next(tokens);
            if let LabelIdent(ident) = &token.token_type {
                let block = builder.create_block();
                blocks.insert(ident.to_string(), block);
            }
        }
        tokens_iter.index = chekpoint;
        while !matches!(tokens_iter.peek(tokens).token_type, Word(EndFunc)) {
            let mut token = tokens_iter.next(tokens);
            match &token.token_type {
                LabelIdent(ident) => {
                    let block = blocks.get(ident).unwrap();
                    if !matches!(tokens[tokens_iter.index - 2].token_type, Word(Jmp) | Word(Jpz)) {
                        builder.ins().jump(*block, &[]);
                    }
                    builder.switch_to_block(*block);
                    current_block = &ident;
                    // builder.seal_block(*prev_block);
                }
                Word(Insert) => todo!(),
                Word(Grow) => todo!(),
                Word(Remove) => todo!(),
                Word(Pop) => {
                    self.stack.pop();
                }
                Word(Call) => {
                    token = tokens_iter.next(tokens);
                    if let FuncIdent(ref ident) = token.token_type {
                        let callee_id;
                        if let Some(id) = self.functions.get(ident) {
                            callee_id = id;
                        } else {
                            self.compile_func(tokens, tokens_iter, ident, func_export)?;
                            callee_id = self.functions.get(ident).unwrap()
                            // .declare_function(&ident, linkage, signature)
                        }
                        let callee_ref = self.module.declare_func_in_func(*callee_id, builder.func);
                        let call = builder.ins().call(callee_ref, &[]);
                        let return_vals = builder.inst_results(call);
                        for val in return_vals {
                            self.stack.push(*val);
                        }
                    } else {
                        return Err(SyntaxError::Expected {
                            expected_token: "function ident".to_string(),
                            found: token.clone(),
                        })?;
                    }

                    // self.module.func
                    // builder.ins().call(FN, args)
                }
                Word(Return) => {
                    let val = self.stack.pop().unwrap();
                    builder.ins().return_(&[val]);
                }
                Word(Jmp) => {
                    token = tokens_iter.next(tokens);
                    // builder.seal_block(*blocks.get(current_block).unwrap());
                    let else_block;
                    let mut should_switch = true;
                    let block_name = format!("elseL{}C{}", token.line, token.column);
                    if let LabelIdent(ref ident) = tokens_iter.peek(tokens).token_type {
                        else_block = *blocks.get(ident).unwrap();
                        should_switch = false
                    } else {
                        else_block = builder.create_block();
                        blocks.insert(block_name, else_block);
                    }

                    // // let binding = &block_name;
                    // blocks.insert(, else_block);

                    let condition = self.stack.pop().unwrap();
                    match &token.token_type {
                        StringLit(ident) => {
                            let if_block = blocks.get(ident).cloned().unwrap();
                            builder
                                .ins()
                                .brif(condition, if_block, &[], else_block, &[]);
                        }
                        TokenIndex(index) => {
                            todo!()
                        }
                        _ => panic!("expected stringlit or token index"),
                    }
                    if should_switch {
                        builder.switch_to_block(else_block);
                        // builder.seal_block(else_block);
                    }
                }
                Word(Jpz) => {
                    token = tokens_iter.next(tokens);
                    // builder.seal_block(*blocks.get(current_block).unwrap());
                    let else_block;
                    let mut should_switch = true;
                    let block_name = format!("elseL{}C{}", token.line, token.column);
                    if let LabelIdent(ref ident) = tokens_iter.peek(tokens).token_type {
                        else_block = *blocks.get(ident).unwrap();
                        should_switch = false
                    } else {
                        else_block = builder.create_block();
                        blocks.insert(block_name, else_block);
                    }

                    // // let binding = &block_name;
                    // blocks.insert(, else_block);

                    let condition = self.stack.pop().unwrap();
                    match &token.token_type {
                        StringLit(ident) => {
                            let if_block = blocks.get(ident).cloned().unwrap();
                            builder
                                .ins()
                                .brif(condition, else_block, &[], if_block, &[]);
                        }
                        TokenIndex(index) => {
                            todo!()
                        }
                        _ => panic!("expected stringlit or token index"),
                    }
                    if should_switch {
                        builder.switch_to_block(else_block);
                        // builder.seal_block(else_block);
                    }
                }
                Word(I128) => {
                    self.compile_numeric_instruction(
                        tokens,
                        tokens_iter,
                        NumericType::I128,
                        &mut builder,
                    )?;
                }
                Word(I64) => {
                    self.compile_numeric_instruction(
                        tokens,
                        tokens_iter,
                        NumericType::I64,
                        &mut builder,
                    )?;
                }
                Word(I32) => {
                    self.compile_numeric_instruction(
                        tokens,
                        tokens_iter,
                        NumericType::I32,
                        &mut builder,
                    )?;
                }
                Word(I16) => {
                    self.compile_numeric_instruction(
                        tokens,
                        tokens_iter,
                        NumericType::I16,
                        &mut builder,
                    )?;
                }
                Word(I8) => {
                    self.compile_numeric_instruction(
                        tokens,
                        tokens_iter,
                        NumericType::I8,
                        &mut builder,
                    )?;
                }
                Word(F64) => {
                    self.compile_numeric_instruction(
                        tokens,
                        tokens_iter,
                        NumericType::F64,
                        &mut builder,
                    )?;
                }
                Word(F32) => {
                    self.compile_numeric_instruction(
                        tokens,
                        tokens_iter,
                        NumericType::F32,
                        &mut builder,
                    )?;
                }

                Word(Set) => {
                    let peeked = tokens_iter.peek(tokens);
                    if let Word(Stack) = peeked.token_type {
                        todo!("handle set stack")
                    } else {
                        token = tokens_iter.next(tokens);
                        let value =
                            self.stack
                                .pop()
                                .ok_or_else(|| RuntimeError::PoppingEmptyStack {
                                    popped_into: token.clone(),
                                })?;
                        match &token.token_type {
                            VarIdent(ident) => {
                                if let Some(var) = self.variables.get(ident) {
                                    builder.def_var(*var, value);
                                } else {
                                    Err(SemanticError::UndefinedVar {
                                        ident: token.clone(),
                                    })?;
                                }
                            }
                            TokenIndex(_) => todo!(),
                            _ => {
                                return Err(SyntaxError::Expected {
                                    expected_token: "variable".to_string(),
                                    found: token.clone(),
                                }
                                .into());
                            }
                        }
                    }
                }
                Word(Get) => {
                    token = tokens_iter.next(tokens);
                    match &token.token_type {
                        VarIdent(ident) => {
                            if let Some(var) = self.variables.get(ident) {
                                let value = builder.use_var(*var);
                                self.stack.push(value);
                            } else {
                                return Err(SemanticError::UndefinedVar {
                                    ident: token.clone(),
                                })?;
                            }
                        }
                        TokenIndex(_) => todo!(),
                        _ => {
                            return Err(SemanticError::UndefinedVar {
                                ident: token.clone(),
                            })?;
                        }
                    }
                }
                Word(Cpy) => todo!(),
                Word(Cast) => {
                    token = tokens_iter.next(tokens);
                    match &token.token_type {
                        Word(I8) => {
                            let val = self.stack.pop().unwrap();
                            let new_val = builder.ins().bitcast(types::I8, MemFlags::new(), val);
                            self.stack.push(new_val);
                        }
                        Word(I16) => {
                            let val = self.stack.pop().unwrap();
                            let new_val = builder.ins().bitcast(types::I16, MemFlags::new(), val);
                            self.stack.push(new_val);
                        }
                        Word(I32) => {
                            let val = self.stack.pop().unwrap();
                            let new_val = builder.ins().bitcast(types::I32, MemFlags::new(), val);
                            self.stack.push(new_val);
                        }
                        Word(I64) => {
                            let val = self.stack.pop().unwrap();
                            let new_val = builder.ins().bitcast(types::I64, MemFlags::new(), val);
                            self.stack.push(new_val);
                        }
                        _ => panic!("Expected castble type")
                    }
                },
                _ => {
                    return Err(SyntaxError::Expected {
                        expected_token: "function instruction".to_string(),
                        found: token.clone(),
                    })?;
                }
            }
        }

        builder.seal_all_blocks();
        builder.finalize();
        let func_id = self
            .module
            .declare_function(func_ident, Linkage::Export, &context.func.signature)
            .map_err(|e| panic!("{}", e.to_string()))
            .unwrap();

        self.functions.insert(func_ident.to_string(), func_id);

        self.module
            .define_function(func_id, &mut context)
            .map_err(|e| panic!("{}", e.to_string()))
            .unwrap();

        self.module.clear_context(&mut context);
        Ok(())
    }

    fn compile_numeric_instruction(
        &mut self,
        tokens: &Vec<Token>,
        tokens_iter: &mut TokenIter,
        num_type: NumericType,
        builder: &mut FunctionBuilder,
    ) -> Result<(), Error> {
        let mut token = tokens_iter.next(tokens);
        match token.token_type {
            Word(Declare) => {
                while matches!(tokens_iter.peek(tokens).token_type, VarIdent(_)) {
                    token = tokens_iter.next(tokens);
                    if let VarIdent(ident) = &token.token_type {
                        let var = Variable::new(*self.variable_index);
                        match num_type {
                            NumericType::I128 => todo!(),
                            NumericType::I64 => {
                                builder.declare_var(var, types::I64);
                            }
                            NumericType::I32 => {
                                builder.declare_var(var, types::I32);
                            }
                            NumericType::I16 => {
                                builder.declare_var(var, types::I16);
                            }
                            NumericType::I8 => {
                                builder.declare_var(var, types::I8);
                            }
                            NumericType::F32 => {
                                builder.declare_var(var, types::F32);
                            }
                            NumericType::F64 => {
                                builder.declare_var(var, types::F64);
                            }
                        }
                        self.variables.insert(ident.clone(), var);
                        *self.variable_index += 1;
                    } else {
                        return Err(SyntaxError::Expected {
                            expected_token: "variable ident".to_string(),
                            found: token.clone(),
                        })?;
                    }
                }
            }
            Word(Push) => {
                while matches!(tokens_iter.peek(tokens).token_type, Number(_)) {
                    token = tokens_iter.next(tokens);
                    if let Number(num) = token.token_type {
                        match num_type {
                            NumericType::I128 => {
                                todo!()
                            }
                            NumericType::I64 => {
                                let value = builder.ins().iconst(types::I64, num as i64);
                                self.stack.push(value);
                            }
                            NumericType::I32 => {
                                let value = builder.ins().iconst(types::I32, num as i64);
                                self.stack.push(value);
                            }
                            NumericType::I16 => {
                                let value = builder.ins().iconst(types::I16, num as i64);
                                self.stack.push(value);
                            }
                            NumericType::I8 => {
                                let value = builder.ins().iconst(types::I8, num as i64);
                                self.stack.push(value);
                            }
                            NumericType::F32 => {
                                let float = parse_float32(tokens_iter, num, tokens)?;
                                let value = builder.ins().f32const(float);
                                self.stack.push(value);
                            }
                            NumericType::F64 => {
                                let float = parse_float64(tokens_iter, num, tokens)?;
                                let value = builder.ins().f64const(float);
                                self.stack.push(value);
                            }
                        }
                    }
                }
            }
            Word(Add) => self.compile_numerical_op(num_type, NumericeOp::Add, builder)?,
            Word(Sub) => self.compile_numerical_op(num_type, NumericeOp::Sub, builder)?,
            Word(Mul) => self.compile_numerical_op(num_type, NumericeOp::Mul, builder)?,
            Word(Div) => self.compile_numerical_op(num_type, NumericeOp::Div, builder)?,
            Word(Rem) => self.compile_numerical_op(num_type, NumericeOp::Rem, builder)?,
            Word(And) => self.compile_numerical_op(num_type, NumericeOp::And, builder)?,
            Word(Or) => self.compile_numerical_op(num_type, NumericeOp::Or, builder)?,
            Word(Xor) => self.compile_numerical_op(num_type, NumericeOp::Xor, builder)?,
            Word(Eq) => self.compile_numerical_op(num_type, NumericeOp::Eq, builder)?,
            Word(Neq) => self.compile_numerical_op(num_type, NumericeOp::Neq, builder)?,
            Word(Gte) => self.compile_numerical_op(num_type, NumericeOp::Gte, builder)?,
            Word(Gt) => self.compile_numerical_op(num_type, NumericeOp::Gt, builder)?,
            Word(Lte) => self.compile_numerical_op(num_type, NumericeOp::Lte, builder)?,
            Word(Lt) => self.compile_numerical_op(num_type, NumericeOp::Lt, builder)?,
            Word(Max) => self.compile_numerical_op(num_type, NumericeOp::Max, builder)?,
            Word(Min) => self.compile_numerical_op(num_type, NumericeOp::Min, builder)?,
            Word(Cpy) => todo!(),
            _ => Err(SyntaxError::Expected {
                expected_token: "numeric operation".to_string(),
                found: token.clone(),
            })?,
        }
        Ok(())
    }

    fn compile_numerical_op(
        &mut self,
        num_type: NumericType,
        op: NumericeOp,
        builder: &mut FunctionBuilder,
    ) -> Result<(), Error> {
        let rhs = self
            .stack
            .pop()
            .expect("Tried popping from empty stack during numerical operation");
        let lhs = self
            .stack
            .pop()
            .expect("Tried popping from empty stack during numerical operation");

        match num_type {
            NumericType::I128 => {
                todo!()
            }
            NumericType::I64 => {
                // builder.ins().or
                self.stack.push(compile_operation! {
                    for int i64,
                    op, rhs, lhs, builder
                });
            }
            NumericType::I32 => {
                self.stack.push(compile_operation! {
                    for int i32,
                    op, rhs, lhs, builder
                });
            }
            NumericType::I16 => {
                self.stack.push(compile_operation! {
                    for int i16,
                    op, rhs, lhs, builder
                });
            }
            NumericType::I8 => {
                self.stack.push(compile_operation! {
                    for int i8,
                    op, rhs, lhs, builder
                });
            }
            NumericType::F32 => todo!(),
            NumericType::F64 => todo!(),
        }
        Ok(())
    }
}

pub unsafe fn run(tokens: &mut Vec<Token>) -> Result<(), Error> {
    unsafe {
        let mut jit = JIT::new();
        let mut tokens_iter = TokenIter { index: 0 };
        let result: i64 = jit.run_code(tokens, &mut tokens_iter).unwrap();
        println!("jit result: {result}");
        Ok(())
    }
}
