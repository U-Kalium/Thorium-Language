mod tokenizer;
mod dynasm_jit;
mod run_byte_code;

use run_byte_code::*;

#[derive(Debug)]
pub enum Error {
    MemoryError(MemoryErrors), 
    SyntacError(SyntaxError),
    SemanticError(SemanticError),
    RuntimeError(RuntimeError),
}

pub enum RunOptions {
    CraneliftJit,
    DynasmJit,
    Interpret
}

impl From<SyntaxError> for Error {
    fn from(value: SyntaxError) -> Self {
        Self::SyntacError(value)
    }
}
impl From<MemoryErrors> for Error {
    fn from(value: MemoryErrors) -> Self {
        Self::MemoryError(value)
    }
}
impl From<SemanticError> for Error {
    fn from(value: SemanticError) -> Self {
        Self::SemanticError(value)
    }
}
impl From<RuntimeError> for Error {
    fn from(value: RuntimeError) -> Self {
        Self::RuntimeError(value)
    }
}

pub fn run(byte_code: String) -> Result<(), Error> {
    let mut tokens = tokenizer::tokenize(byte_code).unwrap();
    dynasm_jit::run(&mut tokens)
}