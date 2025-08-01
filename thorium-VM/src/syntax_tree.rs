#[derive(Debug, Clone)]
pub struct NodeFunc {
    pub ident: String,
    pub export_name: Option<String>,
    pub intructions: Vec<NodeIntruction>,
}

#[derive(Debug, Clone)]
pub enum NodeIntruction {
    Push(NodeValue),
    Pop,
    Call(String),
    Return,
    Add(NodeType),
    Sub(NodeType),
    Div(NodeType),
    Mul(NodeType),
    Declare { variable: NodeVariable },
    Set { variable: NodeVariable },
    Get { variable: NodeVariable },
}

#[derive(Debug, Clone, Copy)]
pub enum NodeValue {
    I32(i32),
    I64(i64),
    I16(i16),
    I8(i8),
    F32(f32),
    F64(f64),
}

#[derive(Debug, Clone)]
pub enum NodeType {
    I32,
    I64,
    I16,
    I8,
    F32,
    F64,
}

#[derive(Debug, Clone)]
pub struct NodeProgram {
    pub funcs: Vec<NodeFunc>,
}

#[derive(Debug, Clone)]
pub struct NodeVariable {
    pub ident: String,
}
