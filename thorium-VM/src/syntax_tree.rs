use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct NodeFunc {
    pub ident: String,
    pub export_name: Option<String>,
    pub intructions: Vec<NodeIntruction>,
    pub labels: HashMap<String, usize>
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
    Rem(NodeType),
    And(NodeType),
    Or(NodeType),
    Xor(NodeType),
    Eq(NodeType),
    Neq(NodeType),
    Gte(NodeType),
    Gt(NodeType),
    Lte(NodeType),
    Lt(NodeType),
    Max(NodeType),
    Min(NodeType),
    Declare { variable: NodeVariable },
    Set { variable: NodeVariable },
    Get { variable: NodeVariable },
    Jmp(String),
    Jpz(String),
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum NodeValue {
    I32(i32),
    I64(i64),
    I16(i16),
    I8(i8),
    F32(f32),
    F64(f64),
}

impl NodeValue {
    pub fn is_zero(&self) -> bool {
        match self {
            NodeValue::I32(val) => *val == 0,
            NodeValue::I64(val) => *val == 0,
            NodeValue::I16(val) => *val == 0,
            NodeValue::I8(val) => *val == 0,
            NodeValue::F32(val) => *val == 0.0,
            NodeValue::F64(val) => *val == 0.0,
        }
    }
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
    pub variables: HashMap<String, Option<NodeValue>>
}

#[derive(Debug, Clone)]
pub struct NodeVariable {
    pub ident: String,
}
