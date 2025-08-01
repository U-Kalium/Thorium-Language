use std::collections::HashMap;

use crate::syntax_tree::{NodeFunc, NodeIntruction, NodeProgram, NodeType, NodeValue};

#[derive(Clone)]
pub struct State {
    program: NodeProgram,
    functions: HashMap<String, NodeFunc>,
    // i32_stack: Vec<i32>,
    // i64_stack: Vec<i64>,
    // i16_stack: Vec<i16>,
    // i8_stack: Vec<i8>,
    // f32_stack: Vec<f32>,
    // f64_stack: Vec<f64>,
    stack: Vec<NodeValue>,

    variables: HashMap<String, Option<NodeValue>>,
}

impl NodeFunc {
    fn run(&self, state: &mut State) -> Option<NodeValue> {
        for instruction in &self.intructions {
            match instruction.run(state) {
                Some(val) => return Some(val),
                None => {}
            }
        }
        None
    }
}

impl NodeIntruction {
    fn run(&self, state: &mut State) -> Option<NodeValue> {
        match self {
            NodeIntruction::Push(node_value) => {
                state.stack.push(node_value.clone());
                None
            }
            NodeIntruction::Call(ident) => {
                let state_clone = state.clone();
                let func = state_clone
                    .functions
                    .get(ident)
                    .expect(&format!("could not find function {:?}", ident));
                func.run(state);
                None
            }
            NodeIntruction::Return => match state.stack.pop() {
                Some(val) => Some(val),
                None => None,
            },
            NodeIntruction::Declare { variable } => {
                let ident = variable.ident.clone();
                if state.variables.contains_key(&ident) {
                    panic!("Declaration of variable {:?} even though it exists", ident)
                }

                state.variables.insert(ident, None);
                None
            }
            NodeIntruction::Set { variable } => {
                let ident = variable.ident.clone();
                if let Some(variable_val) = state.variables.get_mut(&ident) {
                    if let Some(stack_val) = state.stack.pop() {
                        *variable_val = Some(stack_val)
                    } else {
                        panic!("Tried Poping stack when it was empty")
                    }
                }
                None
            }
            NodeIntruction::Get { variable } => {
                let ident = variable.ident.clone();
                if let Some(variable_val) = state.variables.get(&ident) {
                    if let Some(val) = variable_val {
                        state.stack.push(val.clone());
                    } else {
                        panic!("Variable {}, is empty yet tried to get from it", ident)
                    }
                }
                None
            }
            NodeIntruction::Add(node_type) => {
                process_numerical_op(state, node_type.clone(), NumericeOp::Add);
                None
            }
            NodeIntruction::Sub(node_type) => {
                process_numerical_op(state, node_type.clone(), NumericeOp::Sub);
                None
            }
            NodeIntruction::Mul(node_type) => {
                process_numerical_op(state, node_type.clone(), NumericeOp::Mul);
                None
            }
            NodeIntruction::Div(node_type) => {
                process_numerical_op(state, node_type.clone(), NumericeOp::Div);
                None
            }
            NodeIntruction::Pop => {
                if state.stack.pop().is_none() {
                    panic!("tried popping when the stack is empty")
                }
                None
            }
        }
    }
}

enum NumericeOp {
    Add,
    Sub,
    Mul,
    Div,
}

macro_rules! perform_operation {
    ($operation:ident, $rhs:ident, $lhs:ident) => {
        match $operation {
            NumericeOp::Add => $lhs + $rhs,
            NumericeOp::Sub => $lhs - $rhs,
            NumericeOp::Mul => $lhs * $rhs,
            NumericeOp::Div => $lhs / $rhs,
        }
    };
}

fn process_numerical_op(state: &mut State, node_type: NodeType, operation: NumericeOp) {
    let rhs = state
        .stack
        .pop()
        .expect("Tried popping from empty track during numerical operation");
    let lhs = state
        .stack
        .pop()
        .expect("Tried popping from empty track during numerical operation");

    match node_type {
        NodeType::I32 => {
            if let (NodeValue::I32(rhs_val), NodeValue::I32(lhs_val)) = (rhs, lhs) {
                state.stack.push(NodeValue::I32(perform_operation!(
                    operation, rhs_val, lhs_val
                )))
            } else {
                panic!("tried applying operation but rhs was {rhs:?} and lhs was {lhs:?}")
            }
        }
        NodeType::I64 => {
            if let (NodeValue::I64(rhs_val), NodeValue::I64(lhs_val)) = (rhs, lhs) {
                state.stack.push(NodeValue::I64(perform_operation!(
                    operation, rhs_val, lhs_val
                )))
            } else {
                panic!("tried applying operation but rhs was {rhs:?} and lhs was {lhs:?}")
            }
        }
        NodeType::I16 => {
            if let (NodeValue::I16(rhs_val), NodeValue::I16(lhs_val)) = (rhs, lhs) {
                state.stack.push(NodeValue::I16(perform_operation!(
                    operation, rhs_val, lhs_val
                )))
            } else {
                panic!("tried applying operation but rhs was {rhs:?} and lhs was {lhs:?}")
            }
        }
        NodeType::I8 => {
            if let (NodeValue::I8(rhs_val), NodeValue::I8(lhs_val)) = (rhs, lhs) {
                state.stack.push(NodeValue::I8(perform_operation!(
                    operation, rhs_val, lhs_val
                )))
            } else {
                panic!("tried applying operation but rhs was {rhs:?} and lhs was {lhs:?}")
            }
        }
        NodeType::F32 => {
            if let (NodeValue::F32(rhs_val), NodeValue::F32(lhs_val)) = (rhs, lhs) {
                state.stack.push(NodeValue::F32(perform_operation!(
                    operation, rhs_val, lhs_val
                )))
            } else {
                panic!("tried applying operation but rhs was {rhs:?} and lhs was {lhs:?}")
            }
        }
        NodeType::F64 => {
            if let (NodeValue::F64(rhs_val), NodeValue::F64(lhs_val)) = (rhs, lhs) {
                state.stack.push(NodeValue::F64(perform_operation!(
                    operation, rhs_val, lhs_val
                )))
            } else {
                panic!("tried applying operation but rhs was {rhs:?} and lhs was {lhs:?}")
            }
        }
    }

    match (rhs, lhs) {
        (NodeValue::F32(rhs_val), NodeValue::F32(lhs_val)) => state.stack.push(NodeValue::F32(
            perform_operation!(operation, rhs_val, lhs_val),
        )),
        (NodeValue::F64(rhs_val), NodeValue::F64(lhs_val)) => state.stack.push(NodeValue::F64(
            perform_operation!(operation, rhs_val, lhs_val),
        )),
        (NodeValue::I32(rhs_val), NodeValue::I32(lhs_val)) => state.stack.push(NodeValue::I32(
            perform_operation!(operation, rhs_val, lhs_val),
        )),
        (NodeValue::I64(rhs_val), NodeValue::I64(lhs_val)) => state.stack.push(NodeValue::I64(
            perform_operation!(operation, rhs_val, lhs_val),
        )),
        (NodeValue::I16(rhs_val), NodeValue::I16(lhs_val)) => state.stack.push(NodeValue::I16(
            perform_operation!(operation, rhs_val, lhs_val),
        )),
        (NodeValue::I8(rhs_val), NodeValue::I8(lhs_val)) => state.stack.push(NodeValue::I8(
            perform_operation!(operation, rhs_val, lhs_val),
        )),
        (rhs_val, lhs_val) => {
            panic!("tried applying operation but rhs was {rhs_val:?} and lhs was {lhs_val:?}")
        }
    }
}

impl State {
    pub fn new(program: NodeProgram) -> Self {
        Self {
            program,
            functions: HashMap::new(),
            stack: Vec::new(),
            variables: HashMap::new(),
        }
    }

    pub fn run(&mut self) {
        let mut entry_point = None;
        let program = self.program.clone();

        for func in &program.funcs {
            self.functions.insert(func.ident.clone(), func.clone());
            if func.export_name == Some("_start".to_string()) {
                entry_point = Some(func)
            }
        }
        if let Some(func) = entry_point {
            if let Some(val) = func.run(self) {
                println!("returned: {:?}", val)
            }
        }
    }
}
