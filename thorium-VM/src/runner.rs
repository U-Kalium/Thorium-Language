use std::collections::HashMap;

use crate::{parser::parse_tokens, syntax_tree::{self, NodeFunc, NodeIntruction, NodeProgram, NodeType, NodeValue}, tokenizer::tokenize};

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

enum InstructionReturn {
    Some(NodeValue),
    None,
    Jump(String)
}

impl NodeFunc {
    fn run(&self, state: &mut State) -> Option<NodeValue> {
        let mut index = 0;
        while index < self.intructions.len() {
            match self.intructions[index].run(state) {
                InstructionReturn::Some(val) => return Some(val),
                InstructionReturn::None => {},
                InstructionReturn::Jump(label) => {
                    if let Some(label_index) =  self.labels.get(&label){
                        index = *label_index;
                        continue;
                    }
                }
            }
            index += 1;
        }
        None

        // for instruction in &self.intructions {
        //     match instruction.run(state) {
        //         Some(val) => return Some(val),
        //         None => {}
        //     }
        // }
        // None
    }
}

impl NodeIntruction {
    fn run(&self, state: &mut State) -> InstructionReturn { 
        match self {
            NodeIntruction::Push(node_value) => {

                state.stack.push(node_value.clone());
                InstructionReturn::None
            }
            NodeIntruction::Call(ident) => {
                let state_clone = state.clone();
                let func = state_clone
                    .functions
                    .get(ident)
                    .expect(&format!("could not find function {:?}", ident));
                func.run(state);
                InstructionReturn::None
            }
            NodeIntruction::Return => match state.stack.pop() {
                Some(val) => InstructionReturn::Some(val),
                None => InstructionReturn::None,
            },
            NodeIntruction::Declare { variable } => {
                let ident = variable.ident.clone();
                if state.variables.contains_key(&ident) {
                    panic!("Declaration of variable {:?} even though it exists", ident)
                }

                state.variables.insert(ident, None);
                InstructionReturn::None
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
                InstructionReturn::None
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
                InstructionReturn::None
            }
            NodeIntruction::Add(node_type) => {
                process_numerical_op(state, node_type.clone(), NumericeOp::Add);
                InstructionReturn::None
            }
            NodeIntruction::Sub(node_type) => {
                process_numerical_op(state, node_type.clone(), NumericeOp::Sub);
                InstructionReturn::None
            }
            NodeIntruction::Mul(node_type) => {
                process_numerical_op(state, node_type.clone(), NumericeOp::Mul);
                InstructionReturn::None
            }
            NodeIntruction::Div(node_type) => {
                process_numerical_op(state, node_type.clone(), NumericeOp::Div);
                InstructionReturn::None
            }
            NodeIntruction::Rem(node_type) => {
                process_numerical_op(state, node_type.clone(), NumericeOp::Rem );
                InstructionReturn::None
            }
            NodeIntruction::And(node_type) => {
                process_numerical_op(state, node_type.clone(), NumericeOp::And );
                InstructionReturn::None
            }
            NodeIntruction::Or(node_type) => {
                process_numerical_op(state, node_type.clone(), NumericeOp::Or );
                InstructionReturn::None
            }
            NodeIntruction::Xor(node_type) => {
                process_numerical_op(state, node_type.clone(), NumericeOp::Xor );
                InstructionReturn::None
            }
            NodeIntruction::Eq(node_type) => {
                process_numerical_op(state, node_type.clone(), NumericeOp::Eq );
                InstructionReturn::None
            }
            NodeIntruction::Neq(node_type) => {
                process_numerical_op(state, node_type.clone(), NumericeOp::Neq );
                InstructionReturn::None
            }
            NodeIntruction::Gte(node_type) => {
                process_numerical_op(state, node_type.clone(), NumericeOp::Gte );
                InstructionReturn::None
            }
            NodeIntruction::Gt(node_type) => {
                process_numerical_op(state, node_type.clone(), NumericeOp::Gt );
                InstructionReturn::None
            }
            NodeIntruction::Lte(node_type) => {
                process_numerical_op(state, node_type.clone(), NumericeOp::Lte );
                InstructionReturn::None
            }
            NodeIntruction::Lt(node_type) => {
                process_numerical_op(state, node_type.clone(), NumericeOp::Lt );
                InstructionReturn::None
            }
            NodeIntruction::Max(node_type) => {
                process_numerical_op(state, node_type.clone(), NumericeOp::Max );
                InstructionReturn::None
            }
            NodeIntruction::Min(node_type) => {
                process_numerical_op(state, node_type.clone(), NumericeOp::Min );
                InstructionReturn::None
            }
            NodeIntruction::Pop => {
                if state.stack.pop().is_none() {
                    panic!("tried popping when the stack is empty")
                }
                InstructionReturn::None
            }
            NodeIntruction::Jmp(label) => {
                if let Some(value) = state.stack.last() {
                    if !value.is_zero() {
                        InstructionReturn::Jump(label.clone())
                    } else {
                        InstructionReturn::None
                    }
                } else {
                    panic!("tried popping when the stack is empty during jmp operation")
                }
            }
            NodeIntruction::Jpz(label) => {
                if let Some(value) = state.stack.last() {
                    if value.is_zero() {
                        InstructionReturn::Jump(label.clone())
                    } else {
                        InstructionReturn::None
                    }
                } else {
                    panic!("tried popping when the stack is empty during jpz operation")
                }
            }
        }
    }
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
                    for int i32,
                    operation, rhs_val, lhs_val
                )))
            } else {
                panic!("tried applying operation but rhs was {rhs:?} and lhs was {lhs:?}")
            }
        }
        NodeType::I64 => {
            if let (NodeValue::I64(rhs_val), NodeValue::I64(lhs_val)) = (rhs, lhs) {
                state.stack.push(NodeValue::I64(perform_operation!(
                    for int i64,
                    operation, rhs_val, lhs_val
                )))
            } else {
                panic!("tried applying operation but rhs was {rhs:?} and lhs was {lhs:?}")
            }
        }
        NodeType::I16 => {
            if let (NodeValue::I16(rhs_val), NodeValue::I16(lhs_val)) = (rhs, lhs) {
                state.stack.push(NodeValue::I16(perform_operation!(
                    for int i16,
                    operation, rhs_val, lhs_val
                )))
            } else {
                panic!("tried applying operation but rhs was {rhs:?} and lhs was {lhs:?}")
            }
        }
        NodeType::I8 => {
            if let (NodeValue::I8(rhs_val), NodeValue::I8(lhs_val)) = (rhs, lhs) {
                state.stack.push(NodeValue::I8(perform_operation!(
                    for int i8,
                    operation, rhs_val, lhs_val
                )))
            } else {
                panic!("tried applying operation but rhs was {rhs:?} and lhs was {lhs:?}")
            }
        }
        NodeType::F32 => {
            if let (NodeValue::F32(rhs_val), NodeValue::F32(lhs_val)) = (rhs, lhs) {
                state.stack.push(NodeValue::F32(perform_operation!(
                    for float f32,
                    operation, rhs_val, lhs_val
                )))
            } else {
                panic!("tried applying operation but rhs was {rhs:?} and lhs was {lhs:?}")
            }
        }
        NodeType::F64 => {
            if let (NodeValue::F64(rhs_val), NodeValue::F64(lhs_val)) = (rhs, lhs) {
                state.stack.push(NodeValue::F64(perform_operation!(
                    for float f64,
                    operation, rhs_val, lhs_val
                )))
            } else {
                panic!("tried applying operation but rhs was {rhs:?} and lhs was {lhs:?}")
            }
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

    pub fn run(&mut self) -> Option<NodeValue> {
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
                // println!("returned: {:?}", val)
                return Some(val)
            }
        }
        None
    }
}

pub fn run_state_from_str(string: &str) -> Option<NodeValue> {
    let tokens = tokenize(string.to_string()).unwrap();
    let syntax_tree = parse_tokens(&tokens);
    let mut state = State::new(syntax_tree);
    state.run()
}