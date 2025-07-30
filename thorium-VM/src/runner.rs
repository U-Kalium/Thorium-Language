use std::collections::HashMap;

use crate::syntax_tree::{NodeFunc, NodeIntruction, NodeProgram, NodeType, NodeValue};

#[derive(Clone)]
pub struct State {
    program: NodeProgram,
    functions: HashMap<String, NodeFunc>,
    i32_stack: Vec<i32>,
    i64_stack: Vec<i64>,
    i16_stack: Vec<i16>,
    i8_stack: Vec<i8>,

    variables: HashMap<String, Option<NodeValue>>
}

impl NodeFunc {
    fn run(&self, state: &mut State) -> Option<NodeValue> {
        for instruction in &self.intructions {
            match instruction.run(state) {
                Some(val) => return Some(val),
                None => {},
            }
        }
        None
    }
}

impl NodeIntruction {
    fn run(&self, state: &mut State) -> Option<NodeValue> {
        match self {
            NodeIntruction::Const(node_value) => {
                        match node_value {
                            NodeValue::I32(val) => state.i32_stack.push(*val),
                            NodeValue::I64(val) => state.i64_stack.push(*val),
                            NodeValue::I16(val) => state.i16_stack.push(*val),
                            NodeValue::I8(val) => state.i8_stack.push(*val),
                        };
                        None
                    },
            NodeIntruction::Call(ident) => {
                        let state_clone = state.clone();
                        let func = state_clone.functions.get(ident).expect(
                                &format!("could not find function {:?}", ident)
                            );
                        func.run(state);
                        None
                    },
            NodeIntruction::Return(node_type) => {
                        match node_type {
                            NodeType::I32 => {
                                match state.i32_stack.pop() {
                                    Some(val) => Some(NodeValue::I32(val)),
                                    None => None,
                                }
                            }
                            NodeType::I64 => {
                                match state.i64_stack.pop() {
                                    Some(val) => Some(NodeValue::I64(val)),
                                    None => None,
                                }
                            }
                            NodeType::I16 => {
                                match state.i16_stack.pop() {
                                    Some(val) => Some(NodeValue::I16(val)),
                                    None => None,
                                }
                            }
                            NodeType::I8 => {
                                match state.i8_stack.pop() {
                                    Some(val) => Some(NodeValue::I8(val)),
                                    None => None,
                                }
                            }
                        }
                    },
            NodeIntruction::Declare {
                variable, 
                node_type 
            } => {
                let ident = variable.ident.clone();
                if state.variables.contains_key(&ident) {
                    panic!("Declaration of variable {:?} even though it exists", ident)
                }

                state.variables.insert(ident, None);
                None
            },
            NodeIntruction::Set { 
                variable, 
                node_type
            } => {
                let ident = variable.ident.clone();
                if let Some(mut variable_val) = state.variables.get_mut(&ident) {
                    match node_type {
                        NodeType::I32 => {
                            if let Some(stack_val) = state.i32_stack.pop() {
                                *variable_val = Some(NodeValue::I32(stack_val))
                            } else {
                                panic!("Tried Poping stack when it was empty")
                            }
                        },
                        NodeType::I64 => {
                            if let Some(stack_val) = state.i64_stack.pop() {
                                *variable_val = Some(NodeValue::I64(stack_val))
                            } else {
                                panic!("Tried Poping stack when it was empty")
                            }
                        },
                        NodeType::I16 => {
                            if let Some(stack_val) = state.i16_stack.pop() {
                                *variable_val = Some(NodeValue::I16(stack_val))
                            } else {
                                panic!("Tried Poping stack when it was empty")
                            }
                        },
                        NodeType::I8 => {
                            if let Some(stack_val) = state.i8_stack.pop() {
                                *variable_val = Some(NodeValue::I8(stack_val))
                            } else {
                                panic!("Tried Poping stack when it was empty")
                            }
                        },
                    }
                }
                None
            },
            NodeIntruction::Get { 
                variable, 
                node_type
            } => {
                let ident = variable.ident.clone();
                if let Some(variable_val) = state.variables.get(&ident) {
                    if let Some(val) = variable_val {
                        match node_type {
                            NodeType::I32 => {
                                if let NodeValue::I32(val) = val {
                                    state.i32_stack.push(*val);
                                } else {
                                    panic!("Tried assigning type {:?} in variable of type i32", node_type)
                                }
                            },
                            NodeType::I64 => {
                                if let NodeValue::I64(val) = val {
                                    state.i64_stack.push(*val);
                                } else {
                                    panic!("Tried assigning type {:?} in variable of type i64", node_type)
                                }
                            },
                            NodeType::I16 => {
                                if let NodeValue::I16(val) = val {
                                    state.i16_stack.push(*val);
                                } else {
                                    panic!("Tried assigning type {:?} in variable of type i16", node_type)
                                }
                            },
                            NodeType::I8 => {
                                if let NodeValue::I8(val) = val {
                                    state.i8_stack.push(*val);
                                } else {
                                    panic!("Tried assigning type {:?} in variable of type i8", node_type)
                                }
                            },
                        }
                    } else {
                        panic!("Variable {}, is empty yet tried to get from it", ident)
                    }
                }  
                None
            },
        }
    }
}

impl State {
    pub fn new(program: NodeProgram) -> Self {
        Self {
            program,
            functions: HashMap::new(),
            i32_stack: Vec::new(),
            variables: HashMap::new(),
            i64_stack: Vec::new(),
            i16_stack: Vec::new(),
            i8_stack: Vec::new(),
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