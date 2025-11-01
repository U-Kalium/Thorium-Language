use std::sync::Arc;

use hashbrown::HashMap;

use crate::compiler::tokenizer::TokenIter;
use crate::compiler::tokenizer::TokenType::*;

static mut UNKOWN_TYPE_VAR_COUNTER: usize = 0;
fn next_unkown_type() -> usize {
    unsafe {
        UNKOWN_TYPE_VAR_COUNTER += 1;
        UNKOWN_TYPE_VAR_COUNTER
    }
}
#[derive(Debug, Clone)]
pub struct ScopeInfo {
    scope_level: u32,
    id: u32,
    shared_scopes: Vec<u32>,
}
impl ScopeInfo {
    fn new() -> Self {
        Self {
            scope_level: 0,
            id: 0,
            shared_scopes: Vec::new(),
        }
    }
    fn into_scope(&mut self) {
        self.scope_level += 1;
        self.id += 1;
        self.shared_scopes.push(self.id);
    }
    fn out_of_scope(&mut self) {
        self.scope_level -= 1;
        self.shared_scopes.pop();
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    ident: Option<Arc<str>>,
    param_type: Box<Type>,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Boolean,
    Void,
    Func {
        params: Arc<[Param]>,
        returns: Arc<[Box<Type>]>,
    },
    Unknown(u32),
    I64,
    I32,
    I16,
    I8,
    F64,
    F32,
}
#[derive(Debug, Clone)]
pub enum Value {
    I64(i64),
    I32(i32),
    I16(i16),
    I8(i8),
    F64(f64),
    F32(f32),
}

#[derive(Debug, Clone)]
pub enum Expr {
    VarDeclaration {
        ident: Arc<str>,
        var_type: Type,
        assignment: Option<Box<Expr>>,
    },
    Return {
        expr: Box<Expr>,
    },
    Finish {
        expr: Box<Expr>,
    },
    Value(Value),
    Variable {
        ident: Arc<str>,
        var_type: Type,
    },
    Block {
        scope_info: ScopeInfo,
        exprs: Vec<Expr>,
        block_type: Type,
    },
}

#[derive(Clone, Debug)]
struct VariableInfo {
    scope_id: u32,
    is_mutable: bool,
}

#[derive(Clone, Debug)]
struct Variables {
    variables: HashMap<(Arc<str>, Type), Vec<VariableInfo>>,
}
impl Variables {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }
    fn add_var(&mut self, var_info: VariableInfo, var_ident: &str, var_type: Type) {
        if let Some(vars) = self.variables.get_mut(&(var_ident.into(), var_type)) {
            vars.push(var_info.clone())
        }
    }
}

fn parse_expr(tokens: &mut TokenIter) -> Result<Expr, Arc<str>> {
    // let expression;

    let mut token = tokens.peek().unwrap();
    match &token.token_type {
        Let => parse_declaration(tokens),
        Var => parse_declaration(tokens),
        t => Err(format!("{:?} can not be in file base", t).into()),
    }
}

fn try_parse_type(tokens: &mut TokenIter) -> Option<Type> {
    let checkpoint = tokens.index;
    // let mut token = tokens.next().unwrap();

    // try function type sig
    let try_parse_func_type = |tokens: &mut TokenIter| {
        let mut token = tokens.next().unwrap();
        let mut params = vec![];
        let mut returns = vec![];
        if let OpenBracket = token.token_type {
            token = tokens.next().unwrap()
        }
        while !matches!(tokens.peek().unwrap().token_type, CloseBracket) {
            let mut ident: Option<Arc<str>> = None;
            let param_type;
            if let Ident(param) = token.token_type {
                ident = Some(param.into());
                tokens.next().unwrap();
            }
            if let Some(type_) = try_parse_type(tokens) {
                param_type = Box::new(type_)
            } else {
                tokens.index = checkpoint;
                return None;
            }
            token = tokens.next().unwrap();
            if let Comma = token.token_type {
            } else {
                if let CloseBracket = tokens.peek().unwrap().token_type {
                } else {
                    tokens.index = checkpoint;

                    return None;
                }
            }
            params.push(Param { ident, param_type });
        }
        let mut parsing_returns = true;
        while parsing_returns {
            if let Some(return_type) = try_parse_type(tokens) {
                returns.push(Box::new(return_type));
            } else {
                parsing_returns = false
            }
        }

        Some(Type::Func {
            params: params.into(),
            returns: returns.into(),
        })
    };
    let try_parse_primitive_type = |tokens: &mut TokenIter| {
        let token = tokens.next().unwrap();
        match token.token_type {
            Bool => Some(Type::Boolean),
            I64 => Some(Type::I64),
            I32 => Some(Type::I32),
            I16 => Some(Type::I16),
            I8 => Some(Type::I8),
            F64 => Some(Type::F64),
            F32 => Some(Type::F32),
            _ => {
                tokens.index = checkpoint;
                None
            }
        }
    };

    if let Some(_type) = try_parse_func_type(tokens) {
        return Some(_type);
    }
    if let Some(_type) = try_parse_primitive_type(tokens) {
        return Some(_type);
    }

    None
}

fn parse_declaration(tokens: &mut TokenIter) -> Result<Expr, Arc<str>> {
    let is_mutable;
    let  declaration_type ;
    let mut token = tokens.next().unwrap();
    match token.token_type {
        Let => {
            is_mutable = false;
        }
        Var => {
            is_mutable = true;
        }
        _ => unreachable!("parse declaration was called when the let/var keyword was not checked"),
    }
    declaration_type = try_parse_type(tokens);
    
    todo!()
}
