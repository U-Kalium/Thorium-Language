use std::sync::Arc;

use hashbrown::HashMap;

use crate::compiler::tokenizer::TokenIter;
use crate::compiler::tokenizer::TokenType::*;
use crate::compiler::tokenizer::tokenize;


#[derive(Debug, Clone)]
pub struct ScopeInfo {
    pub scope_level: u32,
    pub id: u32,
    pub shared_scopes: Vec<u32>,
}
impl ScopeInfo {
    pub fn new() -> Self {
        Self {
            scope_level: 0,
            id: 0,
            shared_scopes: Vec::new(),
        }
    }
    pub fn into_scope(&mut self) {
        self.scope_level += 1;
        self.id += 1;
        self.shared_scopes.push(self.id);
    }
    pub fn out_of_scope(&mut self) {
        self.scope_level -= 1;
        self.shared_scopes.pop();
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub ident: Option<Arc<str>>,
    pub param_type: Box<Type>,
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
pub struct Expr {
    pub(crate) expr_type: Type,
    pub kind: ExprKind,
}
#[derive(Debug, Clone)]
pub enum ExprKind {
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
    },
    Empty,
}

#[derive(Clone, Debug)]
pub struct VariableInfo {
   pub scope_id: u32,
   pub is_mutable: bool,
}

#[derive(Clone, Debug)]
pub struct Variables {
    pub variables: HashMap<(Arc<str>, Type), Vec<VariableInfo>>,
}
impl Variables {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }
    pub fn add_var(&mut self, var_info: VariableInfo, var_ident: &str, var_type: Type) {
        if let Some(vars) = self.variables.get_mut(&(var_ident.into(), var_type)) {
            vars.push(var_info.clone())
        }
    }
}

