use std::sync::Arc;

use hashbrown::HashMap;
use thiserror::Error;

use crate::compiler::CompilerError;
use crate::compiler::tokenizer::{
    Token,
    TokenType::{self, *},
    tokenize,
};
use crate::compiler::{expression_tree::*, tokenizer::TokenIter};

static mut UNKOWN_TYPE_VAR_COUNTER: u32 = 0;
fn next_unkown_type() -> u32 {
    unsafe {
        UNKOWN_TYPE_VAR_COUNTER += 1;
        UNKOWN_TYPE_VAR_COUNTER
    }
}

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("expected expression, found {token:?}")]
    ExpectedExpression { token: Token },
    #[error("expected expression of type {expected_type:?}, found {token:?}")]
    ExpectedType { expected_type: Type, token: Token },
    #[error("expected {expected_token_type:?}, found {token:?}")]
    ExpectedToken {
        expected_token_type: TokenType,
        token: Token,
    },
    #[error("expected statement end (newline or semicolon), found {token:?}")]
    ExpecetedStatementEnd { token: Token },
}

#[derive(Debug, Clone)]
pub struct ParserCtx {
    pub current_scope_info: ScopeInfo,
}

impl ParserCtx {
    fn new() -> Self {
        Self {
            current_scope_info: ScopeInfo::new(),
        }
    }
}

fn parse_number_lit(
    tokens: &mut TokenIter,
    parser_ctx: &mut ParserCtx,
) -> Result<Expr, CompilerError> {
    let token = tokens.next()?;
    match token.token_type {
        NumberLit(lit) => {
            if lit.contains(".") {
                Ok(Expr {
                    kind: ExprKind::Value(Value::Float(lit.parse().unwrap())),
                    expr_type: Type::UnknownFloat(next_unkown_type()),
                })
            } else {
                Ok(Expr {
                    kind: ExprKind::Value(Value::Integer(lit.parse().unwrap())),
                    expr_type: Type::UnknownNumber(next_unkown_type()),
                })
            }
        }
        _ => unreachable!(),
    }
}

fn parse_expr(tokens: &mut TokenIter, parser_ctx: &mut ParserCtx) -> Result<Expr, CompilerError> {
    // let expression;

    let mut token = tokens.peek().unwrap();
    if let Some(void_expr) = try_parse_void_expr(tokens, parser_ctx) {
        return Ok(void_expr);
    }
    match &token.token_type {
        NumberLit(_) => parse_number_lit(tokens, parser_ctx),
        t => Err(ParserError::ExpectedExpression { token: token }.into()),
    }
}
fn parse_void_expr(
    tokens: &mut TokenIter,
    parser_ctx: &mut ParserCtx,
) -> Result<Expr, CompilerError> {
    let token = tokens.peek().unwrap();
    match &token.token_type {
        NewLine | SemiColon => parse_empty_expr(tokens, parser_ctx),
        Let => parse_declaration(tokens, parser_ctx),
        Var => parse_declaration(tokens, parser_ctx),
        OpenCurlyBracket => parse_block(tokens, parser_ctx),
        Finish => parse_finish(tokens, parser_ctx),
        t => Err(ParserError::ExpectedType {
            expected_type: Type::Void,
            token,
        }
        .into()),
    }
}
fn parse_empty_expr(
    tokens: &mut TokenIter,
    parser_ctx: &mut ParserCtx,
) -> Result<Expr, CompilerError> {
    let token = tokens.next().unwrap();
    match token.token_type {
        NewLine | SemiColon => Ok(Expr {
            expr_type: Type::Void,
            kind: ExprKind::Empty,
        }),
        _ => unreachable!(),
    }
}
fn parse_finish(tokens: &mut TokenIter, parser_ctx: &mut ParserCtx) -> Result<Expr, CompilerError> {
    let token = tokens.next().unwrap();
    match token.token_type {
        Finish => Ok(Expr {
            expr_type: Type::Void,
            kind: ExprKind::Finish {
                expr: Box::new(parse_expr(tokens, parser_ctx)?),
            },
        }),
        _ => unreachable!(),
    }
}

fn try_parse_void_expr(tokens: &mut TokenIter, parser_ctx: &mut ParserCtx) -> Option<Expr> {
    let checkpoint = tokens.index;
    if let Ok(void_expr) = parse_void_expr(tokens, parser_ctx) {
        return Some(void_expr);
    }
    tokens.index = checkpoint;
    None
}

fn try_parse_type(tokens: &mut TokenIter, parser_ctx: &mut ParserCtx) -> Option<Type> {
    let checkpoint = tokens.index;
    // let mut token = tokens.next().unwrap();

    // try function type sig
    let try_parse_func_type = |tokens: &mut TokenIter, parser_ctx: &mut ParserCtx| {
        let func_checkpoint = tokens.index;
        let mut token = tokens.next().unwrap();
        let mut params = vec![];
        let mut returns = vec![];
        if let OpenBracket = token.token_type {
            // token = tokens.next().unwrap()
        } else {
            tokens.index = func_checkpoint;
            return None;
        }
        while !matches!(tokens.peek().unwrap().token_type, CloseBracket) {
            let mut ident: Option<Arc<str>> = None;
            let param_type;
            if let Ident(param) = token.token_type {
                ident = Some(param.into());
                tokens.next().unwrap();
            }
            if let Some(type_) = try_parse_type(tokens, parser_ctx) {
                param_type = Box::new(type_)
            } else {
                tokens.index = func_checkpoint;
                return None;
            }
            token = tokens.next().unwrap();
            if let Comma = token.token_type {
            } else {
                if let CloseBracket = tokens.peek().unwrap().token_type {
                } else {
                    tokens.index = func_checkpoint;

                    return None;
                }
            }
            params.push(Param { ident, param_type });
        }
        tokens.next();
        let mut parsing_returns = true;
        while parsing_returns {
            if let Some(return_type) = try_parse_type(tokens, parser_ctx) {
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
    let try_parse_primitive_type = |tokens: &mut TokenIter, parser_ctx: &mut ParserCtx| {
        let primitive_checpoint = tokens.index;
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
                tokens.index = primitive_checpoint;
                None
            }
        }
    };

    if let Some(_type) = try_parse_func_type(tokens, parser_ctx) {
        return Some(_type);
    }
    if let Some(_type) = try_parse_primitive_type(tokens, parser_ctx) {
        return Some(_type);
    }

    None
}

fn parse_block(tokens: &mut TokenIter, parser_ctx: &mut ParserCtx) -> Result<Expr, CompilerError> {
    let mut token = tokens.next().unwrap();
    if let OpenCurlyBracket = token.token_type {
    } else {
        return Err(ParserError::ExpectedToken {
            expected_token_type: TokenType::OpenCurlyBracket,
            token,
        }
        .into());
        // return Err(format!("Error: Expected open curly bracket found {:?}", token).into());
    }
    parser_ctx.current_scope_info.into_scope();
    let mut block_type = None;
    let mut void_exprs = Vec::new();
    token = tokens.peek().unwrap();
    if let NewLine = token.token_type {
        token = tokens.next()?
    }
    while !matches!(token.token_type, CloseCurlyBracket) {
        match parse_void_expr(tokens, parser_ctx) {
            Ok(Expr {
                expr_type,
                kind: ExprKind::Return { expr },
            }) => block_type = Some(expr.expr_type),
            Ok(void_expr) => void_exprs.push(void_expr),
            Err(error) => return Err(error),
        }
        token = tokens.next()?;
        match token.token_type {
            NewLine | SemiColon => {}
            _ => return Err(ParserError::ExpecetedStatementEnd { token }.into()),
        }
        token = tokens.next()?;
    }

    let scope_info = parser_ctx.current_scope_info.clone();
    parser_ctx.current_scope_info.out_of_scope();
    Ok(Expr {
        expr_type: match block_type {
            Some(expr_type) => expr_type,
            None => Type::Unknown(next_unkown_type()),
        },
        kind: ExprKind::Block {
            scope_info,
            exprs: void_exprs,
        },
    })
}

fn parse_declaration(
    tokens: &mut TokenIter,
    parser_ctx: &mut ParserCtx,
) -> Result<Expr, CompilerError> {
    let is_mutable;
    let declaration_type;
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
    let ident: Arc<str>;
    token = tokens.next().unwrap();
    match token.token_type {
        Ident(ident_string) => ident = ident_string.into(),
        _ => {
            return Err(ParserError::ExpectedToken {
                expected_token_type: TokenType::Ident("".to_string()),
                token,
            }
            .into());
        } // _ => return Err(format!("Error: Expeceted ident found {:?}", token).into()),
    }
    declaration_type = try_parse_type(tokens, parser_ctx);
    token = tokens.peek().unwrap();
    let assignment: Option<Box<Expr>> = match token.token_type {
        Equal => {
            tokens.next().unwrap();
            Some(Box::new(parse_expr(tokens, parser_ctx)?))
        }
        SemiColon | NewLine => None,
        _ => Some(Box::new(parse_block(tokens, parser_ctx)?)),
    };

    Ok(Expr {
        expr_type: Type::Void,
        kind: ExprKind::VarDeclaration {
            ident,
            var_type: match declaration_type {
                Some(var_type) => var_type,
                None => Type::Unknown(next_unkown_type() as u32),
            },
            assignment,
        },
    })
}

fn gen_type_equations(expr: &Expr, type_equations: &mut HashMap<Type, Type>) {
    let current_expr_type = expr.expr_type.clone();
    match &expr.kind {
        ExprKind::VarDeclaration {
            ident,
            var_type,
            assignment,
        } => {
            type_equations.insert(current_expr_type, Type::Void);
            if let Some(assignment_expr) = assignment {
                type_equations.insert(assignment_expr.expr_type.clone(), var_type.clone());
                gen_type_equations(&assignment_expr, type_equations);
            }
        }
        ExprKind::Return { expr } => {
            type_equations.insert(current_expr_type, Type::Void);
            gen_type_equations(expr, type_equations);
        }
        ExprKind::Finish { expr } => {
            type_equations.insert(current_expr_type, Type::Void);
            gen_type_equations(expr, type_equations);
        }
        ExprKind::Value(value) => {}
        ExprKind::Variable { ident, var_type } => {
            type_equations.insert(current_expr_type, var_type.clone());
        }
        ExprKind::Block { scope_info, exprs } => {
            for block_expr in exprs {
                gen_type_equations(block_expr, type_equations);
                match &block_expr.kind {
                    ExprKind::Finish { expr } => {
                        type_equations.insert(expr.expr_type.clone(), {
                            match type_equations.get(&current_expr_type) {
                                Some(Type::Func { params: _, returns }) => *returns[0].clone(),
                                Some(otherwise) => otherwise.clone(),
                                None => current_expr_type.clone(),
                            }
                        });
                    }
                    ExprKind::Return { expr } => {
                        type_equations.insert(expr.expr_type.clone(), {
                            match type_equations.get(&current_expr_type) {
                                Some(Type::Func { params: _, returns }) => *returns[0].clone(),
                                Some(otherwise) => otherwise.clone(),
                                None => current_expr_type.clone(),
                            }
                        });
                    }
                    _ => (),
                }
            }
        }
        ExprKind::Empty => {},
    }
}
// fn generate_equations(&self, equations: &mut HashMap<Type, Type>) {
//         // dbg!(self);
//         // let mut equations = HashMap::new();
//         let first_type_ = &self.type_;
//         match &self.expr {
//             ExprType::Decleration {
//                 is_mutable: _,
//                 ident: _,
//                 type_,
//                 assignment,
//             } => {
//                 if let Some(assignment) = assignment {
//                     equations.insert(assignment.0.type_.clone(), type_.clone());
//                     assignment.0.generate_equations(equations);
//                 }
//                 equations.insert(first_type_.clone(), Type::Void);
//             }
//             ExprType::Finish { expr } => {
//                 equations.insert(first_type_.clone(), Type::Void);
//                 expr.0.generate_equations(equations);
//             }
//             ExprType::Value(value) => match value {
//                 Value::Integer(_) => {
//                     equations.insert(first_type_.clone(), Type::I64);
//                 }
//             },
//             ExprType::Block {
//                 scope_info: _,
//                 exprs,
//             } => {
//                 for (block_expr, _) in exprs {
//                     block_expr.generate_equations(equations);
//                     match &block_expr.expr {
//                         ExprType::Finish { expr } => {
//                             equations.insert(expr.0.type_.clone(), {
//                                 match equations.get(first_type_) {
//                                     Some(Type::Func {
//                                         param_names: _,
//                                         params: _,
//                                         returns,
//                                     }) => returns[0].clone(),
//                                     Some(otherwise) => otherwise.clone(),
//                                     None => first_type_.clone(),
//                                 }
//                             });
//                         }
//                         ExprType::Return { expr } => {
//                             equations.insert(expr.0.type_.clone(), {
//                                 match equations.get(first_type_) {
//                                     Some(Type::Func {
//                                         param_names: _,
//                                         params: _,
//                                         returns,
//                                     }) => returns[0].clone(),
//                                     Some(otherwise) => otherwise.clone(),
//                                     None => first_type_.clone(),
//                                 }
//                             });
//                         }
//                         _ => (),
//                     }
//                 }
//             }
//             ExprType::Return { expr } => {
//                 equations.insert(first_type_.clone(), Type::Void);
//                 expr.0.generate_equations(equations);
//             }
//             ExprType::Variable { ident: _, type_ } => {
//                 equations.insert(first_type_.clone(), type_.clone());
//             },
//         }
//         // equations
//     }
//     fn solve_unkown_types(&self, equations: &HashMap<Type, Type>) -> Self {
//         fn get_new_type(type_: &Type, equations: &HashMap<Type, Type>) -> Type {
//             match type_ {
//                 Type::Unkown(id) => {
//                     if let Some(new_type) = equations.get(&Type::Unkown(*id)) {
//                         get_new_type(new_type, equations)
//                     } else {
//                         type_.clone()
//                     }
//                 }
//                 _ => type_.clone(),
//             }
//         }
//         let new_type = get_new_type(&self.type_, equations);
//         let new_expr_type = match &self.expr {
//             ExprType::Decleration {
//                 is_mutable,
//                 ident,
//                 type_,
//                 assignment,
//             } => ExprType::Decleration {
//                 is_mutable: *is_mutable,
//                 ident: ident.clone(),
//                 type_: type_.clone(),
//                 assignment: if let Some(expr) = assignment {
//                     Some(Box::new((expr.0.solve_unkown_types(equations), expr.1)))
//                 } else {
//                     None
//                 },
//             },
//             ExprType::Finish { expr } => ExprType::Finish {
//                 expr: Box::new((expr.0.solve_unkown_types(equations), expr.1)),
//             },
//             ExprType::Value(value) => ExprType::Value(value.clone()),
//             ExprType::Block { scope_info, exprs } => ExprType::Block {
//                 scope_info: scope_info.clone(),
//                 exprs: exprs
//                     .iter()
//                     .map(|(expr, span)| (expr.solve_unkown_types(equations), span.clone()))
//                     .collect(),
//             },
//             ExprType::Return { expr } => ExprType::Return {
//                 expr: Box::new((expr.0.solve_unkown_types(equations), expr.1)),
//             },
//             ExprType::Variable { ident, type_ } => {
//                 ExprType::Variable { ident: ident.clone(), type_: type_.clone() }
//             },
//         };
//         Self {
//             type_: new_type,
//             expr: new_expr_type,
//         }
//     }
//     fn solve_variable_scoping(&self, variables: &Variables) -> (Self, Variables) {
//         let mut variables = variables.clone();
//         // let mut scope_info_tracker = ScopeInfo::new();
//         // let mut variables = Variables::new();
//         let type_ = self.type_.clone();
//         let new_expr_type = match &self.expr {
//             ExprType::Decleration {
//                 is_mutable,
//                 ident,
//                 type_,
//                 assignment,
//             } => {
//                 let var_info = VariableInfo {
//                     scope_id: unsafe { SCOPE_INFO_TRACKER.id },
//                     is_mutable: *is_mutable,
//                 };
//                 variables.add_var(var_info, ident, type_.clone());
//                 ExprType::Decleration {
//                     is_mutable: *is_mutable,
//                     ident: ident.clone(),
//                     type_: type_.clone(),
//                     assignment: assignment.clone(),
//                 }
//             }
//             ExprType::Block { scope_info, exprs } => {
//                 #[allow(static_mut_refs)]
//                 unsafe {
//                     SCOPE_INFO_TRACKER.into_scope()
//                 };
//                 #[allow(static_mut_refs)]
//                 let scope_info = unsafe { SCOPE_INFO_TRACKER.clone() };
//                 let exprs = exprs
//                     .iter()
//                     .map(|(expr, span)| {
//                         let (new_expr, new_variables) = expr.solve_variable_scoping(&variables);

//                         (new_expr, span.clone())
//                     })
//                     .collect();
//                 #[allow(static_mut_refs)]
//                 unsafe {
//                     SCOPE_INFO_TRACKER.out_of_scope()
//                 };
//                 ExprType::Block { scope_info, exprs }
//             }
//             otherwise => otherwise.clone(),
//         };

//         // todo!()
//         (
//             Self {
//                 type_,
//                 expr: new_expr_type,
//             },
//             variables,
//         )
//     }
#[test]
fn scratch_pad() {
    let file = include_str!("../../../examples/return.th");
    let mut tokens = tokenize(file.to_string());
    let mut token_iter = TokenIter::new(tokens);
    let mut parser_ctx = ParserCtx::new();
    let declaration = parse_declaration(&mut token_iter, &mut parser_ctx).unwrap();
    let mut type_equations = HashMap::new();
    gen_type_equations(&declaration, &mut type_equations);
    dbg!(declaration);
    dbg!(type_equations);
    assert!(true)
}
