use std::sync::Arc;

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
                    expr_type: Type::Unknown(next_unkown_type()),
                })
            } else {
                Ok(Expr {
                    kind: ExprKind::Value(Value::Integer(lit.parse().unwrap())),
                    expr_type: Type::Unknown(next_unkown_type())
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

#[test]
fn scratch_pad() {
    let file = include_str!("../../../examples/return.th");
    let mut tokens = tokenize(file.to_string());
    let mut token_iter = TokenIter::new(tokens);
    let mut parser_ctx = ParserCtx::new();
    let declaration = parse_declaration(&mut token_iter, &mut parser_ctx).unwrap();
    dbg!(declaration);
    assert!(true)
}
