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
// fn offset_to_LC()
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
    #[error("could not find variable {ident} of type {var_type:?}")]
    VariableNotFound { ident: Arc<str>, var_type: Type },
}
#[derive(Debug, Clone)]
struct VariableInfo {
    scope_info: ScopeInfo,
    is_mutable: bool,
}

impl VariableInfo {
    fn new(scope_info: ScopeInfo, is_mutable: bool) -> Self {
        Self {
            scope_info,
            is_mutable,
        }
    }
}

#[derive(Debug, Clone)]
pub struct VariableCtx {
    variable_infos: HashMap<Arc<str>, Vec<(Type, VariableInfo)>>,
}

#[derive(Debug, Clone)]
pub struct ParserCtx {
    pub current_scope_info: ScopeInfo,
    pub variable_info: HashMap<Arc<str>, Vec<(Type, VariableInfo)>>,
    pub type_equations: HashMap<Type, Type>,
}

impl ParserCtx {
    fn new() -> Self {
        Self {
            current_scope_info: ScopeInfo::new(),
            variable_info: HashMap::new(),
            type_equations: HashMap::new(),
        }
    }
    fn add_variable(&mut self, ident: Arc<str>, var_type: Type, var_info: VariableInfo) {
        if let Some(var_infos) = self.variable_info.get_mut(&ident) {
            var_infos.push((var_type, var_info));
        } else {
            let var_infos_and_types = vec![(var_type, var_info)];
            self.variable_info.insert(ident, var_infos_and_types);
        }
    }
    fn change_var_type(&mut self, ident: &Arc<str>, old_var_type: &Type, new_var_type: &Type) {
        if let Some(var_info_and_types) = self.variable_info.get_mut(ident) {
            for (var_type, _) in var_info_and_types.iter_mut().rev() {
                if var_type == old_var_type {
                    *var_type = new_var_type.clone();
                    return;
                }
            }
            panic!("tried changing a the type of a variable that does not exist")
        }
    }
    fn is_var_accesable(
        &self,
        ident: &Arc<str>,
        what_var_type: &Type,
        scope_info: &ScopeInfo,
    ) -> bool {
        if let Some(var_types_and_infos) = self.variable_info.get(ident) {
            for (var_type, var_info) in var_types_and_infos.iter().rev() {
                if var_info
                    .scope_info
                    .shared_scopes
                    .iter()
                    .find(|id| **id == scope_info.id)
                    .is_some()
                    && *var_type == *what_var_type
                {
                    return true;
                } else {
                    continue;
                }
            }
            false
        } else {
            false
        }
    }
    fn is_var_accesable_or_available(
        &self,
        ident: &Arc<str>,
        what_var_type: &Type,
        scope_info: &ScopeInfo,
    ) -> bool {
        if self.is_var_accesable(ident, what_var_type, scope_info) {
            return true;
        }
        if let Some(var_types_and_infos) = self.variable_info.get(ident) {
            for (var_type, var_info) in var_types_and_infos.iter().rev() {
                if var_info
                    .scope_info
                    .shared_scopes
                    .iter()
                    .find(|id| **id == scope_info.id)
                    .is_some()
                    && matches!(
                        var_type,
                        Type::Unknown(_) | Type::UnknownFloat(_) | Type::UnknownNumber(_)
                    )
                {
                    return true;
                } else {
                    continue;
                }
            }
            false
        } else {
            false
        }
    }
    fn is_var_mutable_and_accesable(
        &self,
        ident: &Arc<str>,
        what_var_type: &Type,
        scope_info: &ScopeInfo,
    ) -> bool {
        if let Some(var_types_and_infos) = self.variable_info.get(ident) {
            for (var_type, var_info) in var_types_and_infos.iter().rev() {
                if var_info
                    .scope_info
                    .shared_scopes
                    .iter()
                    .find(|id| **id == scope_info.id)
                    .is_some()
                    && *var_type == *what_var_type
                    && var_info.is_mutable
                {
                    return true;
                } else {
                    continue;
                }
            }
            false
        } else {
            false
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
        _ => {
            return Err(ParserError::ExpectedToken {
                expected_token_type: TokenType::NumberLit("".to_string()),
                token,
            })?;
        }
    }
}

fn parse_expr(tokens: &mut TokenIter, parser_ctx: &mut ParserCtx) -> Result<Expr, CompilerError> {
    // let expression;

    let mut token = tokens.peek().unwrap();
    if let Some(void_expr) = try_parse_void_expr(tokens, parser_ctx) {
        return Ok(void_expr);
    }
    if let Some(expr) = try_parse_assignment(tokens, parser_ctx) {
        return Ok(expr);
    }

    if let Some(expr) = try_parse_variable(tokens, parser_ctx) {
        return Ok(expr);
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
    if let Some(expr) = try_parse_assignment(tokens, parser_ctx) {
        return Ok(expr);
    }
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
        _ => {
            return Err(ParserError::ExpectedToken {
                expected_token_type: TokenType::SemiColon,
                token,
            })?;
        }
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
        _ => {
            return Err(ParserError::ExpectedToken {
                expected_token_type: TokenType::Finish,
                token,
            })?;
        }
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
        token = tokens.peek()?;
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
fn parse_assignment(
    tokens: &mut TokenIter,
    parser_ctx: &mut ParserCtx,
) -> Result<Expr, CompilerError> {
    let mut token = tokens.next()?;
    let mut ident;
    match token.token_type {
        Ident(found_ident) => ident = found_ident,
        _ => {
            return Err(ParserError::ExpectedToken {
                expected_token_type: TokenType::Ident("".to_string()),
                token,
            })?;
        }
    };
    token = tokens.peek()?;
    let expr = match token.token_type {
        Equal => {
            tokens.next()?;
            Box::new(parse_expr(tokens, parser_ctx)?)
        }
        OpenBracket => Box::new(parse_block(tokens, parser_ctx)?),
        _ => Err(ParserError::ExpectedExpression { token })?,
    };

    Ok(Expr {
        expr_type: Type::Void,
        kind: ExprKind::VarAssignment {
            ident: ident.clone().into(),
            expr: expr.clone(),
        },
    })
}

fn try_parse_assignment(tokens: &mut TokenIter, parser_ctx: &mut ParserCtx) -> Option<Expr> {
    let checkpoint = tokens.index;
    if let Ok(assignment) = parse_assignment(tokens, parser_ctx) {
        return Some(assignment);
    }
    tokens.index = checkpoint;
    None
}

fn parse_variable(
    tokens: &mut TokenIter,
    parser_ctx: &mut ParserCtx,
) -> Result<Expr, CompilerError> {
    let token = tokens.next()?;
    let expr_type = Type::Unknown(next_unkown_type());
    match token.token_type {
        Ident(ident) => Ok(Expr {
            expr_type: expr_type.clone(),
            kind: ExprKind::Variable {
                ident: ident.clone().into(),
                var_type: expr_type,
            },
        }),
        _ => {
            return Err(ParserError::ExpectedToken {
                expected_token_type: TokenType::Ident("".to_string()),
                token,
            })?;
        }
    }
}

fn try_parse_variable(tokens: &mut TokenIter, parser_ctx: &mut ParserCtx) -> Option<Expr> {
    let checkpoint = tokens.index;
    if let Ok(expr) = parse_variable(tokens, parser_ctx) {
        return Some(expr);
    }
    tokens.index = checkpoint;
    None
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
        _ => {
            return Err(ParserError::ExpectedToken {
                expected_token_type: TokenType::Let,
                token,
            })?;
        }
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
    let var_type = match declaration_type {
        Some(var_type) => var_type,
        None => Type::Unknown(next_unkown_type() as u32),
    };
    parser_ctx.add_variable(
        ident.clone(),
        var_type.clone(),
        VariableInfo::new(parser_ctx.current_scope_info.clone(), is_mutable),
    );

    token = tokens.peek().unwrap();
    let assignment: Option<Box<Expr>> = match token.token_type {
        Equal => {
            tokens.next().unwrap();
            Some(Box::new(parse_expr(tokens, parser_ctx)?))
        }
        SemiColon | NewLine => None,
        _ => Some(Box::new(parse_block(tokens, parser_ctx)?)),
    };
    // dbg!(tokens.current());
    Ok(Expr {
        expr_type: Type::Void,
        kind: ExprKind::VarDeclaration {
            ident,
            var_type,
            assignment,
        },
    })
}

fn gen_type_equations(parser_ctx: &mut ParserCtx, expr: &Expr) {
    let current_expr_type = expr.expr_type.clone();
    match &expr.kind {
        ExprKind::VarDeclaration {
            ident,
            var_type,
            assignment,
        } => {
            parser_ctx
                .type_equations
                .insert(current_expr_type, Type::Void);
            if let Some(assignment_expr) = assignment {
                match var_type {
                    Type::Unknown(_) => parser_ctx.type_equations.insert(var_type.clone(),assignment_expr.expr_type.clone()),
                    _ => parser_ctx.type_equations.insert( assignment_expr.expr_type.clone(), var_type.clone()),
                };
                
                // parser_ctx
                //     .type_equations
                //     .insert(var_type.clone(), assignment_expr.expr_type.clone());
                gen_type_equations(parser_ctx, &assignment_expr);
            }
        }
        ExprKind::Return { expr } => {
            parser_ctx
                .type_equations
                .insert(current_expr_type, Type::Void);
            gen_type_equations(parser_ctx, expr);
        }
        ExprKind::Finish { expr } => {
            parser_ctx
                .type_equations
                .insert(current_expr_type, Type::Void);
            gen_type_equations(parser_ctx, expr);
        }
        ExprKind::Value(value) => {}
        ExprKind::Variable { ident, var_type } => {
            

            parser_ctx
                .type_equations
                .insert(current_expr_type, var_type.clone());
        }
        ExprKind::Block { scope_info, exprs } => {
            for block_expr in exprs {
                gen_type_equations(parser_ctx, block_expr);
                match &block_expr.kind {
                    ExprKind::Finish { expr } => {
                        parser_ctx.type_equations.insert(expr.expr_type.clone(), {
                            match parser_ctx.type_equations.get(&current_expr_type) {
                                Some(Type::Func { params: _, returns }) => *returns[0].clone(),
                                Some(otherwise) => otherwise.clone(),
                                None => current_expr_type.clone(),
                            }
                        });

                    }
                    ExprKind::Return { expr } => {
                        parser_ctx.type_equations.insert(expr.expr_type.clone(), {
                            match parser_ctx.type_equations.get(&current_expr_type) {
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
        ExprKind::Empty => {}
        ExprKind::VarAssignment { ident, expr } => {
            parser_ctx
                .type_equations
                .insert(current_expr_type, Type::Void);
            gen_type_equations(parser_ctx, expr);
        }
    }
}
fn get_new_type(original_type: &Type, type_equations: &HashMap<Type, Type>) -> Type {
    match original_type {
        Type::Unknown(id) | Type::UnknownNumber(id) | Type::UnknownFloat(id) => {
            if let Some(new_type) = type_equations.get(original_type) {
                get_new_type(new_type, type_equations)
            } else {
                original_type.clone()
            }
        }
        _ => original_type.clone(),
    }
}
fn check_solve_expr(expr: &Expr, parser_ctx: &mut ParserCtx) -> Result<Expr, CompilerError> {
    let new_type = get_new_type(&expr.expr_type, &parser_ctx.type_equations);
    let new_expr_kind = match &expr.kind {
        ExprKind::VarDeclaration {
            ident,
            var_type,
            assignment,
        } => {
            let solved_var_type = get_new_type(var_type, &parser_ctx.type_equations);
            parser_ctx.change_var_type(ident, &var_type, &solved_var_type);
            ExprKind::VarDeclaration {
                ident: ident.clone(),
                var_type: solved_var_type.clone(),
                assignment: if let Some(assignment_expr) = assignment {
                    Some(Box::new(check_solve_expr(&assignment_expr, parser_ctx)?))
                } else {
                    None
                },
            }
        }
        ExprKind::Return { expr } => ExprKind::Return {
            expr: Box::new(check_solve_expr(&expr, parser_ctx)?),
        },
        ExprKind::Finish { expr } => ExprKind::Finish {
            expr: Box::new(check_solve_expr(&expr, parser_ctx)?),
        },
        ExprKind::Value(value) => ExprKind::Value(value.clone()),
        ExprKind::Variable { ident, var_type } => {
            let solved_var_type = get_new_type(&var_type, &parser_ctx.type_equations);
            println!("solved variable type is: {:?}", solved_var_type);
            if let Some(variable_infos) = parser_ctx.variable_info.get(ident) {
                for (decl_var_type, var_info) in variable_infos.iter().rev() {
                    let final_decl_var_type;
                    if matches!(decl_var_type, Type::Unknown(_)) {
                        final_decl_var_type  = get_new_type(decl_var_type, &parser_ctx.type_equations); 
                    } else {
                        final_decl_var_type = decl_var_type.clone();

                    }

                    if solved_var_type.is_compatible(&final_decl_var_type) {
                        parser_ctx
                            .type_equations
                            .insert(final_decl_var_type, solved_var_type.clone());
                        break;
                    }
                }
            }
            // dbg!(&solved_var_type);
            if !parser_ctx.is_var_accesable_or_available(
                ident,
                &solved_var_type,
                &parser_ctx.current_scope_info,
            ) {
                return Err(ParserError::VariableNotFound {
                    ident: ident.clone(),
                    var_type: solved_var_type.clone(),
                })?;
            }
            ExprKind::Variable {
                ident: ident.clone(),
                var_type: solved_var_type.clone(),
            }
        }
        ExprKind::Block { scope_info, exprs } => {
            parser_ctx.current_scope_info = scope_info.clone();
            ExprKind::Block {
                scope_info: scope_info.clone(),
                exprs: {
                    let mut new_exprs = Vec::new();
                    for expr in exprs {
                        new_exprs.push(check_solve_expr(&expr, parser_ctx)?);
                    }
                    new_exprs
                },
            }
        }
        ExprKind::Empty => ExprKind::Empty,
        ExprKind::VarAssignment { ident, expr } => ExprKind::VarAssignment {
            ident: ident.clone(),
            expr: expr.clone(),
        },
    };
    Ok(Expr {
        expr_type: new_type,
        kind: new_expr_kind,
    })
}

#[test]
fn scratch_pad() {
    let file = include_str!("../../../examples/variables.th");
    let mut tokens = tokenize(file.to_string());
    // println!("tokens: {:?}", &tokens);
    let mut token_iter = TokenIter::new(tokens);
    let mut parser_ctx = ParserCtx::new();
    let mut declaration = parse_declaration(&mut token_iter, &mut parser_ctx).unwrap();
    gen_type_equations(&mut parser_ctx, &declaration);

    dbg!(&declaration);
    declaration = check_solve_expr(&declaration, &mut parser_ctx).unwrap();
    dbg!(&parser_ctx.type_equations);
    dbg!(declaration);
    assert!(true)
}
