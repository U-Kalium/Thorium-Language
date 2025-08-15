use std::collections::HashMap;
use std::{collections::HashSet, iter::Peekable};

use crate::tokenizer::TokenType::*;
use crate::tokenizer::{Number, Token};

struct FunctionType {
    parems: HashMap<String, String>,
    returns: Vec<String>,
}
impl FunctionType {
    fn new() -> Self {
        FunctionType {
            parems: HashMap::new(),
            returns: Vec::new(),
        }
    }
}

struct VariableProperties {
    variable_type: String,
    is_mutable: bool,
}

struct ExpressionReturn {
    byte_code: String,
    expression_type: String,
}
#[derive(Debug, Clone)]

enum Value {
    Custom(String),
    Number(Number),
}
#[derive(Debug, Clone)]
enum Condition {
    Equal,
    NotEqual,
}
#[derive(Debug, Clone)]
enum Expression {
    Variable(String),
    Value {
        expr_type: String,
        value: Value,
    },
    Conditional {
        condition: Condition,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
}

struct Parser {
    functions: HashMap<String, FunctionType>,
    variables: HashMap<String, VariableProperties>,
    types: HashSet<String>,
}

impl Parser {
    fn new() -> Self {
        Parser {
            functions: HashMap::new(),
            variables: HashMap::new(),
            types: HashSet::new(),
        }
    }

    fn parse_func<'a, I: Iterator<Item = &'a Token>>(
        &mut self,
        tokens: &mut Peekable<I>,
    ) -> String {
        fn parse_func_args<'a, I: Iterator<Item = &'a Token>>(tokens: &mut Peekable<I>) -> String {
            let func_args = String::new();
            let token = tokens.next().unwrap();
            match &token.token_type {
                CloseBracket => {}
                Ident(ident) => todo!("add function paremeters"),
                t => panic!(
                    "Syntax Error: expected ) or ident for function args, found {t:?} at {}:{} ",
                    token.line, token.column
                ),
            }
            func_args
        }
        let func_name;
        let mut func_type = FunctionType::new();

        let mut func = String::new();
        let mut token = tokens.next().unwrap();
        match &token.token_type {
            Fn => {
                func.push_str("func ");
            }
            t => panic!(
                "Syntax Error: expected `fn` keyword, found {t:?} at {}:{}",
                token.line, token.column
            ),
        }
        // func name
        token = tokens.next().unwrap();
        match &token.token_type {
            Ident(ident) => {
                if self.functions.contains_key(ident) {
                    panic!("Error: function {ident} already declared")
                }
                func.push_str(format!("${ident}").as_str());
                func_name = ident.clone()
            }
            OpenBracket => {
                todo!("anonymouse function parsing")
            }
            t => panic!(
                "Syntax Error: expected ident or (), found {t:?} at {}:{} ",
                token.line, token.column
            ),
        }
        // func arguments
        token = tokens.next().unwrap();
        match &token.token_type {
            OpenBracket => func.push_str(&parse_func_args(tokens)),
            OpenAngleBracket => {
                todo!("Method signature")
            }
            t => panic!(
                "Syntax Error: expected () or <>, found {t:?} at {}:{} ",
                token.line, token.column
            ),
        }
        // func returns
        func_type.returns.push(self.parse_type(tokens));
        func.push_str(":\n");

        // func body
        let mut func_variables = HashMap::new();
        func.push_str(&self.parse_scope(tokens, func_type.returns.clone(), &mut func_variables));
        func.push_str("endfunc");

        self.functions.insert(func_name, func_type);

        func
    }

    fn parse_scope<'a, I: Iterator<Item = &'a Token>>(
        &mut self,
        tokens: &mut Peekable<I>,
        return_type: Vec<String>,
        variables: &mut HashMap<String, VariableProperties>,
    ) -> String {
        // let mut variables = HashMap::new();
        let mut scope = String::new();
        let mut token = tokens.next().unwrap();
        match &token.token_type {
            OpenCurlyBracket => {}
            t => panic!(
                "Syntax Error: expected {{, found {t:?} at {}{} ",
                token.line, token.column
            ),
        }
        token = tokens.next().unwrap();
        while token.token_type != CloseCurlyBracket {
            scope.push_str(&self.parse_statement(tokens, variables, return_type.clone()));
            token = tokens.peek().unwrap();
        }
        tokens.next();
        scope
    }

    fn parse_type<'a, I: Iterator<Item = &'a Token>>(
        &mut self,
        tokens: &mut Peekable<I>,
    ) -> String {
        let parsed_type;
        let token = tokens.next().unwrap();
        match &token.token_type {
            I64 => parsed_type = "i64".to_string(),
            I32 => parsed_type = "i32".to_string(),
            I16 => parsed_type = "i16".to_string(),
            I8 => parsed_type = "i8".to_string(),
            F64 => parsed_type = "f64".to_string(),
            F32 => parsed_type = "f32".to_string(),
            Bool => parsed_type = "bool".to_string(),
            Ident(_ident) => todo!("Custom types parsing"),
            t => panic!(
                "Syntax Error: expected type, found {t:?} at {}:{} ",
                token.line, token.column
            ),
        }

        parsed_type
    }

    fn parse_statement<'a, I: Iterator<Item = &'a Token>>(
        &mut self,
        tokens: &mut Peekable<I>,
        variables: &mut HashMap<String, VariableProperties>,
        scope_return_type: Vec<String>,
    ) -> String {
        let mut statement = String::new();
        let mut token = tokens.next().unwrap();
        fn parse_else<'a, I: Iterator<Item = &'a Token>>(
            parser: &mut Parser,
            tokens: &mut Peekable<I>,
            variables: &mut HashMap<String, VariableProperties>,
            // token: &'a Token,
            scope_return_type: Vec<String>,
            statement: &mut String,
        ) {
            tokens.next();
            let peeked = tokens.peek().unwrap();
            if let If = peeked.token_type {
                let token = tokens.next().unwrap();
                parse_if(
                    parser,
                    tokens,
                    variables,
                    token,
                    scope_return_type,
                    statement,
                );
            } else {
                statement.push_str(&parser.parse_scope(tokens, scope_return_type, variables))
            }
        }
        fn parse_if<'a, I: Iterator<Item = &'a Token>>(
            parser: &mut Parser,
            tokens: &mut Peekable<I>,
            variables: &mut HashMap<String, VariableProperties>,
            token: &'a Token,
            scope_return_type: Vec<String>,
            statement: &mut String,
        ) {
            let expression = parser.parse_expression(tokens, variables);
            if expression.expression_type != "bool".to_string() {
                panic!(
                    "Error: expected bool type found {} at {}:{}",
                    expression.expression_type, token.line, token.column
                )
            }
            statement.push_str(&expression.byte_code);

            let scope = parser.parse_scope(tokens, scope_return_type.clone(), variables);

            let end_if_label = format!("ifendL{}C{}", token.line, token.column);
            let else_label = format!("elseL{}C{}", token.line, token.column);
            statement.push_str(&format!("    jpz \"{else_label}\"\n"));
            statement.push_str(&scope);
            statement.push_str(&format!("    i8 push 1\n"));
            statement.push_str(&format!("    jmp \"{end_if_label}\"\n"));
            statement.push_str(&format!("@{else_label}\n"));
            let else_token = tokens.peek().unwrap();
            if let Else = else_token.token_type {
                parse_else(parser, tokens, variables, scope_return_type, statement);
            }
            statement.push_str(&format!("@{end_if_label}\n"));
        }

        match &token.token_type {
            Return => {
                token = tokens.next().unwrap();
                match &token.token_type {
                    NumberLit(number) => {
                        statement
                            .push_str(&format!("    {} push {number}\n", scope_return_type[0]));
                    }
                    Ident(ident) => {
                        if !variables.contains_key(ident) {
                            panic!(
                                "Error: Variable {ident} does not exist yet was used at {}:{}",
                                token.line, token.column
                            )
                        }
                        statement.push_str(&format!("    get %{ident}\n"));
                    }
                    t => panic!(
                        "Syntax Error: expected i32 after return, found {t:?} at {}:{} ",
                        token.line, token.column
                    ),
                }
                statement.push_str("    return\n");
            }
            Var => {
                token = tokens.next().unwrap();
                match &token.token_type {
                    Ident(ident) => {
                        statement.push_str(&format!("    declare %{ident}\n"));
                        // token = tokens.next().unwrap();
                        let variable_properties = VariableProperties {
                            variable_type: self.parse_type(tokens),
                            is_mutable: true,
                        };
                        variables.insert(ident.clone(), variable_properties);
                        let peeked_token = tokens.peek().unwrap();
                        match peeked_token.token_type {
                            NewLine => {}
                            SemiColon => {}
                            _ => {
                                statement.push_str(&self.parse_assignment(tokens, variables, ident))
                            }
                        }
                    }
                    t => panic!(
                        "Syntax Error: expected identifier, found {t:?} at {}:{} ",
                        token.line, token.column
                    ),
                }
            }
            Let => {
                token = tokens.next().unwrap();
                match &token.token_type {
                    Ident(ident) => {
                        statement.push_str(&format!("    declare %{ident}\n"));
                        // token = tokens.next().unwrap();
                        let variable_properties = VariableProperties {
                            variable_type: self.parse_type(tokens),
                            is_mutable: false,
                        };
                        variables.insert(ident.clone(), variable_properties);
                        let peeked_token = tokens.peek().unwrap();
                        match peeked_token.token_type {
                            NewLine => {}
                            SemiColon => {}
                            _ => {
                                statement.push_str(&self.parse_assignment(tokens, variables, ident))
                            }
                        }
                    }
                    t => panic!(
                        "Syntax Error: expected identifier, found {t:?} at {}:{} ",
                        token.line, token.column
                    ),
                }
            }
            If => {
                parse_if(
                    self,
                    tokens,
                    variables,
                    token,
                    scope_return_type,
                    &mut statement,
                );
            }
            Ident(ident) => {
                if let Some(variable_properties) = variables.get(ident) {
                    if !variable_properties.is_mutable {
                        panic!(
                            "Error: tried mutating variable {ident} but it is immutable, found at {}:{}",
                            token.line, token.column
                        )
                    }
                    statement.push_str(&self.parse_assignment(tokens, variables, ident));
                } else {
                    panic!(
                        "Error: variable {ident} does not exist, found at {}:{}",
                        token.line, token.column
                    )
                }
            }

            t => panic!(
                "Syntax Error: expected statement, found {t:?} at {}:{} ",
                token.line, token.column
            ),
        }
        token = tokens.next().unwrap();
        match &token.token_type {
            SemiColon => {}
            NewLine => {}
            t => panic!(
                "Syntax Error: expected newline or semicolon, found {t:?} at {}:{} ",
                token.line, token.column
            ),
        }
        statement
    }

    fn parse_expression<'a, I: Iterator<Item = &'a Token>>(
        &mut self,
        tokens: &mut Peekable<I>,
        variables: &HashMap<String, VariableProperties>,
    ) -> ExpressionReturn {
        let mut expression = ExpressionReturn {
            expression_type: String::new(),
            byte_code: String::new(),
        };
        let mut expression_tokens = Vec::new();
        while match tokens.peek().unwrap().token_type {
            Ident(_) => true,
            DoubleEqual => true,
            NumberLit(_) => true,
            _ => false,
        } {
            let token = tokens.next().unwrap();
            expression_tokens.push(token);
        }
        let expr_tokens: Peekable<std::slice::Iter<'_, &'a Token>> =
            expression_tokens.iter().peekable();
        fn collect_expr<'a>(
            mut tokens: Peekable<std::slice::Iter<'_, &'a Token>>,
            variables: &HashMap<String, VariableProperties>,
        ) -> Expression {
            let token = tokens.next().unwrap();
            match &token.token_type {
                NumberLit(num) => {
                    if let Some(token) = tokens.next() {
                        // token needs to be an operation
                        match &token.token_type {
                            DoubleEqual => {
                                if let Some(token) = tokens.clone().peek() {
                                    let mut num_type = "i64".to_string();
                                    let rhs = collect_expr(tokens, variables);
                                    match &rhs {
                                        Expression::Variable(ident) => {
                                            let prop = variables.get(ident).unwrap();
                                            num_type = prop.variable_type.clone()
                                        }
                                        Expression::Conditional {
                                            condition: _,
                                            lhs: _,
                                            rhs: _,
                                        } => panic!(
                                            "Error: can not chain conditionals together like at {}:{}",
                                            token.line, token.column
                                        ),
                                        _ => {}
                                    }
                                    return Expression::Conditional {
                                        condition: Condition::Equal,
                                        lhs: Box::new(Expression::Value {
                                            expr_type: num_type,
                                            value: Value::Number(num.clone()),
                                        }),
                                        rhs: Box::new(rhs),
                                    };
                                } else {
                                    panic!(
                                        "Syntax Error: expected ident or literal found nothing at {}:{}",
                                        token.line, token.column
                                    )
                                }
                            }
                            op => todo!("operation: {op:?} not implemented"),
                        }
                    } else {
                        return Expression::Value {
                            expr_type: "i64".to_string(),
                            value: Value::Number(num.clone()),
                        };
                    }
                }
                Ident(ident) => {
                    if !variables.contains_key(ident) {
                        panic!(
                            "Error: variable {ident} has not been declared in the scope but is used at {}:{}",
                            token.line, token.column
                        )
                    }
                    if let Some(token) = tokens.next() {
                        // token needs to be an operation
                        match &token.token_type {
                            DoubleEqual => {
                                // if let Some(_) = tokens.peek() {
                                //     return Expression::Conditional {
                                //         condition: Condition::Equal,
                                //         lhs: Box::new(Expression::Variable(ident.to_string())),
                                //         rhs: Box::new(collect_expr(tokens, variables))
                                //     }
                                // } else {
                                //     panic!("Syntax Error: expected ident or literal found nothing at {}:{}", token.line, token.column)
                                // }
                                if let Some(token) = tokens.clone().peek() {
                                    // let mut num_type = "i64".to_string();
                                    let mut rhs = collect_expr(tokens, variables);
                                    match &mut rhs {
                                        Expression::Value {
                                            expr_type,
                                            value: _value,
                                        } => {
                                            let prop = variables.get(ident).unwrap();
                                            *expr_type = prop.variable_type.clone()
                                        }
                                        Expression::Conditional {
                                            condition: _,
                                            lhs: _,
                                            rhs: _,
                                        } => panic!(
                                            "Error: can not chain conditionals together like at {}:{}",
                                            token.line, token.column
                                        ),
                                        _ => {}
                                    }
                                    return Expression::Conditional {
                                        condition: Condition::Equal,
                                        lhs: Box::new(Expression::Variable(ident.to_string())),
                                        rhs: Box::new(rhs),
                                    };
                                } else {
                                    panic!(
                                        "Syntax Error: expected ident or literal found nothing at {}:{}",
                                        token.line, token.column
                                    )
                                }
                            }
                            op => todo!("operation: {op:?} not implemented"),
                        }
                    } else {
                        return Expression::Variable(ident.to_string());
                    }
                }
                wrong_token => panic!(
                    "Syntax Error: expected ident or literal found {wrong_token:?} at {}:{}",
                    token.line, token.column
                ),
            }
        }
        fn parse_expr_tree(
            expr: &Expression,
            variables: &HashMap<String, VariableProperties>,
        ) -> String {
            let mut expr_string = String::new();
            match expr {
                Expression::Variable(ident) => {
                    expr_string.push_str(&format!("    get %{}\n", ident))
                }
                Expression::Value { expr_type, value } => match value {
                    Value::Custom(_) => todo!("custom types not implemented"),
                    Value::Number(number) => {
                        expr_string.push_str(&format!("    {expr_type} push {number}\n"))
                    }
                },
                Expression::Conditional {
                    condition,
                    lhs,
                    rhs,
                } => {
                    expr_string.push_str(&parse_expr_tree(lhs, variables));
                    expr_string.push_str(&parse_expr_tree(rhs, variables));
                    let mut operation_type = "i64".to_string();
                    match &**lhs {
                        Expression::Value {
                            expr_type,
                            value: _,
                        } => operation_type = expr_type.clone(),
                        Expression::Variable(ident) => {
                            operation_type = variables.get(ident).unwrap().variable_type.clone()
                        }
                        _ => {}
                    }
                    match &**rhs {
                        Expression::Value {
                            expr_type,
                            value: _,
                        } => operation_type = expr_type.clone(),
                        Expression::Variable(ident) => {
                            operation_type = variables.get(ident).unwrap().variable_type.clone()
                        }
                        _ => {}
                    }
                    match condition {
                        Condition::Equal => {
                            expr_string.push_str(&format!("    {operation_type} eq\n"))
                        }
                        Condition::NotEqual => todo!(),
                    }
                    expr_string.push_str("    cast i8\n");
                }
            }
            expr_string
        }
        let expression_tree = collect_expr(expr_tokens, variables);
        // dbg!(&expression_tree);
        let byte_code = parse_expr_tree(&expression_tree, variables);
        // dbg!(&byte_code);

        expression.byte_code = byte_code.clone();

        match &expression_tree {
            Expression::Variable(ident) => {
                expression.expression_type = variables.get(ident).unwrap().variable_type.clone()
            }
            Expression::Value {
                expr_type,
                value: _,
            } => expression.expression_type = expr_type.clone(),
            Expression::Conditional {
                condition: _,
                lhs: _,
                rhs: _,
            } => expression.expression_type = "bool".to_string(),
        }

        expression
    }

    fn parse_assignment<'a, I: Iterator<Item = &'a Token>>(
        &mut self,
        tokens: &mut Peekable<I>,
        variables: &mut HashMap<String, VariableProperties>,
        variable: &String,
    ) -> String {
        let mut assignment = String::new();
        let peeked_token = tokens.peek().unwrap();
        match &peeked_token.token_type {
            OpenCurlyBracket => {
                let varibale_type = variables.get(variable).unwrap().variable_type.clone();
                assignment.push_str(&self.parse_scope(tokens, vec![varibale_type], variables))
            }
            Equal => {
                tokens.next().unwrap();
                let token = tokens.next().unwrap();
                let variable_type = variables.get(variable).unwrap().variable_type.clone();
                match &token.token_type {
                    NumberLit(num) => {
                        assignment.push_str(&format!("    {variable_type} push {num}\n"))
                    }
                    Ident(ident) => {
                        if let Some(variable_properties) = variables.get(ident) {
                            let variable_type =
                                variables.get(variable).unwrap().variable_type.clone();
                            if variable_type == variable_properties.variable_type {
                                assignment.push_str(&format!("    get %{ident}\n"));
                            }
                        } else {
                            panic!(
                                "Error: tried assigning an unkown identifier {ident} to variable {variable}"
                            );
                        }
                    }
                    True => {
                        if variable_type == "bool".to_string() {
                            assignment.push_str(&format!("    i8 push 1"));
                        } else {
                            panic!(
                                "Error: tried assigning a bool type to {:?} at {}:{}",
                                token.token_type, token.line, token.column
                            )
                        }
                    }
                    False => {
                        if variable_type == "bool".to_string() {
                            assignment.push_str(&format!("    i8 push 0"));
                        } else {
                            panic!(
                                "Error: tried assigning a bool type to {:?} at {}:{}",
                                token.token_type, token.line, token.column
                            )
                        }
                    }
                    t => panic!(
                        "Syntax Error: expected int or variable, found {t:?} at {}:{} ",
                        token.line, token.column
                    ),
                }
            }
            t => panic!(
                "Syntax Error: expected {{}} or = , found {t:?} at {}:{} ",
                peeked_token.line, peeked_token.column
            ),
        }
        assignment.push_str(&format!("    set %{variable}\n"));
        assignment
    }
}

pub fn parse<'a, I: Iterator<Item = &'a Token>>(tokens: &mut Peekable<I>) -> String {
    let mut parser = Parser::new();
    let mut bin = String::new();
    bin.push_str(
        "
func $start \"_start\" :
    call $main
    return

",
    );
    let peeked_token = tokens.peek().unwrap();
    match &peeked_token.token_type {
        Fn => bin.push_str(parser.parse_func(tokens).as_str()),
        t => panic!(
            "Syntax Error: unexpected token {t:?} found in file base at {}{}",
            peeked_token.line, peeked_token.column
        ),
    }

    bin
}
