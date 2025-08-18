use std::collections::HashMap;
use std::{collections::HashSet};

use crate::tokenizer::TokenType::{self, *};
use crate::tokenizer::{Number, Token, TokenIter};

struct FunctionType {
    parems: HashMap<String, String>,
    returns: Vec<TypeDescription>,
}
impl FunctionType {
    fn new() -> Self {
        FunctionType {
            parems: HashMap::new(),
            returns: Vec::new(),
        }
    }
}
#[derive(Clone, Debug, PartialEq)]
enum ModifierType {
    FixedArray(usize),
    DynamicArray,
    None,
}
#[derive(Clone, Debug, PartialEq)]
struct TypeDescription {
    modifier: ModifierType,
    _type: String,
}

struct VariableProperties {
    variable_type: TypeDescription,
    is_mutable: bool,
}

struct ExpressionReturn {
    byte_code: String,
    expression_type: TypeDescription,
}
#[derive(Debug, Clone)]

enum Value {
    Custom(String),
    Number(String),
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
        expr_type: TypeDescription,
        value: Value,
    },
    Conditional {
        condition: Condition,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    IndexedArray {
        expr_type: TypeDescription,
        variable: String,
        index: Box<Expression>,
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

    fn parse_func(
        &mut self,
        tokens: &mut TokenIter,
    ) -> String {
        fn parse_func_args(tokens: &mut TokenIter) -> String {
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

    fn parse_scope(
        &mut self,
        tokens: &mut TokenIter,
        return_type: Vec<TypeDescription>,
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

    fn parse_type(
        &mut self,
        tokens: &mut TokenIter,
    ) -> TypeDescription {
        let mut parsed_type = TypeDescription {
            modifier: ModifierType::None,
            _type: String::new(),
        };
        fn parse_standalone_type(
            tokens: &mut TokenIter,
        ) -> String {
            let token = tokens.next().unwrap();
            match &token.token_type {
                I64 => "i64".to_string(),
                I32 => "i32".to_string(),
                I16 => "i16".to_string(),
                I8 => "i8".to_string(),
                F64 => "f64".to_string(),
                F32 => "f32".to_string(),
                Bool => "bool".to_string(),
                Ident(_ident) => todo!("Custom types parsing"),
                t => panic!(
                    "Syntax Error: expected type, found {t:?} at {}:{} ",
                    token.line, token.column
                ),
            }
        }
        fn parse_dependent_type(
            tokens: &mut TokenIter,
        ) -> ModifierType {
            let mut token = tokens.peek().unwrap();
            match token.token_type {
                OpenSquareBracket => {
                    tokens.next();
                    token = tokens.next().unwrap();
                    match token.token_type {
                        NumberLit(num) => {
                            token = tokens.next().unwrap();
                            match &token.token_type {
                                CloseSquareBracket => {
                                    ModifierType::FixedArray(num.parse().unwrap())
                                }
                                t => panic!(
                                    "Syntax Error: expected array parem, found {t:?} at {}:{} ",
                                    token.line, token.column
                                ),
                            }
                        }
                        t => panic!(
                            "Syntax Error: expected array parem, found {t:?} at {}:{} ",
                            token.line, token.column
                        ),
                    }
                }
                _ => ModifierType::None,
            }
        }
        parsed_type.modifier = parse_dependent_type(tokens);
        parsed_type._type = parse_standalone_type(tokens);

        parsed_type
    }

    fn parse_statement(
        &mut self,
        tokens: &mut TokenIter,
        variables: &mut HashMap<String, VariableProperties>,
        scope_return_type: Vec<TypeDescription>,
    ) -> String {
        let mut statement = String::new();
        fn parse_else(
            parser: &mut Parser,
            tokens: &mut TokenIter,
            variables: &mut HashMap<String, VariableProperties>,
            // token: &'a Token,
            scope_return_type: Vec<TypeDescription>,
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
        fn parse_if(
            parser: &mut Parser,
            tokens: &mut TokenIter,
            variables: &mut HashMap<String, VariableProperties>,
            token: Token,
            scope_return_type: Vec<TypeDescription>,
            statement: &mut String,
        ) {
            let expression = parser.parse_expression(tokens, variables);
            if expression.expression_type._type != "bool".to_string() {
                panic!(
                    "Error: expected bool type found {} at {}:{}",
                    expression.expression_type._type, token.line, token.column
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
        let mut token = tokens.peek().unwrap();
        match &token.token_type {
            Finish => {
                tokens.next();
                let expression = self.parse_expression(tokens, variables);
                if scope_return_type[0] != expression.expression_type {
                    todo!("Proper Error for handeling scope return and expression type mismatch");
                }
                statement.push_str(&expression.byte_code);
            }
            Return => {
                tokens.next();
                let expression = self.parse_expression(tokens, variables);
                if scope_return_type[0] != expression.expression_type {
                    todo!("Proper Error for handeling scope return and expression type mismatch");
                }
                statement.push_str(&expression.byte_code);
            }
            Var => {
                tokens.next();
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
                                statement.push_str(&self.parse_assignment(tokens, variables, &ident))
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
                tokens.next();
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
                                statement.push_str(&self.parse_assignment(tokens, variables, &ident))
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
                tokens.next();
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
                tokens.next();
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
            NewLine => {}
            SemiColon => {}

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

    fn parse_expression(
        &mut self,
        tokens: &mut TokenIter,
        variables: &HashMap<String, VariableProperties>,
    ) -> ExpressionReturn {
        let mut expression = ExpressionReturn {
            expression_type: TypeDescription {
                modifier: ModifierType::None,
                _type: String::new(),
            },
            byte_code: String::new(),
        };
        let mut expression_tokens = Vec::new();
        let mut no_of_open_square_brackets = 0;
        while match tokens.peek().unwrap().token_type {
            Ident(_) => true,
            DoubleEqual => true,
            NumberLit(_) => true,
            OpenSquareBracket => {
                no_of_open_square_brackets += 1;
                true
            }
            CloseSquareBracket => {
                if no_of_open_square_brackets > 0 {
                    no_of_open_square_brackets -= 1;
                    true
                } else {
                    false
                }
            }
            _ => false,
        } {
            let token = tokens.next().unwrap();
            expression_tokens.push(token);
        }
        let mut expr_tokens =
            TokenIter::new(expression_tokens);
        fn collect_expr(
            tokens: &mut TokenIter,
            variables: &HashMap<String, VariableProperties>,
            parser: &mut Parser,
        ) -> Expression {
            let token = tokens.next().unwrap();
            match &token.token_type {
                NumberLit(num) => {
                    let peeked = tokens.peek();
                    if let Some(token) = peeked {
                        // token needs to be an operation
                        match &token.token_type {
                            DoubleEqual => {
                                tokens.next();
                                if let Some(token) = tokens.peek() {
                                    let mut num_type = TypeDescription {
                                        modifier: ModifierType::None,
                                        _type: "i64".to_string(),
                                    };
                                    let rhs = collect_expr(tokens, variables, parser);
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
                                            // "Error: can not chain conditionals together like at {}:{}",
                                            // &token.line, &token.column
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
                            CloseSquareBracket => {

                            }
                            op => todo!("operation: {op:?} not implemented"),
                        }
                    }
                    return Expression::Value {
                        expr_type: TypeDescription {
                            modifier: ModifierType::None,
                            _type: "i64".to_string(),
                        },
                        value: Value::Number(num.clone()),
                    };
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
                                if let Some(token) = tokens.peek() {
                                    // let mut num_type = "i64".to_string();
                                    let mut rhs = collect_expr(tokens, variables, parser);
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
                                            // "Error: can not chain conditionals together like at {}:{}",
                                            // token.line, token.column
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
                            OpenSquareBracket => {
                                let index = collect_expr(tokens, variables, parser);
                                let mut next_token = tokens.next().unwrap();
                                if let CloseSquareBracket = next_token.token_type {
                                } else {
                                    todo!(
                                        "Proper error for when indexing into array and missing closing bracket"
                                    )
                                }
                                let array_expr_type = &variables.get(ident).unwrap().variable_type;
                                let expr_type = TypeDescription {
                                    modifier: ModifierType::None,
                                    _type: array_expr_type._type.clone()
                                };
                                Expression::IndexedArray {
                                    expr_type: expr_type.clone(),
                                    variable: ident.to_string(),
                                    index: Box::new(index),
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
                        expr_string.push_str(&format!("    {} push {number}\n", expr_type._type))
                    }
                },
                Expression::Conditional {
                    condition,
                    lhs,
                    rhs,
                } => {
                    expr_string.push_str(&parse_expr_tree(lhs, variables));
                    expr_string.push_str(&parse_expr_tree(rhs, variables));
                    let mut operation_type = TypeDescription {
                        modifier: ModifierType::None,
                        _type: "i64".to_string(),
                    };
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
                            if let ModifierType::None = operation_type.modifier {
                                expr_string.push_str(&format!("    {} eq\n", operation_type._type))
                            }
                        }
                        Condition::NotEqual => todo!(),
                    }
                    expr_string.push_str("    cast i8\n");
                }
                Expression::IndexedArray {
                    expr_type,
                    variable,
                    index,
                } => {
                    match &**index {
                        Expression::Variable(ident) => {
                            let variable = variables.get(ident).unwrap();
                            if (TypeDescription {
                                modifier: ModifierType::None,
                                _type: "i64".to_string(),
                            }) != variable.variable_type
                            {
                                todo!(
                                    "Proper error for indexing an array witha  variable of incorrect type"
                                )
                            }
                            expr_string.push_str(&format!("    get %{ident}\n"));
                        }
                        Expression::Value { expr_type, value } => match value {
                            Value::Custom(_) => todo!(
                                "Proper error for trying to index an array witha  non number value"
                            ),
                            Value::Number(number) => {
                                expr_string.push_str(&format!("    i64 push {number}\n"))
                            }
                        },
                        Expression::IndexedArray {
                            expr_type,
                            variable,
                            index,
                        } => expr_string.push_str(&parse_expr_tree(&index, variables)),
                        _ => todo!(
                            "Proper Error when trying to index an array with an incorrect expression type"
                        )
                    }
                    expr_string.push_str(&format!("    i64 push 1\n"));
                    expr_string.push_str(&format!("    i64 add\n"));
                    expr_string.push_str(&format!("    cpy %{variable} - top top\n"));

                } 

                
            }
            expr_string
        }
        let expression_tree = collect_expr(&mut expr_tokens, variables, self);
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
            } => {
                expression.expression_type = TypeDescription {
                    modifier: ModifierType::None,
                    _type: "bool".to_string(),
                }
            }
            Expression::IndexedArray {
                expr_type,
                variable,
                index,
            } => expression.expression_type = expr_type.clone(),
        }

        expression
    }

    fn parse_assignment(
        &mut self,
        tokens: &mut TokenIter,
        variables: &mut HashMap<String, VariableProperties>,
        variable_to_be_assign: &String,
    ) -> String {
        let mut assignment = String::new();
        let peeked_token = tokens.peek().unwrap();
        let mut variable_to_be_assign_type = variables
            .get(variable_to_be_assign)
            .unwrap()
            .variable_type
            .clone();
        fn parse_value_assignment(
            token: &Token,
            variable_to_be_assign: &String,
            variables: &mut HashMap<String, VariableProperties>,
            state: &mut Parser,
            tokens: &mut TokenIter,
            assignment: &mut String,
            variable_to_be_assign_type: TypeDescription,
        ) {
            match &token.token_type {
                OpenCurlyBracket => {
                    let varibale_type = variables
                        .get(variable_to_be_assign)
                        .unwrap()
                        .variable_type
                        .clone();
                    assignment.push_str(&state.parse_scope(tokens, vec![varibale_type], variables))
                }
                Equal => {
                    tokens.next().unwrap();
                    let mut token = tokens.peek().unwrap();

                    match &token.token_type {
                        NumberLit(num) => {
                            let expression = state.parse_expression(tokens, variables);
                            assignment.push_str(&expression.byte_code);
                        }
                        Ident(ident) => {
                            let expression = state.parse_expression(tokens, variables);
                            assignment.push_str(&expression.byte_code);
                        }
                        True => {
                            tokens.next();
                            if variable_to_be_assign_type
                                == (TypeDescription {
                                    modifier: ModifierType::None,
                                    _type: "bool".to_string(),
                                })
                            {
                                assignment.push_str(&format!("    i8 push 1"));
                            } else {
                                panic!(
                                    "Error: tried assigning a bool type to {:?} at {}:{}",
                                    token.token_type, token.line, token.column
                                )
                            }
                        }
                        False => {
                            tokens.next();
                            if variable_to_be_assign_type
                                == (TypeDescription {
                                    modifier: ModifierType::None,
                                    _type: "bool".to_string(),
                                })
                            {
                                assignment.push_str(&format!("    i8 push 0"));
                            } else {
                                panic!(
                                    "Error: tried assigning a bool type to {:?} at {}:{}",
                                    token.token_type, token.line, token.column
                                )
                            }
                        }
                        OpenSquareBracket => {
                            tokens.next();
                            assignment.push_str(&state.parse_array_expr(
                                tokens,
                                variables,
                                variable_to_be_assign_type,
                            ));
                        }
                        t => panic!(
                            "Syntax Error: expected int or variable, found {t:?} at {}:{} ",
                            token.line, token.column
                        ),
                    }
                }
                t => panic!(
                    "Syntax Error: expected {{}} or = , found {t:?} at {}:{} ",
                    token.line, token.column
                ),
            }
        }
        match &peeked_token.token_type {
            OpenCurlyBracket => {
                parse_value_assignment(
                    &peeked_token,
                    variable_to_be_assign,
                    variables,
                    self,
                    tokens,
                    &mut assignment,
                    variable_to_be_assign_type,
                );
                assignment.push_str(&format!("    set %{variable_to_be_assign}\n"));
            }
            Equal => {
                parse_value_assignment(
                    &peeked_token,
                    variable_to_be_assign,
                    variables,
                    self,
                    tokens,
                    &mut assignment,
                    variable_to_be_assign_type,
                );
                assignment.push_str(&format!("    set %{variable_to_be_assign}\n"));
            }
            OpenSquareBracket => {
                let array_index: usize;
                let mut token;
                tokens.next().unwrap();
                token = tokens.next().unwrap();
                if let NumberLit(num) = &token.token_type {
                    array_index = num.parse().unwrap();
                } else {
                    todo!("Proper error for when a none number is used to index array")
                }
                token = tokens.next().unwrap();
                if let CloseSquareBracket = token.token_type {
                    // array_index = num.parse().unwrap();
                } else {
                    todo!("Proper error for when a ] isnt used to end array indexing")
                }
                // out of bounds check
                if let ModifierType::FixedArray(array_size) = variable_to_be_assign_type.modifier {
                    if array_index >= array_size {
                        todo!("Proper error for indexing out of array bounds")
                    }
                }
                let peeked_token = tokens.peek().unwrap();
                variable_to_be_assign_type.modifier = ModifierType::None;
                parse_value_assignment(
                    &peeked_token,
                    variable_to_be_assign,
                    variables,
                    self,
                    tokens,
                    &mut assignment,
                    variable_to_be_assign_type,
                );
                assignment.push_str(&format!(
                    "    set stack %{variable_to_be_assign} - {}\n", array_index +1
                ));
            }
            t => panic!(
                "Syntax Error: expected {{}} or = , found {t:?} at {}:{} ",
                peeked_token.line, peeked_token.column
            ),
        }
        assignment
    }

    fn parse_array_expr(
        &mut self,
        tokens: &mut TokenIter,
        variables: &mut HashMap<String, VariableProperties>,
        array_type: TypeDescription,
    ) -> String {
        let mut parsed_array = String::new();
        match array_type.modifier {
            ModifierType::DynamicArray => todo!(),
            ModifierType::FixedArray(size) => {
                let mut token;
                let mut elem_index = 0;
                let mut elements = Vec::new();
                while tokens.peek().unwrap().token_type != CloseSquareBracket {
                    if elem_index > size {
                        todo!("Proper Error for when an array expr is longer than array type")
                    }
                    // tokens.next();
                    // _ = tokens.next().unwrap();
                    elements.push(self.parse_expression(tokens, variables));

                    token = tokens.next().unwrap();
                    if let Comma = token.token_type {
                        _ = tokens.next_if(|token| token.token_type == NewLine);
                    } else {
                        todo!(
                            "Proper Error a comma not being present after an expression during array parsing"
                        )
                    }
                    elem_index += 1;
                }
                for element in elements.iter().rev() {
                    if element.expression_type._type != array_type._type {
                        todo!("Proper Error for when an array expr isnt array element type")
                    }
                    parsed_array.push_str(&element.byte_code);
                }
                parsed_array.push_str(&format!("    i64 push {size}\n"));
                parsed_array.push_str(&format!("    i64 push top\n"));
                tokens.next();
            }
            _ => todo!("Proper Error for trying to assing a array expr to a none array type"),
        }
        parsed_array
    }
}

pub fn parse(tokens: &mut TokenIter) -> String {
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
