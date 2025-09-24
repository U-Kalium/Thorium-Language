use std::collections::HashMap;
use std::collections::HashSet;

use dynasmrt::Modifier;

use crate::compiler::syntax_tree::*;
use crate::compiler::tokenizer::TokenType;
use crate::compiler::tokenizer::TokenType::*;
use crate::compiler::tokenizer::{Token, TokenIter};

const LEFT_RIGHT_OPERATIONS: [TokenType; 7] = [
    DoubleEqual,
    GreatEqual,
    LessEqual,
    Add,
    Minus,
    Star,
    ForwardSlash,
];

struct Parser {
    functions: HashMap<String, FunctionDef>,
    types: HashSet<String>,
    highest_scope_return_type: Option<TypeDescription>,
}

impl Parser {
    fn new() -> Self {
        Parser {
            functions: HashMap::new(),
            types: HashSet::new(),
            highest_scope_return_type: None,
        }
    }

    fn parse_if_expr(
        &mut self,
        tokens: &mut TokenIter,
        variables: &mut HashMap<String, Variable>,
        expected_type: TypeDescription,
    ) -> Expression {
        let mut token = tokens.next().unwrap();
        let location;
        if let If = token.token_type {
            location = format!("L{}C{}", token.line, token.column);
        } else {
            panic!(
                "Syntax Error: expected if found {:?} at {}:{}",
                token.token_type, token.line, token.column
            )
        }
        let condition = self.parse_expression(tokens, variables, TypeDescription::bool());
        let expression = self.parse_expression(tokens, variables, expected_type.clone());
        let mut else_branch = None;
        token = tokens.next().unwrap();
        if let Else = token.token_type {
            else_branch = Some(Box::new(self.parse_expression(
                tokens,
                variables,
                expected_type.clone(),
            )));
            tokens.next().unwrap();
        }
        Expression {
            expr_type: expected_type,
            expression: ExpressionType::If {
                location,
                condition: Box::new(condition),
                expression: Box::new(expression),
                else_branch,
            },
        }
    }

    fn parse_loop_expr(
        &mut self,
        tokens: &mut TokenIter,
        variables: &mut HashMap<String, Variable>,
        expected_type: TypeDescription,
    ) -> Expression {
        let loop_token = tokens.next().unwrap();
        let location = format!("L{}C{}", loop_token.line, loop_token.column);

        let mut peeked = tokens.peek().unwrap();
        match peeked.token_type {
            Ident(ident) => {
                // while/for loop
                if variables.contains_key(&ident) {
                    tokens.next().unwrap();
                    peeked = tokens.peek().unwrap();
                    tokens.back();
                    match peeked.token_type {
                        Equal => self.parse_for_loop(tokens, variables, expected_type, location),
                        _ => self.parse_while_loop(tokens, variables, expected_type, location),
                    }
                } else {
                    // for each loop
                    self.parse_foreach_loop(tokens, variables, expected_type)
                }
            }
            True => self.parse_while_loop(tokens, variables, expected_type, location),
            False => self.parse_while_loop(tokens, variables, expected_type, location),
            _ => self.parse_for_loop(tokens, variables, expected_type, location),
        }
    }

    fn parse_while_loop(
        &mut self,
        tokens: &mut TokenIter,
        variables: &mut HashMap<String, Variable>,
        expected_type: TypeDescription,
        location: String,
    ) -> Expression {
        let condition = self.parse_expression(tokens, variables, TypeDescription::bool());
        let expression = self.parse_expression(tokens, variables, expected_type.clone());
        tokens.next().unwrap();
        Expression {
            expr_type: expected_type,
            expression: ExpressionType::WhileLoop {
                location,
                condition: Box::new(condition),
                expression: Box::new(expression),
            },
        }
    }

    fn parse_foreach_loop(
        &mut self,
        tokens: &mut TokenIter,
        variables: &mut HashMap<String, Variable>,
        expected_type: TypeDescription,
    ) -> Expression {
        todo!()
    }

    fn parse_for_loop(
        &mut self,
        tokens: &mut TokenIter,
        variables: &mut HashMap<String, Variable>,
        expected_type: TypeDescription,
        location: String
    ) -> Expression {
        let first_statement = self.parse_statement(tokens, variables, TypeDescription::void());
        let condition = self.parse_expression(tokens, variables, TypeDescription::bool());
        tokens.next().unwrap();
        let last_statement = self.parse_statement(tokens, variables, TypeDescription::void());
        let expression = self.parse_expression(tokens, variables, expected_type.clone());
        tokens.next().unwrap();

        Expression {
            expr_type: expected_type,
            expression: ExpressionType::ForLoop {
                location,
                first_statement: Box::new(first_statement),
                condition: Box::new(condition),
                last_statement: Box::new(last_statement),
                expression: Box::new(expression),
            },
        }
    }

    fn parse_declaration(
        &mut self,
        tokens: &mut TokenIter,
        variables: &mut HashMap<String, Variable>,
        // statement: &mut String,
        is_mutable: bool,
    ) -> Statement {
        tokens.next();
        let token = tokens.next().unwrap();
        match &token.token_type {
            Ident(ident) => {
                // token = tokens.next().unwrap();
                let mut expression = None;
                let variable = Variable {
                    variable_type: self.parse_type(tokens),
                    is_mutable: is_mutable,
                    ident: ident.to_owned(),
                };
                variables.insert(ident.to_string(), variable.clone());
                let peeked = tokens.peek().unwrap();
                if let NewLine | SemiColon = peeked.token_type {
                } else {
                    expression = Some(self.parse_assignment(tokens, variables, &variable));
                }

                Statement::VariableDecleration {
                    variable: variable,
                    assignment: expression,
                }
            }
            t => panic!(
                "Syntax Error: expected identifier, found {t:?} at {}:{} ",
                token.line, token.column
            ),
        }
    }

    fn parse_func_type(&mut self, tokens: &mut TokenIter) -> TypeDef {
        let mut func_type = FunctionDef::new();
        let mut token = tokens.next().unwrap();
        match &token.token_type {
            OpenBracket => {
                let peeked = tokens.peek().unwrap();
                while !matches!(peeked.token_type, CloseBracket) {
                    token = tokens.next().unwrap();
                    match token.token_type {
                        Ident(ident) => todo!("add function paremeters"),
                        t => panic!(
                            "Syntax Error: expected ) or ident for function args, found {t:?} at {}:{} ",
                            token.line, token.column
                        ),
                    }
                }
                tokens.next();
            }
            OpenAngleBracket => {
                todo!("generics")
            }
            t => panic!(
                "Syntax Error: expected () or <>, found {t:?} at {}:{} ",
                token.line, token.column
            ),
        }

        let return_type = self.parse_type(tokens);
        func_type.returns = Box::new(return_type);

        TypeDef::Func(func_type)
    }

    fn parse_scope(
        &mut self,
        tokens: &mut TokenIter,
        return_type: TypeDescription,
        variables: &mut HashMap<String, Variable>,
    ) -> Expression {
        // let mut variables = HashMap::new();
        // let mut scope = Expression {
        //     expr_type: return_type,
        //     expression: ExpressionType::Scope { statements: Vec::new() }
        // };
        let mut is_highest_scope = false;
        if let None = self.highest_scope_return_type {
            is_highest_scope = true;
            self.highest_scope_return_type = Some(return_type.clone())
        }
        let mut statements = Vec::new();
        let mut token = tokens.next().unwrap();
        match &token.token_type {
            OpenCurlyBracket => {}
            t => panic!(
                "Syntax Error: expected {{, found {t:?} at {}:{} ",
                token.line, token.column
            ),
        }
        token = tokens.next().unwrap();
        while token.token_type != CloseCurlyBracket {
            statements.push(self.parse_statement(tokens, variables, return_type.clone()));
            token = tokens.peek().unwrap();
        }
        tokens.next();
        if is_highest_scope {
            self.highest_scope_return_type = None
        }
        Expression {
            expr_type: return_type,
            expression: ExpressionType::Scope { statements },
        }
    }

    fn parse_type(&mut self, tokens: &mut TokenIter) -> TypeDescription {
        let mut parsed_type = TypeDescription {
            modifier: ModifierType::None,
            _type: TypeDef::new(),
        };
        fn parse_standalone_type(parser: &mut Parser, tokens: &mut TokenIter) -> TypeDef {
            let token = tokens.next().unwrap();
            match &token.token_type {
                I64 => TypeDef::Number(NumberDef::Integer(IntegerDef::I64)),
                I32 => TypeDef::Number(NumberDef::Integer(IntegerDef::I32)),
                I16 => TypeDef::Number(NumberDef::Integer(IntegerDef::I16)),
                I8 => TypeDef::Number(NumberDef::Integer(IntegerDef::I8)),
                F64 => TypeDef::Number(NumberDef::Float(FloatDef::F64)),
                F32 => TypeDef::Number(NumberDef::Float(FloatDef::F32)),
                Bool => TypeDef::Boolean,
                OpenBracket => {
                    tokens.back();
                    parser.parse_func_type(tokens)
                }
                Ident(_ident) => todo!("Custom types parsing"),
                t => panic!(
                    "Syntax Error: expected type, found {t:?} at {}:{} ",
                    token.line, token.column
                ),
            }
        }
        fn parse_dependent_type(tokens: &mut TokenIter) -> ModifierType {
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
        parsed_type._type = parse_standalone_type(self, tokens);

        parsed_type
    }

    fn parse_statement(
        &mut self,
        tokens: &mut TokenIter,
        variables: &mut HashMap<String, Variable>,
        scope_return_type: TypeDescription,
    ) -> Statement {
        let mut statement;

        let mut token = tokens.peek().unwrap();
        match &token.token_type {
            Finish => {
                tokens.next();
                if let Some(return_type) = self.highest_scope_return_type.clone() {
                    let expression = self.parse_expression(tokens, variables, return_type.clone());
                    if return_type != expression.expr_type {
                        todo!(
                            "Proper Error for handeling mismatch expected {return_type:?} got {:?}",
                            expression.expr_type
                        );
                    }
                    statement = Statement::Finish { expression }
                    // statement.push_str(&expression.byte_code);
                } else {
                    panic!("Use of finish when there is no highest scope return type")
                }

                // statement.push_str("    return\n");
            }
            Return => {
                tokens.next();
                let expression =
                    self.parse_expression(tokens, variables, scope_return_type.clone());
                if scope_return_type != expression.expr_type {
                    todo!("Proper Error for handeling scope return and expression type mismatch");
                }
                // statement.push_str(&expression.byte_code);
                statement = Statement::Return { expression }
            }
            Var => {
                statement = self.parse_declaration(tokens, variables, true);
            }
            Let => {
                statement = self.parse_declaration(tokens, variables, false);
            }
            If | Loop => {
                let expression = self.parse_expression(tokens, variables, TypeDescription::void());
                statement = Statement::Expression(expression);
                tokens.back();
                if let NewLine | SemiColon = tokens.current().token_type {
                    tokens.back();
                } else {
                }
            }
            Ident(ident) => {
                tokens.next();
                if let Some(variable) = variables.get(ident) {
                    if !variable.is_mutable {
                        panic!(
                            "Error: tried mutating variable {ident} but it is immutable, found at {}:{}",
                            token.line, token.column
                        )
                    }
                    if let OpenSquareBracket = tokens.peek().unwrap().token_type {
                        let array_index: usize;
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
                        if let ModifierType::FixedArray(array_size) =
                            variable.variable_type.modifier
                        {
                            if array_index >= array_size {
                                todo!("Proper error for indexing out of array bounds")
                            }
                        }
                        let expression =
                            self.parse_assignment(tokens, &mut variables.clone(), variable);

                        statement = Statement::Assignment {
                            variable: variable.clone(),
                            expression: expression,
                            array_index: Some(array_index),
                        }
                    } else {
                        let expression =
                            self.parse_assignment(tokens, &mut variables.clone(), variable);
                        statement = Statement::Assignment {
                            variable: variable.clone(),
                            expression: expression,
                            array_index: None,
                        }
                    }

                    // statement.push_str(&self.parse_assignment(tokens, variables, ident));
                } else {
                    panic!(
                        "Error: variable {ident} does not exist, found at {}:{}",
                        token.line, token.column
                    )
                }
            }
            NewLine | SemiColon => statement = Statement::Empty,
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
        variables: &mut HashMap<String, Variable>,
        expected_type: TypeDescription,
    ) -> Expression {
        // let mut expression = ExpressionReturn {
        //     expression_type: TypeDescription {
        //         modifier: ModifierType::None,
        //         _type: TypeDef::new(),
        //     },
        //     byte_code: String::new(),
        // };
        let expression_tokens = Vec::new();
        // while match tokens.peek().unwrap().token_type {
        //     If => true,
        //     Else => true,
        //     Loop => true,
        //     DoubleEqual | GreatEqual | LessEqual | Add | Minus | Star | ForwardSlash
        //     | OpenAngleBracket | CloseAngleBracket => true,
        //     NumberLit(_) | Ident(_) => true,
        //     OpenBracket => {
        //         no_of_open_brackets += 1;
        //         true
        //     }
        //     CloseBracket => {
        //         if no_of_open_brackets > 0 {
        //             no_of_open_brackets -= 1;
        //             true
        //         } else {
        //             false
        //         }
        //     }
        //     OpenSquareBracket => {
        //         no_of_open_square_brackets += 1;
        //         true
        //     }
        //     CloseSquareBracket => {
        //         if no_of_open_square_brackets > 0 {
        //             no_of_open_square_brackets -= 1;
        //             true
        //         } else {
        //             false
        //         }
        //     }
        //     OpenCurlyBracket => {
        //         no_of_open_curly_brackets += 1;
        //         true
        //     }
        //     CloseCurlyBracket => {
        //         if no_of_open_curly_brackets > 0 {
        //             no_of_open_curly_brackets -= 1;
        //             if no_of_open_curly_brackets == 0 {
        //                 no_of_open_curly_brackets -= 1;
        //             }
        //             true
        //         } else {
        //             false
        //         }
        //     }
        //     _ => no_of_open_curly_brackets > 0,
        // } {
        //     let token = tokens.next().unwrap();
        //     expression_tokens.push(token);
        //     if no_of_open_curly_brackets < 0 {
        //         break;
        //     }
        // }
        let mut expr_tokens = TokenIter::new(expression_tokens);
        fn collect_condition_expr(
            condition: Condition,
            tokens: &mut TokenIter,
            parser: &mut Parser,
            variables: &mut HashMap<String, Variable>,
            num_or_ident: &String,
            token: Token,
            is_var: bool,
        ) -> Expression {
            tokens.next();
            if let Some(token) = tokens.peek() {
                let mut num_type = TypeDescription {
                    modifier: ModifierType::None,
                    _type: TypeDef::from_int(IntegerDef::I64),
                };
                let mut rhs =
                    collect_expr(TypeDescription::default_int(), tokens, variables, parser);
                match &mut rhs {
                    Expression {
                        expr_type,
                        expression: ExpressionType::Value { value: _value },
                    } => {
                        let prop = variables.get(num_or_ident).unwrap();
                        *expr_type = prop.variable_type.clone()
                    }
                    Expression {
                        expr_type,
                        expression:
                            ExpressionType::ConditionalOp {
                                condition: _,
                                lhs: _,
                                rhs: _,
                            },
                    } => panic!(
                        // "Error: can not chain conditionals together like at {}:{}",
                        // token.line, token.column
                    ),
                    _ => {}
                }
                if is_var {
                    let var = variables.get(num_or_ident).unwrap().clone();
                    let expr_typ = ExpressionType::ConditionalOp {
                        condition,
                        lhs: Box::new(Expression {
                            expr_type: var.clone().variable_type,
                            expression: ExpressionType::Variable(var),
                        }),
                        rhs: Box::new(rhs),
                    };
                    return Expression {
                        expr_type: TypeDescription::bool(),
                        expression: expr_typ,
                    };
                } else {
                    let val = Expression {
                        expr_type: num_type,
                        expression: ExpressionType::Value {
                            value: Value::Number(num_or_ident.clone()),
                        },
                    };
                    let expr_type = ExpressionType::ConditionalOp {
                        condition: condition,
                        lhs: Box::new(val),
                        rhs: Box::new(rhs),
                    };
                    return Expression {
                        expr_type: TypeDescription::bool(),
                        expression: expr_type,
                    };
                }
            } else {
                panic!(
                    "Syntax Error: expected ident or literal found nothing at {}:{}",
                    token.line, token.column
                )
            }
        }

        fn collect_expr(
            expected_type: TypeDescription,
            tokens: &mut TokenIter,
            variables: &mut HashMap<String, Variable>,
            parser: &mut Parser,
        ) -> Expression {
            let token = tokens.next().unwrap();
            match &token.token_type {
                NumberLit(num) => {
                    return match_expr_start(expected_type, tokens, variables, parser, num, false);
                }
                Ident(ident) => {
                    if !variables.contains_key(ident) {
                        panic!(
                            "Error: variable {ident} has not been declared in the scope but is used at {}:{}",
                            token.line, token.column
                        )
                    }
                    return match_expr_start(expected_type, tokens, variables, parser, ident, true);
                }
                OpenCurlyBracket => {
                    tokens.back();
                    return parser.parse_scope(tokens, expected_type, variables);
                }
                OpenBracket => {
                    tokens.back();
                    let atom = parse_atom(expected_type.clone(), tokens, variables, parser);
                    match &tokens.peek().unwrap().token_type {
                        DoubleEqual | LessEqual | GreatEqual | OpenAngleBracket
                        | CloseAngleBracket => todo!(),
                        Add | Star | Minus | ForwardSlash => {
                            return collect_arith_expr(
                                expected_type,
                                tokens,
                                variables,
                                parser,
                                0,
                                Some(atom),
                            );
                        }
                        _ => todo!(),
                    }
                }
                If => {
                    tokens.back();
                    return parser.parse_if_expr(tokens, variables, expected_type);
                }
                Loop => {
                    tokens.back();
                    return parser.parse_loop_expr(tokens, variables, expected_type);
                }
                wrong_token => panic!(
                    "Syntax Error: expected expression found {wrong_token:?} at {}:{}",
                    token.line, token.column
                ),
            }

            fn match_expr_start(
                expected_type: TypeDescription,
                tokens: &mut TokenIter,
                variables: &mut HashMap<String, Variable>,
                parser: &mut Parser,
                num_or_ident: &String,
                is_var: bool,
            ) -> Expression {
                if let Some(token) = tokens.peek() {
                    // token needs to be an operation
                    return match &token.token_type {
                        DoubleEqual => collect_condition_expr(
                            Condition::Equal,
                            tokens,
                            parser,
                            variables,
                            num_or_ident,
                            token,
                            true,
                        ),
                        LessEqual => collect_condition_expr(
                            Condition::LessEqual,
                            tokens,
                            parser,
                            variables,
                            num_or_ident,
                            token,
                            true,
                        ),
                        GreatEqual => collect_condition_expr(
                            Condition::GreatEqual,
                            tokens,
                            parser,
                            variables,
                            num_or_ident,
                            token,
                            true,
                        ),
                        OpenAngleBracket => collect_condition_expr(
                            Condition::LessThan,
                            tokens,
                            parser,
                            variables,
                            num_or_ident,
                            token,
                            true,
                        ),
                        CloseAngleBracket => collect_condition_expr(
                            Condition::GreaterThan,
                            tokens,
                            parser,
                            variables,
                            num_or_ident,
                            token,
                            true,
                        ),
                        OpenSquareBracket => {
                            tokens.next();
                            let index = collect_expr(
                                TypeDescription::default_int(),
                                tokens,
                                variables,
                                parser,
                            );
                            let next_token = tokens.next().unwrap();
                            if let CloseSquareBracket = next_token.token_type {
                            } else {
                                todo!(
                                    "Proper error for when indexing into array and missing closing bracket"
                                )
                            }
                            let var = variables.get(num_or_ident).unwrap();
                            let _type = TypeDescription {
                                modifier: ModifierType::None,
                                _type: var.variable_type._type.clone(),
                            };
                            let expr_type = ExpressionType::IndexedArray {
                                variable: var.clone(),
                                index: Box::new(index),
                            };
                            Expression {
                                expr_type: _type,
                                expression: expr_type,
                            }
                        }
                        Add => {
                            tokens.back();
                            let expr = collect_arith_expr(
                                expected_type,
                                tokens,
                                variables,
                                parser,
                                0,
                                None,
                            );
                            expr
                        }
                        Star => {
                            tokens.back();
                            let expr = collect_arith_expr(
                                expected_type,
                                tokens,
                                variables,
                                parser,
                                0,
                                None,
                            );
                            expr
                        }
                        Minus => {
                            tokens.back();
                            let expr = collect_arith_expr(
                                expected_type,
                                tokens,
                                variables,
                                parser,
                                0,
                                None,
                            );
                            expr
                        }
                        ForwardSlash => {
                            tokens.back();
                            let expr = collect_arith_expr(
                                expected_type,
                                tokens,
                                variables,
                                parser,
                                0,
                                None,
                            );
                            expr
                        }
                        CloseSquareBracket => {
                            if is_var {
                                let var = variables.get(num_or_ident).unwrap().clone();
                                return Expression {
                                    expr_type: var.clone().variable_type,
                                    expression: ExpressionType::Variable(var),
                                };
                            } else {
                                return Expression {
                                    expr_type: TypeDescription {
                                        modifier: ModifierType::None,
                                        _type: expected_type._type,
                                    },
                                    expression: ExpressionType::Value {
                                        value: Value::Number(num_or_ident.clone()),
                                    },
                                };
                            }
                        }
                        // op => todo!("operation: {op:?} not implemented"),
                        op => {
                            if is_var {
                                let var = variables.get(num_or_ident).unwrap().clone();
                                return Expression {
                                    expr_type: var.clone().variable_type,
                                    expression: ExpressionType::Variable(var),
                                };
                            } else {
                                return Expression {
                                    expr_type: TypeDescription {
                                        modifier: ModifierType::None,
                                        _type: expected_type._type,
                                    },
                                    expression: ExpressionType::Value {
                                        value: Value::Number(num_or_ident.to_string()),
                                    },
                                };
                            }
                        }
                    };
                } else {
                    if is_var {
                        let var = variables.get(num_or_ident).unwrap().clone();
                        return Expression {
                            expr_type: var.clone().variable_type,
                            expression: ExpressionType::Variable(var),
                        };
                    } else {
                        return Expression {
                            expr_type: TypeDescription {
                                modifier: ModifierType::None,
                                _type: expected_type._type,
                            },
                            expression: ExpressionType::Value {
                                value: Value::Number(num_or_ident.to_string()),
                            },
                        };
                    }
                }
            }

            fn parse_atom(
                expected_type: TypeDescription,
                tokens: &mut TokenIter,
                variables: &HashMap<String, Variable>,
                parser: &mut Parser,
            ) -> Expression {
                if let Some(token) = tokens.next() {
                    match token.token_type {
                        OpenBracket => {
                            let expr = collect_arith_expr(
                                expected_type,
                                tokens,
                                variables,
                                parser,
                                0,
                                None,
                            );
                            let token = tokens.next().unwrap();
                            if token.token_type != CloseBracket {
                                todo!("Proper Error for unmatched '('")
                            }
                            expr
                        }
                        NumberLit(num) => Expression {
                            expr_type: expected_type,
                            expression: ExpressionType::Value {
                                value: Value::Number(num),
                            },
                        },
                        Ident(ident) => {
                            let var = variables.get(&ident).unwrap().clone();
                            Expression {
                                expr_type: expected_type,
                                expression: ExpressionType::Variable(var),
                            }
                        }
                        wrong_token => todo!("{:?}", wrong_token),
                    }
                } else {
                    todo!()
                }
            }

            fn collect_arith_expr(
                expected_type: TypeDescription,
                tokens: &mut TokenIter,
                variables: &HashMap<String, Variable>,
                parser: &mut Parser,
                min_prec: usize,
                lhs: Option<Expression>,
            ) -> Expression {
                let mut atom_lhs;
                if let Some(lhs) = lhs {
                    atom_lhs = lhs
                } else {
                    atom_lhs = parse_atom(expected_type.clone(), tokens, variables, parser);
                }

                loop {
                    let token;
                    if let Some(tok) = tokens.next() {
                        token = tok
                    } else {
                        break;
                    }
                    if !token.is_bin_op() {
                        tokens.back();
                        break;
                    }

                    let (prec, operation) = match token.token_type {
                        Add => (0, ArithmaticOp::Addition),
                        Minus => (0, ArithmaticOp::Subtraction),
                        ForwardSlash => (1, ArithmaticOp::Division),
                        Star => (1, ArithmaticOp::Multiplication),
                        op => todo!("Proper error for an invalid bin operator {:?}", op),
                    };

                    if prec < min_prec {
                        break;
                    }
                    let next_min_proc = prec + 1;
                    let atom_rhs = collect_arith_expr(
                        expected_type.clone(),
                        tokens,
                        variables,
                        parser,
                        next_min_proc,
                        None,
                    );
                    atom_lhs = Expression {
                        expr_type: expected_type.clone(),
                        expression: ExpressionType::ArithmaticOp {
                            op: operation,
                            lhs: Box::new(atom_lhs),
                            rhs: Box::new(atom_rhs),
                        },
                    }
                }
                if let Some(token) = tokens.peek() {
                    if token.token_type == CloseBracket {
                        return atom_lhs;
                    } else if token.is_bin_op() {
                        tokens.back();
                        collect_arith_expr(
                            expected_type.clone(),
                            tokens,
                            variables,
                            parser,
                            0,
                            Some(atom_lhs),
                        )
                    } else {
                        return atom_lhs;
                    }
                } else {
                    return atom_lhs;
                }
                // if tokens.peek().is_none() {
                //     return atom_lhs;
                // } else {
                //     tokens.back();
                //     collect_arith_expr(
                //         expected_type.clone(),
                //         tokens,
                //         variables,
                //         parser,
                //         0,
                //         Some(atom_lhs),
                //     )
                // }
                // return atom_lhs;
            }
        }

        let expression_tree = collect_expr(expected_type, tokens, variables, self);
        // dbg!(&expression_tree);
        // dbg!(&byte_code);

        // expression.byte_code = byte_code.clone();

        // match &expression_tree {
        //     Expression::Variable(var) => {
        //         expression.expression_type = var.variable_type
        //     }
        //     Expression::Value {
        //         expr_type,
        //         value: _,
        //     } => expression.expression_type = expr_type.clone(),
        //     Expression::ConditionalOp {
        //         condition: _,
        //         lhs: _,
        //         rhs: _,
        //     } => {
        //         expression.expression_type = TypeDescription {
        //             modifier: ModifierType::None,
        //             _type: TypeDef::Boolean,
        //         }
        //     }
        //     Expression::IndexedArray {
        //         expr_type,
        //         variable,
        //         index,
        //     } => expression.expression_type = expr_type.clone(),
        //     Expression::ArithmaticOp {
        //         expr_type,
        //         op,
        //         lhs,
        //         rhs,
        //     } => expression.expression_type = expr_type.clone(),
        // }

        expression_tree
    }

    fn parse_assignment(
        &mut self,
        tokens: &mut TokenIter,
        variables: &mut HashMap<String, Variable>,
        variable_to_be_assign: &Variable,
    ) -> Expression {
        // let mut assignment = String::new();
        let peeked_token = tokens.peek().unwrap();
        let mut variable_to_be_assign_type = variable_to_be_assign.variable_type.clone();
        fn parse_value_assignment(
            token: &Token,
            variables: &mut HashMap<String, Variable>,
            state: &mut Parser,
            tokens: &mut TokenIter,
            variable_to_be_assign_type: TypeDescription,
        ) -> Expression {
            match &token.token_type {
                OpenCurlyBracket => {
                    let varibale_type = variable_to_be_assign_type;
                    if let TypeDescription {
                        modifier: ModifierType::None,
                        _type: TypeDef::Func(func_def),
                    } = varibale_type
                    {
                        state.parse_scope(tokens, *func_def.returns, variables)
                    } else {
                        state.parse_scope(tokens, varibale_type, variables)
                    }
                }
                Equal => {
                    tokens.next().unwrap();
                    let mut token = tokens.peek().unwrap();

                    match &token.token_type {
                        NumberLit(num) => {
                            let expression = state.parse_expression(
                                tokens,
                                variables,
                                variable_to_be_assign_type,
                            );
                            expression
                        }
                        Ident(ident) => {
                            let expression = state.parse_expression(
                                tokens,
                                variables,
                                variable_to_be_assign_type,
                            );
                            expression
                        }
                        True => {
                            tokens.next();
                            if variable_to_be_assign_type
                                == (TypeDescription {
                                    modifier: ModifierType::None,
                                    _type: TypeDef::Boolean,
                                })
                            {
                                Expression {
                                    expr_type: TypeDescription::bool(),
                                    expression: ExpressionType::Value {
                                        value: Value::Boolean(true),
                                    },
                                }

                                // assignment.push_str(&format!("    i8 push 1"));
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
                                    _type: TypeDef::Boolean,
                                })
                            {
                                Expression {
                                    expr_type: TypeDescription::bool(),
                                    expression: ExpressionType::Value {
                                        value: Value::Boolean(false),
                                    },
                                }

                                // assignment.push_str(&format!("    i8 push 0"));
                            } else {
                                panic!(
                                    "Error: tried assigning a bool type to {:?} at {}:{}",
                                    token.token_type, token.line, token.column
                                )
                            }
                        }
                        OpenSquareBracket => {
                            tokens.next();
                            state.parse_array_expr(tokens, variables, variable_to_be_assign_type)
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
                    variables,
                    self,
                    tokens,
                    variable_to_be_assign_type,
                )
                // assignment.push_str(&format!("    set %{variable_to_be_assign}\n"));
            }
            Equal => {
                parse_value_assignment(
                    &peeked_token,
                    variables,
                    self,
                    tokens,
                    variable_to_be_assign_type,
                )
                // assignment.push_str(&format!("    set %{variable_to_be_assign}\n"));
            }
            // OpenSquareBracket => {
            //     let array_index: usize;
            //     let mut token;
            //     tokens.next().unwrap();
            //     token = tokens.next().unwrap();
            //     if let NumberLit(num) = &token.token_type {
            //         array_index = num.parse().unwrap();
            //     } else {
            //         todo!("Proper error for when a none number is used to index array")
            //     }
            //     token = tokens.next().unwrap();
            //     if let CloseSquareBracket = token.token_type {
            //         // array_index = num.parse().unwrap();
            //     } else {
            //         todo!("Proper error for when a ] isnt used to end array indexing")
            //     }
            //     // out of bounds check
            //     if let ModifierType::FixedArray(array_size) = variable_to_be_assign_type.modifier {
            //         if array_index >= array_size {
            //             todo!("Proper error for indexing out of array bounds")
            //         }
            //     }
            //     let peeked_token = tokens.peek().unwrap();
            //     variable_to_be_assign_type.modifier = ModifierType::None;
            //     parse_value_assignment(
            //         &peeked_token,
            //         variables,
            //         self,
            //         tokens,
            //         variable_to_be_assign_type,
            //     )
            //     // assignment.push_str(&format!(
            //     //     "    set stack %{variable_to_be_assign} - {}\n",
            //     //     array_index + 1
            //     // ));
            // }
            t => panic!(
                "Syntax Error: expected {{}} or = , found {t:?} at {}:{} ",
                peeked_token.line, peeked_token.column
            ),
        }
    }

    fn parse_array_expr(
        &mut self,
        tokens: &mut TokenIter,
        variables: &mut HashMap<String, Variable>,
        array_type: TypeDescription,
    ) -> Expression {
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
                    elements.push(self.parse_expression(
                        tokens,
                        variables,
                        array_type.strip_modifiers(),
                    ));

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
                // for element in elements.iter_mut().rev() {
                //     match element {
                //         ExpressionReturn {
                //             byte_code: _,
                //             expression_type:
                //                 TypeDescription {
                //                     modifier: ModifierType::None,
                //                     _type,
                //                 },
                //         } => match _type {
                //             TypeDef::Number(_) => {
                //                 if array_type._type.clone().is_number() {
                //                     element.expression_type._type = array_type.clone()._type
                //                 }
                //             }
                //             TypeDef::Boolean => todo!(),
                //             TypeDef::Func(_) => todo!(),
                //         },
                //         _ => panic!("Proper error when a array element has a modifeir"),
                //     }

                //     parsed_array.push_str(&element.byte_code);
                // }
                // parsed_array.push_str(&format!("    i64 push {size}\n"));
                // parsed_array.push_str(&format!("    i64 push top\n"));
                tokens.next();
                Expression {
                    expr_type: array_type,
                    expression: ExpressionType::ArrayLit { elements: elements },
                }
            }
            _ => todo!("Proper Error for trying to assing a array expr to a none array type"),
        }
    }
}

pub fn parse(tokens: &mut TokenIter) -> Program {
    let mut parser = Parser::new();
    let mut bin = String::new();
    let mut program = Program {
        statements: Vec::new(),
    };

    let mut peeked_token = tokens.peek().unwrap();
    while !matches!(peeked_token.token_type, Eof) {
        match &peeked_token.token_type {
            Let => {
                let mut variables = HashMap::new();
                let statement = parser.parse_declaration(tokens, &mut variables, false);
                program.statements.push(statement);
            }
            NewLine => {
                tokens.next().unwrap();
            }
            t => panic!(
                "Syntax Error: unexpected token {t:?} found in file base at {}:{}",
                peeked_token.line, peeked_token.column
            ),
        }
        peeked_token = tokens.peek().unwrap()
    }

    program
}
