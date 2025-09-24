use hashbrown::HashMap;

use crate::compiler::syntax_tree::*;

struct Gener {
    variables: HashMap<String, Variable>,
}
impl FunctionDef {
    fn byte_code(&self, variables: &HashMap<String, Variable>, expression: &Expression) -> String {
        expression.byte_code(variables)

        // if *self.returns == expression.expr_type {
        // } else {
        //     dbg!(&self.returns);
        //     dbg!(&expression.expr_type);
        //     panic!("Function return type and expression type do not match")
        // }
    }
}

impl Program {
    pub fn byte_code(&self) -> String {
        // dbg!(self);
        let mut byte_code = String::new();
        let variables = HashMap::new();
        byte_code.push_str(
            "
func $start \"_start\" :
    call $main
    return
endfunc

",
        );
        for statement in &self.statements {
            byte_code.push_str(&statement.byte_code(&variables));
        }

        byte_code
    }
}

impl Expression {
    fn byte_code(&self, variables: &HashMap<String, Variable>) -> String {
        let mut byte_code = String::new();
        match &self.expression {
            ExpressionType::Variable(variable) => {
                byte_code.push_str(&format!("    get %{}\n", variable.ident));
            }
            ExpressionType::Value { value } => match &self.expr_type {
                TypeDescription {
                    modifier: ModifierType::None,
                    _type,
                } => {
                    byte_code.push_str(&format!(
                        "    {} push {}\n",
                        self.expr_type._type.to_string(),
                        value.to_byte_code()
                    ));
                }

                _ => todo!("proper bytecode for expression type {:?}", self),
            },
            ExpressionType::ConditionalOp {
                condition,
                lhs,
                rhs,
            } => {
                byte_code.push_str(&lhs.byte_code(variables));
                byte_code.push_str(&rhs.byte_code(variables));
                byte_code.push_str(&format!(
                    "    {} {}\n",
                    self.expr_type._type.to_string(),
                    condition.to_byte_code()
                ));
            }
            ExpressionType::IndexedArray { variable, index } => {
                byte_code.push_str(&index.byte_code(variables));
                byte_code.push_str("    i64 push 1\n");
                byte_code.push_str("    i64 add\n");
                byte_code.push_str(&format!("    cpy %{} - top top\n", variable.ident));
            }
            ExpressionType::ArithmaticOp { op, lhs, rhs } => {
                byte_code.push_str(&lhs.byte_code(variables));
                byte_code.push_str(&rhs.byte_code(variables));
                byte_code.push_str(&format!(
                    "    {} {}\n",
                    self.expr_type._type.to_string(),
                    op.to_byte_code()
                ));
            }
            ExpressionType::Scope { statements } => {
                for statement in statements {
                    byte_code.push_str(&statement.byte_code(variables));
                }
            }
            ExpressionType::If {
                location,
                condition,
                expression,
                else_branch,
            } => {
                parse_if(
                    condition,
                    else_branch,
                    expression,
                    None,
                    variables,
                    &mut byte_code,
                    location,
                );
            }
            ExpressionType::WhileLoop {
                location,
                condition,
                expression,
            } => {
                let begin_loop_label = format!("beginLoop{location}");
                let end_loop_label = format!("endLoop{location}");
                byte_code.push_str(&format!("@{begin_loop_label}\n"));
                byte_code.push_str(&condition.byte_code(variables));
                byte_code.push_str(&format!(
                    "    i8 jpz \"{end_loop_label}\""
                ));
                byte_code.push_str(&expression.byte_code(variables));
                byte_code.push_str(&format!("    i8 push 1\n"));
                byte_code.push_str(&format!("    i8 jmp \"{begin_loop_label}\"\n"));
                byte_code.push_str(&format!("@{end_loop_label}\n"));
            }
            ExpressionType::ForLoop {
                location,
                first_statement,
                condition,
                last_statement,
                expression,
            } => {
                let begin_loop_label = format!("beginLoop{location}");
                let end_loop_label = format!("endLoop{location}");
                byte_code.push_str(&first_statement.byte_code(variables));
                byte_code.push_str(&format!("@{begin_loop_label}\n"));
                byte_code.push_str(&condition.byte_code(variables));
                byte_code.push_str(&format!(
                    "    i8 jpz \"{end_loop_label}\""
                ));
                byte_code.push_str(&expression.byte_code(variables));
                byte_code.push_str(&last_statement.byte_code(variables));
                byte_code.push_str(&format!("    i8 push 1\n"));
                byte_code.push_str(&format!("    i8 jmp \"{begin_loop_label}\"\n"));
                byte_code.push_str(&format!("@{end_loop_label}\n"));

            },
            ExpressionType::ArrayLit { elements } => {
                for element in elements.clone().iter_mut().rev() {
                    match element {
                        Expression {
                            expr_type:
                                TypeDescription {
                                    modifier: ModifierType::None,
                                    _type,
                                },
                            expression,
                        } => match _type {
                            TypeDef::Number(_) => {
                                // if array_type._type.clone().is_number() {
                                //     element.expr_type._type = self.expression.
                                // }
                                if self.expr_type._type.clone().is_number() {
                                    element.expr_type._type = self.expr_type._type.clone()
                                }
                            }
                            TypeDef::Boolean => todo!(),
                            TypeDef::Func(_) => todo!(),
                            TypeDef::Void => todo!(),
                        },
                        _ => panic!("Proper error when a array element has a modifeir"),
                        // ExpressionReturn {
                        //     byte_code: _,
                        //     expression_type:
                        //         TypeDescription {
                        //             modifier: ModifierType::None,
                        //             _type,
                        //         },
                        // } => match _type {
                        //     TypeDef::Number(_) => {
                        //         if array_type._type.clone().is_number() {
                        //             element.expression_type._type = array_type.clone()._type
                        //         }
                        //     }
                        //     TypeDef::Boolean => todo!(),
                        //     TypeDef::Func(_) => todo!(),
                        // },
                        // _ => panic!("Proper error when a array element has a modifeir"),
                    }

                    byte_code.push_str(&element.byte_code(variables));
                }
                // byte_code.push_str(&format!("    i64 push {}\n", elements.len()));
                // byte_code.push_str(&format!("    i64 push top\n"));
            }
        }
        byte_code
    }
}

impl Statement {
    fn byte_code(&self, variables: &HashMap<String, Variable>) -> String {
        let mut byte_code = String::new();

        match self {
            Statement::VariableDecleration {
                variable,
                assignment,
            } => match variable {
                Variable {
                    variable_type:
                        TypeDescription {
                            modifier: ModifierType::None,
                            _type: TypeDef::Func(function),
                        },
                    is_mutable: false,
                    ident,
                } => {
                    byte_code.push_str(&format!("func ${ident}:\n"));
                    byte_code
                        .push_str(&function.byte_code(variables, &assignment.clone().unwrap()));
                    byte_code.push_str("endfunc\n");
                }
                Variable {
                    variable_type:
                        TypeDescription {
                            modifier: ModifierType::None,
                            _type,
                        },
                    is_mutable: _,
                    ident,
                } => {
                    byte_code.push_str(&format!("    {} declare %{}\n", _type.to_string(), ident));
                    if let Some(expr) = assignment {
                        byte_code.push_str(&expr.byte_code(variables));
                        byte_code.push_str(&format!("    set %{ident}\n"));
                    }
                }
                Variable {
                    variable_type:
                        TypeDescription {
                            modifier: ModifierType::FixedArray(size),
                            _type,
                        },
                    is_mutable: _,
                    ident,
                } => {
                    byte_code.push_str(&format!("    i64 declare %{}\n", ident));
                    let elem_type_string = _type.to_string();
                    if let Some(expr) = assignment {
                        byte_code.push_str(&expr.byte_code(variables))
                    } else {
                        for _ in 0..*size {
                            byte_code.push_str(&format!("    {elem_type_string} push 0\n"));
                        }
                    }
                    byte_code.push_str(&format!("    i64 push {size}\n"));
                    byte_code.push_str(&format!("    i64 push top\n"));
                    byte_code.push_str(&format!("    set %{ident}\n"));
                }
                var => todo!("{:?}", var),
            },
            Statement::Return { expression } => {
                byte_code.push_str(&expression.byte_code(variables));
            }
            Statement::Finish { expression } => {
                byte_code.push_str(&expression.byte_code(variables));
                byte_code.push_str("    return\n");
            }
            Statement::Expression(expression) => {
                if let TypeDef::Void = expression.expr_type._type {
                } else {
                    panic!("Found expression without void type when expression with void type ")
                }
                byte_code.push_str(&expression.byte_code(variables));
            }
            Statement::Assignment {
                variable,
                expression,
                array_index,
            } => {
                // match &variable.variable_type {
                //     TypeDescription {
                //         modifier
                //     }
                // }
                byte_code.push_str(&expression.byte_code(variables));
                if let Some(array_index) = array_index {
                    byte_code.push_str(&format!(
                        "    set stack %{} - {}\n",
                        variable.ident,
                        array_index + 1
                    ));
                } else {
                    byte_code.push_str(&format!("    set %{}\n", variable.ident));
                }
            }
            Statement::Empty => {}
        }

        byte_code
    }
}
fn parse_else(
    else_expr: &Box<Expression>,
    merge_label: String,
    variables: &HashMap<String, Variable>,
    expr_string: &mut String,
) {
    if let ExpressionType::If {
        location,
        condition,
        expression,
        else_branch,
    } = &else_expr.expression
    {
        parse_if(
            &condition,
            &else_branch,
            &expression,
            Some(merge_label),
            variables,
            expr_string,
            &location,
        );
    } else {
        expr_string.push_str(&else_expr.byte_code(variables));
        expr_string.push_str(&format!("    i8 push 1\n"));
        expr_string.push_str(&format!("    i8 jmp \"{}\"\n", &merge_label.clone()));
    }
}
fn parse_if(
    condition: &Box<Expression>,
    else_branch: &Option<Box<Expression>>,
    expression: &Box<Expression>,
    maybe_merge_label: Option<String>,
    variables: &HashMap<String, Variable>,
    expr_string: &mut String,
    location: &String,
) {
    expr_string.push_str(&condition.byte_code(variables));
    let mut final_if = false;

    let scope = expression.byte_code(variables);
    let merge_label;
    if let Some(label) = &maybe_merge_label {
        merge_label = label.clone()
    } else {
        final_if = true;
        merge_label = format!("merge{}", location)
    }

    let if_label = format!("if{}", location);
    if let Some(else_expr) = else_branch {
        expr_string.push_str(&format!(
            "    {} jmp \"{if_label}\"\n",
            condition.expr_type.strip_modifiers()._type.to_string()
        ));
        parse_else(else_expr, merge_label.clone(), variables, expr_string);
    } else {
        expr_string.push_str(&format!(
            "    {} jpz \"{merge_label}\"\n",
            condition.expr_type.strip_modifiers()._type.to_string()
        ));
    }
    expr_string.push_str(&format!("@{if_label}\n"));
    expr_string.push_str(&scope);
    expr_string.push_str(&format!("    i8 push 1\n"));
    expr_string.push_str(&format!("    i8 jmp \"{}\"\n", merge_label.clone()));
    if final_if {
        expr_string.push_str(&format!("@{}\n", merge_label));
    }
}

// impl Gener {
//     fn gen_decleration(&self, statement: Statement) -> String {
//         let mut declaration = String::new();
//         if let Statement::VariableDecleration {
//             variable,
//             assignment,
//         } = statement
//         {
//             match variable.variable_type {
//                 TypeDescription {
//                     modifier: ModifierType::None,
//                     _type: TypeDef::Func(func_def),
//                 } => {
//                     declaration.push_str(format!("func ${ident}:\n").as_str());
//                     declaration.push_str(&self.gen_scope(tokens, *func_def.returns.clone()));
//                     declaration.push_str("    return\n");
//                     declaration.push_str("endfunc\n");

//                     self.functions.insert(ident.to_string(), func_def);
//                 }
//                 TypeDescription {
//                     modifier: ModifierType::FixedArray(size),
//                     ref _type,
//                 } => {
//                     declaration.push_str(&format!("   i64 declare %{ident}\n"));
//                     variables.insert(ident.clone(), variable_properties.clone());
//                     if let Some(expression) = assignment {}
//                     let peeked_token = tokens.peek().unwrap();
//                     match peeked_token.token_type {
//                         NewLine | SemiColon => {
//                             let elem_type_string = _type.to_string();
//                             for i in 0..size {
//                                 declaration.push_str(&format!("    {elem_type_string} push 0\n"));
//                             }
//                             declaration.push_str(&format!("    i64 push {size}\n"));
//                             declaration.push_str(&format!("    i64 push top\n"));
//                             declaration.push_str(&format!("    set %{ident}\n"));
//                         }
//                         _ => {
//                             declaration.push_str(&self.parse_assignment(tokens, variables, &ident))
//                         }
//                     }
//                 }
//                 _ => {
//                     declaration.push_str(&format!(
//                         "   {} declare %{ident}\n",
//                         variable_properties
//                             .variable_type
//                             .strip_modifiers()
//                             ._type
//                             .to_string()
//                     ));
//                     variables.insert(ident.clone(), variable_properties);
//                     let peeked_token = tokens.peek().unwrap();
//                     match peeked_token.token_type {
//                         NewLine | SemiColon => {}
//                         _ => {
//                             declaration.push_str(&self.parse_assignment(tokens, variables, &ident))
//                         }
//                     }
//                 }
//             }
//         } else {
//             panic!("Semantic Error: expected a variable deceration")
//         }
//         declaration
//     }
//     fn gen_expr(&self, expr: Expression) -> String {
//         fn parse_arith_tree(
//             expr: &Expression,
//             variables: &HashMap<String, VariableProperties>,
//             instructions: &mut Vec<String>,
//         ) {
//             if let Expression::ArithmaticOp {
//                 expr_type,
//                 op,
//                 lhs,
//                 rhs,
//             } = expr
//             {
//                 let int_type = expr_type._type.to_string();
//                 match op {
//                     ArithmaticOp::Addition => instructions.push(format!("    {int_type} add\n")),
//                     ArithmaticOp::Subtraction => instructions.push(format!("    {int_type} sub\n")),
//                     ArithmaticOp::Multiplication => {
//                         instructions.push(format!("    {int_type} mul\n"))
//                     }
//                     ArithmaticOp::Division => instructions.push(format!("    {int_type} div\n")),
//                 };

//                 match &**rhs {
//                     Expression::Variable(ident) => {
//                         instructions.push(format!("    get %{}\n", ident))
//                     }
//                     Expression::Value { expr_type, value } => match value {
//                         Value::Custom(_) => todo!("custom types not implemented"),
//                         Value::Number(number) => instructions.push(format!(
//                             "    {} push {number}\n",
//                             expr_type._type.to_string()
//                         )),
//                     },
//                     Expression::IndexedArray {
//                         expr_type: _,
//                         variable,
//                         index,
//                     } => {
//                         instructions.push(parse_indexed_array(variables, index, variable));
//                     }
//                     Expression::ArithmaticOp {
//                         expr_type: _,
//                         op: _,
//                         lhs: _,
//                         rhs: _,
//                     } => {
//                         parse_arith_tree(rhs, variables, instructions);
//                     }
//                     _ => todo!(
//                         "Proper error for a arithmatic tree that contains an invalid expression"
//                     ),
//                 }
//                 match &**lhs {
//                     Expression::Variable(ident) => {
//                         instructions.push(format!("    get %{}\n", ident))
//                     }
//                     Expression::Value { expr_type, value } => match value {
//                         Value::Custom(_) => todo!("custom types not implemented"),
//                         Value::Number(number) => instructions.push(format!(
//                             "    {} push {number}\n",
//                             expr_type._type.to_string()
//                         )),
//                     },
//                     Expression::IndexedArray {
//                         expr_type: _,
//                         variable,
//                         index,
//                     } => {
//                         instructions.push(parse_indexed_array(variables, index, variable));
//                     }
//                     Expression::ArithmaticOp {
//                         expr_type: _,
//                         op: _,
//                         lhs: _,
//                         rhs: _,
//                     } => {
//                         parse_arith_tree(lhs, variables, instructions);
//                     }
//                     _ => todo!(
//                         "Proper error for a arithmatic tree that contains an invalid expression"
//                     ),
//                 }
//             }
//         }
//         fn parse_indexed_array(
//             variables: &HashMap<String, VariableProperties>,
//             index: &Box<Expression>,
//             variable: &String,
//         ) -> String {
//             let mut expr_string = String::new();
//             match &**index {
//                 Expression::Variable(ident) => {
//                     let variable = variables.get(ident).unwrap();
//                     if (TypeDescription {
//                         modifier: ModifierType::None,
//                         _type: TypeDef::undefined_number(),
//                     }) != variable.variable_type
//                     {
//                         todo!(
//                             "Proper error for indexing an array witha  variable of incorrect type"
//                         )
//                     }
//                     expr_string.push_str(&format!("    get %{ident}\n"));
//                 }
//                 Expression::Value { expr_type, value } => match value {
//                     Value::Custom(_) => {
//                         todo!("Proper error for trying to index an array witha  non number value")
//                     }
//                     Value::Number(number) => {
//                         expr_string.push_str(&format!("    i64 push {number}\n"))
//                     }
//                 },
//                 Expression::IndexedArray {
//                     expr_type,
//                     variable,
//                     index,
//                 } => expr_string.push_str(&parse_expr_tree(&index, variables)),
//                 _ => todo!(
//                     "Proper Error when trying to index an array with an incorrect expression type"
//                 ),
//             }
//             expr_string.push_str(&format!("    i64 push 1\n"));
//             expr_string.push_str(&format!("    i64 add\n"));
//             expr_string.push_str(&format!("    cpy %{variable} - top top\n"));
//             expr_string
//         }
//         fn parse_expr_tree(
//             expr: &Expression,
//             variables: &HashMap<String, VariableProperties>,
//         ) -> String {
//             let mut expr_string = String::new();
//             match expr {
//                 Expression::Variable(ident) => {
//                     expr_string.push_str(&format!("    get %{}\n", ident))
//                 }
//                 Expression::Value { expr_type, value } => match value {
//                     Value::Custom(_) => todo!("custom types not implemented"),
//                     Value::Number(number) => expr_string.push_str(&format!(
//                         "    {} push {number}\n",
//                         expr_type._type.to_string()
//                     )),
//                 },
//                 Expression::ConditionalOp {
//                     condition,
//                     lhs,
//                     rhs,
//                 } => {
//                     expr_string.push_str(&parse_expr_tree(lhs, variables));
//                     expr_string.push_str(&parse_expr_tree(rhs, variables));
//                     let mut operation_type = TypeDescription {
//                         modifier: ModifierType::None,
//                         _type: TypeDef::undefined_number(),
//                     };
//                     match &**lhs {
//                         Expression::Value {
//                             expr_type,
//                             value: _,
//                         } => operation_type = expr_type.clone(),
//                         Expression::Variable(ident) => {
//                             operation_type = variables.get(ident).unwrap().variable_type.clone()
//                         }
//                         _ => {}
//                     }
//                     match &**rhs {
//                         Expression::Value {
//                             expr_type,
//                             value: _,
//                         } => operation_type = expr_type.clone(),
//                         Expression::Variable(ident) => {
//                             operation_type = variables.get(ident).unwrap().variable_type.clone()
//                         }
//                         _ => {}
//                     }
//                     match condition {
//                         Condition::Equal => {
//                             if let ModifierType::None = operation_type.modifier {
//                                 expr_string.push_str(&format!(
//                                     "    {} eq\n",
//                                     operation_type._type.to_string()
//                                 ))
//                             }
//                         }
//                         Condition::LessEqual => {
//                             if let ModifierType::None = operation_type.modifier {
//                                 expr_string.push_str(&format!(
//                                     "    {} lte\n",
//                                     operation_type._type.to_string()
//                                 ))
//                             }
//                         }
//                         Condition::LessThan => {
//                             if let ModifierType::None = operation_type.modifier {
//                                 expr_string.push_str(&format!(
//                                     "    {} lt\n",
//                                     operation_type._type.to_string()
//                                 ))
//                             }
//                         }
//                         Condition::GreatEqual => {
//                             if let ModifierType::None = operation_type.modifier {
//                                 expr_string.push_str(&format!(
//                                     "    {} gte\n",
//                                     operation_type._type.to_string()
//                                 ))
//                             }
//                         }
//                         Condition::GreaterThan => {
//                             if let ModifierType::None = operation_type.modifier {
//                                 expr_string.push_str(&format!(
//                                     "    {} gt\n",
//                                     operation_type._type.to_string()
//                                 ))
//                             }
//                         }
//                         Condition::NotEqual => todo!(),
//                     }
//                     expr_string.push_str("    cast i8\n");
//                 }
//                 Expression::ArithmaticOp {
//                     expr_type,
//                     op,
//                     lhs,
//                     rhs,
//                 } => {
//                     let mut instructions: Vec<String> = Vec::new();
//                     parse_arith_tree(expr, variables, &mut instructions);
//                     instructions.reverse();
//                     instructions
//                         .iter()
//                         .for_each(|inst| expr_string.push_str(&inst));
//                 }
//                 Expression::IndexedArray {
//                     expr_type,
//                     variable,
//                     index,
//                 } => {
//                     expr_string.push_str(&parse_indexed_array(variables, index, variable));
//                 }
//             }
//             expr_string
//         }
//     }
// }
// fn parse_else(
//     parser: &mut Parser,
//     tokens: &mut TokenIter,
//     variables: &mut HashMap<String, VariableProperties>,
//     // token: &'a Token,
//     scope_return_type: TypeDescription,
//     statement: &mut String,
//     maybe_merge_label: Option<String>,
// ) {
//     tokens.next();
//     let peeked = tokens.peek().unwrap();
//     if let If = peeked.token_type {
//         let token = tokens.next().unwrap();
//         parse_if(
//             parser,
//             tokens,
//             variables,
//             token,
//             scope_return_type,
//             statement,
//             maybe_merge_label,
//         );
//     } else {
//         statement.push_str(&parser.parse_scope(tokens, scope_return_type, variables));
//         statement.push_str(&format!("    i8 push 1\n"));
//         statement.push_str(&format!(
//             "    i8 jmp \"{}\"\n",
//             &maybe_merge_label.clone().unwrap()
//         ));
//         // if let Else = tokens.peek().unwrap().token_type {

//         // } else {
//         //     statement.push_str(&format!("@{}\n", &maybe_merge_label.unwrap()));
//         // }
//     }
// }
// fn parse_if(
//     parser: &mut Parser,
//     tokens: &mut TokenIter,
//     variables: &mut HashMap<String, VariableProperties>,
//     token: Token,
//     scope_return_type: TypeDescription,
//     statement: &mut String,
//     maybe_merge_label: Option<String>,
// ) {
//     let expression = parser.parse_expression(tokens, variables, TypeDescription::bool());
//     if expression.expression_type._type != TypeDef::Boolean {
//         panic!(
//             "Error: expected bool type found {:?} at {}:{}",
//             expression.expression_type._type, token.line, token.column
//         )
//     }
//     statement.push_str(&expression.byte_code);
//     let mut final_if = false;

//     let scope = parser.parse_scope(tokens, scope_return_type.clone(), variables);
//     let merge_label;
//     if let Some(label) = maybe_merge_label {
//         merge_label = label
//     } else {
//         final_if = true;
//         merge_label = format!("mergeL{}C{}", token.line, token.column);
//     }

//     let if_label = format!("ifL{}C{}", token.line, token.column);
//     let else_token = tokens.peek().unwrap();
//     if let Else = else_token.token_type {
//         statement.push_str(&format!(
//             "    {} jmp \"{if_label}\"\n",
//             scope_return_type.strip_modifiers()._type.to_string()
//         ));
//         parse_else(
//             parser,
//             tokens,
//             variables,
//             scope_return_type,
//             statement,
//             Some(merge_label.clone()),
//         );
//     } else {
//         statement.push_str(&format!(
//             "    {} jpz \"{merge_label}\"\n",
//             expression
//                 .expression_type
//                 .strip_modifiers()
//                 ._type
//                 .to_string()
//         ));
//     }
//     statement.push_str(&format!("@{if_label}\n"));
//     statement.push_str(&scope);
//     statement.push_str(&format!("    i8 push 1\n"));
//     statement.push_str(&format!("    i8 jmp \"{}\"\n", merge_label.clone()));
//     if final_if {
//         statement.push_str(&format!("@{}\n", merge_label));
//     }
// }
// fn parse_loop(
//     parser: &mut Parser,
//     tokens: &mut TokenIter,
//     variables: &mut HashMap<String, VariableProperties>,
//     scope_return_type: TypeDescription,
//     statement: &mut String,
// ) {
//     fn parse_while_loop(
//         parser: &mut Parser,
//         tokens: &mut TokenIter,
//         variables: &mut HashMap<String, VariableProperties>,
//         scope_return_type: TypeDescription,
//         statement: &mut String,
//     ) {
//         let loop_token = tokens.peek().unwrap();
//         let begin_loop_label =
//             format!("beginLoopL{}C{}", loop_token.line, loop_token.column);
//         let end_loop_label = format!("endLoopL{}C{}", loop_token.line, loop_token.column);
//         statement.push_str(&format!("@{begin_loop_label}\n"));
//         let expression_return =
//             parser.parse_expression(tokens, variables, TypeDescription::bool());
//         statement.push_str(&expression_return.byte_code);
//         statement.push_str(&format!(
//             "    {} jpz \"{end_loop_label}\"\n",
//             expression_return
//                 .expression_type
//                 .strip_modifiers()
//                 ._type
//                 .to_string()
//         ));

//         let scope = parser.parse_scope(tokens, scope_return_type, variables);
//         statement.push_str(&scope);

//         statement.push_str(&format!("    i8 push 1\n"));
//         statement.push_str(&format!("    i8 jmp \"{begin_loop_label}\"\n"));
//         statement.push_str(&format!("@{end_loop_label}\n"));
//     }
//     fn parse_foreach_loop(
//         parser: &mut Parser,
//         tokens: &mut TokenIter,
//         variables: &mut HashMap<String, VariableProperties>,
//         scope_return_type: TypeDescription,
//         statement: &mut String,
//     ) {
//         todo!("for each loop")
//     }
//     fn parse_for_loop(
//         parser: &mut Parser,
//         tokens: &mut TokenIter,
//         variables: &mut HashMap<String, VariableProperties>,
//         scope_return_type: TypeDescription,
//         statement: &mut String,
//     ) {
//         let loop_token = tokens.peek().unwrap();
//         let begin_loop_label =
//             format!("beginLoopL{}C{}", loop_token.line, loop_token.column);
//         let end_loop_label = format!("endLoopL{}C{}", loop_token.line, loop_token.column);
//         let first_statement =
//             parser.parse_statement(tokens, variables, scope_return_type.clone());
//         let expression_return =
//             parser.parse_expression(tokens, variables, TypeDescription::bool());
//         tokens.next();
//         let second_statement =
//             parser.parse_statement(tokens, variables, scope_return_type.clone());
//         dbg!(&expression_return);
//         statement.push_str(&first_statement);
//         statement.push_str(&format!("@{begin_loop_label}\n"));
//         statement.push_str(&expression_return.byte_code);
//         statement.push_str(&format!(
//             "    {} jmp \"{end_loop_label}\"\n",
//             expression_return
//                 .expression_type
//                 .strip_modifiers()
//                 ._type
//                 .to_string()
//         ));

//         let scope = parser.parse_scope(tokens, scope_return_type, variables);
//         statement.push_str(&scope);

//         statement.push_str(&second_statement);

//         statement.push_str(&format!("    i8 push 1\n"));
//         statement.push_str(&format!("    i8 jmp \"{begin_loop_label}\"\n"));
//         statement.push_str(&format!("@{end_loop_label}\n"));
//     }
//     // checking what type of loop
//     let mut peeked = tokens.peek().unwrap();
//     match peeked.token_type {
//         Ident(ident) => {
//             // while/for loop
//             if variables.contains_key(&ident) {
//                 tokens.next().unwrap();
//                 peeked = tokens.peek().unwrap();
//                 tokens.back();
//                 match peeked.token_type {
//                     Equal => parse_for_loop(
//                         parser,
//                         tokens,
//                         variables,
//                         scope_return_type,
//                         statement,
//                     ),
//                     _ => parse_while_loop(
//                         parser,
//                         tokens,
//                         variables,
//                         scope_return_type,
//                         statement,
//                     ),
//                 }
//             } else {
//                 // for each loop
//                 parse_foreach_loop(parser, tokens, variables, scope_return_type, statement);
//             }
//         }
//         True => parse_while_loop(parser, tokens, variables, scope_return_type, statement),
//         False => parse_while_loop(parser, tokens, variables, scope_return_type, statement),
//         _ => parse_for_loop(parser, tokens, variables, scope_return_type, statement),
//     }
// }
