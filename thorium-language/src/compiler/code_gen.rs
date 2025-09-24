use std::fmt::format;

use hashbrown::HashMap;

use crate::compiler::syntax_tree::*;


impl FunctionDef {
    fn byte_code(&self, variables: &HashMap<String, Variable>, expression: &Expression) -> String {
        expression.byte_code(variables)

    }
}

impl Variable {
    fn mangle(&self) -> String {
        mangle_with_location(self.ident.to_owned(), self.location.to_owned())
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
                byte_code.push_str(&format!("    get %{}\n", variable.mangle()));
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
                byte_code.push_str(&format!("    cpy %{} - top top\n", variable.mangle()));
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
                            expression: _,
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

fn mangle_with_location(ident: String, location: String) -> String {
    format!("{ident}{location}")
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
                    location,
                } => {
                    byte_code.push_str(&format!("func ${}:\n", ident));
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
                    location,
                } => {
                    byte_code.push_str(&format!("    {} declare %{}\n", _type.to_string(), mangle_with_location(ident.to_owned(), location.to_owned())));
                    if let Some(expr) = assignment {
                        byte_code.push_str(&expr.byte_code(variables));
                        byte_code.push_str(&format!("    set %{}\n", mangle_with_location(ident.to_owned(), location.to_owned())));
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
                    location,
                } => {
                    byte_code.push_str(&format!("    i64 declare %{}\n", mangle_with_location(ident.to_owned(), location.to_owned())));
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
                    byte_code.push_str(&format!("    set %{}\n", mangle_with_location(ident.to_owned(), location.to_owned())));
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
                byte_code.push_str(&expression.byte_code(variables));
                if let Some(array_index) = array_index {
                    byte_code.push_str(&format!(
                        "    set stack %{} - {}\n",
                        mangle_with_location(variable.ident.to_owned(), variable.location.to_owned()),
                        array_index + 1
                    ));
                } else {
                    byte_code.push_str(&format!("    set %{}\n", variable.mangle()));
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
