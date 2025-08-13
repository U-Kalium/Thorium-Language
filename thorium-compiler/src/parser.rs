use std::collections::HashMap;
use std::{collections::HashSet, iter::Peekable};

use crate::tokenizer::Token;
use crate::tokenizer::TokenType::*;



struct FunctionType {
    parems: HashMap<String, String>,
    returns: Vec<String> 
}
impl FunctionType {
    fn new() -> Self {
        FunctionType {
            parems: HashMap::new(),
            returns: Vec::new()
        }
    }
}

struct VariableProperties {
    variable_type: String,
    is_mutable: bool
}

struct Parser {
    functions: HashMap<String, FunctionType>,
    types: HashSet<String>,
}

impl Parser {
    fn new() -> Self {
        Parser {
            functions: HashMap::new(),
            types: HashSet::new()
        }
    }

    fn parse_func<'a, I: Iterator<Item = &'a Token>>(&mut self, tokens: &mut Peekable<I>) -> String {
        fn parse_func_args<'a, I: Iterator<Item = &'a Token>>(tokens: &mut Peekable<I>) -> String {
            let func_args = String::new();
            let token = tokens.next().unwrap();
            match &token.token_type {
                CloseBracket => {}
                Ident(ident) => todo!("add function paremeters"),
                t => panic!("Syntax Error: expected ) or ident for function args, found {t:?} at {}:{} ", token.line, token.column)

            }
            func_args
        }
        let mut func_name = String::new();
        let mut func_type = FunctionType::new();

        let mut func = String::new();
        let mut token = tokens.next().unwrap();
        match &token.token_type {
            Fn => {
                func.push_str("func ");
            }
            t => panic!("Syntax Error: expected `fn` keyword, found {t:?} at {}:{}", token.line, token.column)
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
            t => panic!("Syntax Error: expected ident or (), found {t:?} at {}:{} ", token.line, token.column)
        }
        // func arguments
        token = tokens.next().unwrap();
        match &token.token_type {
            OpenBracket => {
                func.push_str(&parse_func_args(tokens))
            }
            OpenAngleBracket =>  {
                todo!("Method signature")
            }
            t => panic!("Syntax Error: expected () or <>, found {t:?} at {}:{} ", token.line, token.column)
        }
        // func returns
        func_type.returns.push(self.parse_type(tokens));
        func.push_str(":\n");

        // func body
        func.push_str(&self.parse_scope(tokens, func_type.returns.clone()));
        func.push_str("endfunc");
        
        self.functions.insert(func_name, func_type);

        func
    }

    fn parse_scope<'a, I: Iterator<Item = &'a Token>>(&mut self, tokens: &mut Peekable<I>, return_type: Vec<String>) -> String  {
        let mut variables = HashMap::new();
        let mut scope = String::new();
        let mut token = tokens.next().unwrap();
        match &token.token_type {
            OpenCurlyBracket => {}
            t => panic!("Syntax Error: expected {{, found {t:?} at {}{} ", token.line, token.column)
        }
        token = tokens.next().unwrap();
        while token.token_type != CloseCurlyBracket{
            scope.push_str(&self.parse_statement(tokens, &mut variables, return_type.clone()));
            token = tokens.peek().unwrap();
        }
        tokens.next();
        scope
    }

    fn parse_type<'a, I: Iterator<Item = &'a Token>>(&mut self, tokens: &mut Peekable<I>) -> String {
        let parsed_type ;
        let token = tokens.next().unwrap();
        match &token.token_type {
            I64 => parsed_type = "i64".to_string(),
            I32 => parsed_type = "i32".to_string(),
            I16 => parsed_type = "i16".to_string(),
            I8 => parsed_type = "i8".to_string(),
            F64 => parsed_type = "f64".to_string(),
            F32 => parsed_type = "f32".to_string(),
            Ident(_ident) => todo!("Custom types parsing"),
            t => panic!("Syntax Error: expected type, found {t:?} at {}:{} ", token.line, token.column)
        }

        parsed_type
    }

    fn parse_statement<'a, I: Iterator<Item = &'a Token>>(&mut self, tokens: &mut Peekable<I>, variables: &mut HashMap<String, VariableProperties>, scope_return_type: Vec<String>) -> String {
        let mut statement = String::new();
        let mut token = tokens.next().unwrap();
        match &token.token_type {
            Return => {
                token = tokens.next().unwrap();
                match &token.token_type {
                    NumberLit(number) => {
                        statement.push_str(&format!("    {} push {number}\n", scope_return_type[0]));
                    }
                    Ident(ident) => {
                        if !variables.contains_key(ident) {
                            panic!("Error: Variable {ident} does not exist yet was used at {}:{}", token.line, token.column)
                        }
                        statement.push_str(&format!("    get %{ident}\n"));
                    }
                    t => panic!("Syntax Error: expected i32 after return, found {t:?} at {}:{} ", token.line, token.column)
                }
            }
            Var => {
                token = tokens.next().unwrap();
                match &token.token_type {
                    Ident(ident) => {
                        statement.push_str(&format!("    declare %{ident}\n"));
                        // token = tokens.next().unwrap();
                        let variable_properties = VariableProperties {
                            variable_type: self.parse_type(tokens),
                            is_mutable: true
                        };
                        variables.insert(ident.clone(), variable_properties);
                        let peeked_token = tokens.peek().unwrap();
                        match peeked_token.token_type {
                            NewLine => {},
                            SemiColon => {}
                            _ => statement.push_str(&self.parse_assignment(tokens, variables, ident))
                        }
                        // match &token.token_type {
                        //     I32 => {
                        //         let variable_properties = VariableProperties {
                        //             variable_type: "i32".to_string(),
                        //             is_mutable: true
                        //         };
                        //         variables.insert(ident.clone(), variable_properties);
                        //         let peeked_token = tokens.peek().unwrap();
                        //         match peeked_token.token_type {
                        //             NewLine => {},
                        //             SemiColon => {}
                        //             _ => statement.push_str(&self.parse_assignment(tokens, variables, ident))
                        //         }
                                
                        //     }
                        //     Ident(ident) => todo!("var decleration with custom types"),
                        //     t => panic!("Syntax Error: expected type, found {t:?} at {}:{} ", token.line, token.column)

                        // }
                    }
                    t => panic!("Syntax Error: expected identifier, found {t:?} at {}:{} ", token.line, token.column)
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
                            is_mutable: false
                        };
                        variables.insert(ident.clone(), variable_properties);
                        let peeked_token = tokens.peek().unwrap();
                        match peeked_token.token_type {
                            NewLine => {},
                            SemiColon => {}
                            _ => statement.push_str(&self.parse_assignment(tokens, variables, ident))
                        }
                        // match &token.token_type {
                        //     I32 => {
                        //         let variable_properties = VariableProperties {
                        //             variable_type: "i32".to_string(),
                        //             is_mutable: false
                        //         };
                        //         variables.insert(ident.clone(), variable_properties);
                        //         let peeked_token = tokens.peek().unwrap();
                        //         match peeked_token.token_type {
                        //             NewLine => {},
                        //             SemiColon => {}
                        //             _ => statement.push_str(&self.parse_assignment(tokens, variables, ident))
                        //         }
                                
                        //     }
                        //     Ident(ident) => todo!("let decleration with custom types"),
                        //     t => panic!("Syntax Error: expected type, found {t:?} at {}:{} ", token.line, token.column)

                        // }
                    }
                    t => panic!("Syntax Error: expected identifier, found {t:?} at {}:{} ", token.line, token.column)
                }

            }
            Ident(ident) => {
                let variable_properties = variables.get(ident).unwrap();
                if !variable_properties.is_mutable {
                    panic!("Error: tried mutating variable {ident} but it is immutable, found at {}:{}", token.line, token.column)
                }
                statement.push_str(&self.parse_assignment(tokens, variables, ident));
            }

            t => panic!("Syntax Error: expected statement, found {t:?} at {}:{} ", token.line, token.column)
        }
        token = tokens.next().unwrap();
        match &token.token_type {
            SemiColon => {}
            NewLine => {}
            t => panic!("Syntax Error: expected newline or semicolon, found {t:?} at {}:{} ", token.line, token.column)
        }
        statement
    }

    fn parse_assignment<'a, I: Iterator<Item = &'a Token>>(&mut self, tokens: &mut Peekable<I>, variables: &mut HashMap<String, VariableProperties>, variable: &String) -> String {
        let mut assignment = String::new();
        let mut peeked_token = tokens.peek().unwrap();
        match &peeked_token.token_type {
            OpenCurlyBracket => {
                let varibale_type = variables.get(variable).unwrap().variable_type.clone();
                assignment.push_str(&self.parse_scope(tokens, vec![varibale_type]))
            },
            Equal => {
                tokens.next().unwrap();
                let mut token = tokens.next().unwrap();
                let variable_type = variables.get(variable).unwrap().variable_type.clone();
                match &token.token_type {
                    NumberLit(num) => {
                        assignment.push_str(&format!("    {variable_type} push {num}\n"))
                    }
                    Ident(ident) => {
                        if let Some(variable_properties) = variables.get(ident) {
                            let variable_type = variables.get(variable).unwrap().variable_type.clone();
                            if variable_type == variable_properties.variable_type {
                                assignment.push_str(&format!("    get %{ident}\n"));
                            }
                        } else {
                            panic!("Error: tried assigning an unkown identifier {ident} to variable {variable}");

                        }
                    }
                    t => panic!("Syntax Error: expected int or variable, found {t:?} at {}:{} ", token.line, token.column)
                }   
            }
            t => panic!("Syntax Error: expected {{}} or = , found {t:?} at {}:{} ", peeked_token.line, peeked_token.column)
        }
        assignment.push_str(&format!("    set %{variable}\n"));
        assignment
    }
}

pub fn parse<'a, I: Iterator<Item = &'a Token>>(tokens: &mut Peekable<I>) -> String {
    let mut parser = Parser::new(); 
    let mut bin = String::new();
    bin.push_str
("
func $start \"_start\" :
    call $main
    return

");
    let peeked_token = tokens.peek().unwrap();
    match &peeked_token.token_type {
        Fn => bin.push_str(parser.parse_func(tokens).as_str()),
        t => panic!("Syntax Error: unexpected token {t:?} found in file base at {}{}", peeked_token.line, peeked_token.column)
    }

    bin
}
