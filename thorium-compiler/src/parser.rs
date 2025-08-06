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

struct Parser {
    functions: HashMap<String, FunctionType>,
    types: HashSet<String>
}

impl Parser {
    fn new() -> Self {
        Parser {
            functions: HashMap::new(),
            types: HashSet::new()
        }
    }

    fn parse_func_sig<'a, I: Iterator<Item = &'a Token>>(&mut self, tokens: &mut Peekable<I>) -> String {
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

        let mut func_sig = String::new();
        let mut token = tokens.next().unwrap();
        match &token.token_type {
            Fn => {
                func_sig.push_str("func ");
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
                func_sig.push_str(format!("${ident}").as_str());
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
                func_sig.push_str(&parse_func_args(tokens))
            }
            OpenAngleBracket =>  {
                todo!("Method signature")
            }
            t => panic!("Syntax Error: expected () or <>, found {t:?} at {}:{} ", token.line, token.column)
        }
        // func returns
        func_type.returns.push(self.parse_type(tokens));
        func_sig.push_str(":\n");

        // func body
        func_sig.push_str(&self.parse_scope(tokens));
        
        self.functions.insert(func_name, func_type);

        func_sig
    }

    fn parse_scope<'a, I: Iterator<Item = &'a Token>>(&mut self, tokens: &mut Peekable<I>) -> String  {
        let mut scope = String::new();
        let mut token = tokens.next().unwrap();
        match &token.token_type {
            OpenCurlyBracket => {}
            t => panic!("Syntax Error: expected {{, found {t:?} at {}{} ", token.line, token.column)
        }
        token = tokens.next().unwrap();
        while token.token_type != CloseCurlyBracket{
            scope.push_str(&self.parse_statement(tokens));
            token = tokens.next().unwrap();
        }
        scope
    }

    fn parse_type<'a, I: Iterator<Item = &'a Token>>(&mut self, tokens: &mut Peekable<I>) -> String {
        let parsed_type ;
        let token = tokens.next().unwrap();
        match &token.token_type {
            I32 => parsed_type = "i32".to_string(),
            Ident(_ident) => todo!("Custom types parsing"),
            t => panic!("Syntax Error: expected type, found {t:?} at {}{} ", token.line, token.column)
        }

        parsed_type
    }

    fn parse_statement<'a, I: Iterator<Item = &'a Token>>(&mut self, tokens: &mut Peekable<I>) -> String {
        let mut statement = String::new();
        let mut token = tokens.next().unwrap();
        match &token.token_type {
            Return => {
                token = tokens.next().unwrap();
                match &token.token_type {
                    IntLit(number) => {
                        statement.push_str(&format!("   i32.push {number}"));
                    }
                    t => panic!("Syntax Error: expected i32, found {t:?} at {}:{} ", token.line, token.column)
                }
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
}

pub fn parse<'a, I: Iterator<Item = &'a Token>>(tokens: &mut Peekable<I>) -> String {
    let mut parser = Parser::new(); 
    let mut bin = String::new();
    bin.push_str
("
func $start export \"_start\" :
    call $main
    return

");
    let peeked_token = tokens.peek().unwrap();
    match &peeked_token.token_type {
        Fn => bin.push_str(parser.parse_func_sig(tokens).as_str()),
        t => panic!("Syntax Error: unexpected token {t:?} found in file base at {}{}", peeked_token.line, peeked_token.column)
    }

    bin
}
