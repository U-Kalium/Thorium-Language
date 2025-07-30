use crate::{parser::{NodeExpr, NodeFunc, NodeProgram, NodeScope, NodeStatement, NodeType}, tokenizer::Token};

const WASM_START_FUNC: &str = 
"
    (func (export \"_start\") (result i32)
        call $main
    )

";

pub fn gen_wasm(syntax_tree: NodeProgram) -> String {
    let mut wasm = String::new();
    wasm.push_str("(module");
    wasm.push_str(WASM_START_FUNC);
    for func in syntax_tree.functions {
        wasm.push_str(&func.gen_wasm());
    }

    wasm.push_str(")");
    wasm
}

impl NodeFunc {
    fn gen_wasm(&self) -> String {
        let mut wasm = String::new();
        let mut func_sig = String::new();
        let func_body = self.body.gen_wasm();
        let mut func_params = String::new();
        let mut func_results = String::new();
        let func_ident = self.ident.clone();

        if self.func_return.is_some() {
            let return_type = match self.func_return.as_ref().unwrap() {
                NodeType::I32 => "i32"
            };
            func_results.push_str(format!("(result {return_type})").as_str());
        }

        func_sig.push_str(
            format!(
"
    (func ${func_ident} {func_results}
"
            ).as_str()
        );

        wasm.push_str(&func_sig);
        wasm.push_str(&func_body);

        wasm
    }
}

impl NodeScope {
    fn gen_wasm(&self) -> String {
        let mut wasm = String::new();
        let mut variable_decleration = String::new();
        let mut statements = String::new();

        for statement in &self.statements {
            statement.gen_wasm(&mut statements,&mut variable_decleration);
            // wasm.push_str(format!("        {statement_string}").as_str());
        }
        wasm.push_str(&variable_decleration);
        wasm.push_str(&statements);
        wasm.push_str("    )\n");
        wasm
    }
}

impl NodeStatement {
    fn gen_wasm(&self, statements: &mut String, variable_decleration: &mut String){


        match self {
            NodeStatement::Return(expr) => {
                statements.push_str(format!("        {}\n", &expr.gen_wasm()).as_str());
                statements.push_str("        return\n");
            }
            NodeStatement::VariableDecleration{
                ident,
                var_type,
                expression,
            } => {
                variable_decleration.push_str(format!("        (local ${ident} i32)\n").as_str());
                statements.push_str(format!("        (local.set ${ident} ({}))\n", &expression.gen_wasm()).as_str());
            }
            NodeStatement::VariableAssignment{
                ident,
                expression
            } => {
                statements.push_str(format!("        (local.set ${ident} ({}))\n", expression.gen_wasm()).as_str());
            }
        }
    }

        // (local $var i32) ;; create a local variable named $var
    // (local.set $var (i32.const 10)) ;; set $var to 10
}

impl NodeExpr {
    fn gen_wasm(&self) -> String {
        let mut wasm = String::new();
        let value = match self {
            NodeExpr::I32(value) => format!("i32.const {}", value.to_string()),
            NodeExpr::Variable(ident) => format!("local.get ${}", ident)
        };
        wasm.push_str(format!("{value}").as_str());
        wasm
    }
}