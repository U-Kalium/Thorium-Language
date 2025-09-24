use hashbrown::HashMap;

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement> 
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDef {
    pub parems: HashMap<String, Box<TypeDescription>>,
    pub returns: Box<TypeDescription>,
}
impl FunctionDef {
    pub fn new() -> Self {
        FunctionDef {
            parems: HashMap::new(),
            returns: Box::new(TypeDescription::bool()),
        }
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum ModifierType {
    FixedArray(usize),
    DynamicArray,
    None,
}
#[derive(Clone, Debug, PartialEq)]
pub struct  TypeDescription {
    pub modifier: ModifierType,
    pub _type: TypeDef,
}

impl TypeDescription {
    pub fn default_int() -> Self {
        TypeDescription {
            modifier: ModifierType::None,
            _type: TypeDef::default_int(),
        }
    }
    pub fn bool() -> Self {
        TypeDescription {
            modifier: ModifierType::None,
            _type: TypeDef::Boolean,
        }
    }
    pub fn strip_modifiers(&self) -> Self {
        TypeDescription {
            modifier: ModifierType::None,
            _type: self._type.clone(),
        }
    }
    pub fn undefined_number() -> Self {
        TypeDescription {
            modifier: ModifierType::None,
            _type: TypeDef::undefined_number(),
        }
    }
    pub fn void() -> Self {
        TypeDescription {
            modifier: ModifierType::None,
            _type: TypeDef::Void
        }
    }
}

#[derive(Debug, Clone)]

pub struct Variable {
    pub variable_type: TypeDescription,
    pub is_mutable: bool,
    pub ident: String,
    pub location: String
}


#[derive(Clone, Debug, PartialEq)]
pub  enum TypeDef {
    Number(NumberDef),
    Boolean,
    Func(FunctionDef),
    Void,
}

impl TypeDef {
    pub fn new() -> Self {
        Self::Boolean
    }
    pub fn void() -> Self {
        Self::Void
    }
    pub fn from_int(int: IntegerDef) -> Self {
        TypeDef::Number(NumberDef::Integer(int))
    }
    pub fn undefined_number() -> Self {
        TypeDef::Number(NumberDef::Undefined)
    }

    pub fn default_int() -> Self {
        TypeDef::Number(NumberDef::Integer(IntegerDef::I64))
    }
    pub fn to_string(&self) -> String {
        match self {
            TypeDef::Number(number_def) => number_def.to_string(),
            TypeDef::Boolean => "i8".to_string(),
            TypeDef::Func(_) => todo!(),
            TypeDef::Void => todo!()
        }
    }
    pub fn is_number(&self) -> bool {
        match self {
            TypeDef::Number(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum NumberDef {
    Integer(IntegerDef),
    Float(FloatDef),
    Undefined,
}

impl NumberDef {
    pub fn to_string(&self) -> String {
        match self {
            NumberDef::Integer(integer_def) => integer_def.to_string(),
            NumberDef::Float(float_def) => float_def.to_string(),
            NumberDef::Undefined => {
                todo!("error for trying to create a string from an undefined number")
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum IntegerDef {
    I128,
    I64,
    I32,
    I16,
    I8,
}

impl IntegerDef {
    pub fn to_string(&self) -> String {
        match self {
            IntegerDef::I128 => "i128".to_string(),
            IntegerDef::I64 => "i64".to_string(),
            IntegerDef::I32 => "i32".to_string(),
            IntegerDef::I16 => "i16".to_string(),
            IntegerDef::I8 => "i8".to_string(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FloatDef {
    F32,
    F64,
}

impl FloatDef {
    pub fn to_string(&self) -> String {
        match self {
            FloatDef::F32 => "f32".to_string(),
            FloatDef::F64 => "f64".to_string(),
        }
    }
}


#[derive(Debug, Clone)]

pub enum Value {
    Number(String),
    Boolean(bool)
}

impl Value {
    pub fn to_byte_code(&self) -> String {
        match self {
            Value::Number(num) => num.clone(),
            Value::Boolean(bool) => {
                if *bool {
                    "1".to_string()
                } else {
                    "0".to_string()
                }
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum Condition {
    Equal,
    NotEqual,
    LessEqual,
    GreatEqual,
    LessThan,
    GreaterThan,
}
impl Condition {
    pub fn to_byte_code(&self) -> String {
        match self {
            Condition::Equal => "eq".to_string(),
            Condition::NotEqual => "neq".to_string(),
            Condition::LessEqual => "lte".to_string(),
            Condition::GreatEqual => "gte".to_string(),
            Condition::LessThan => "lt".to_string(),
            Condition::GreaterThan => "gt".to_string(),
        }
    }
}
#[derive(Debug, Clone)]
pub enum ArithmaticOp {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

impl ArithmaticOp {
    pub fn to_byte_code(&self) -> String {
        match self {
            ArithmaticOp::Addition => "add".to_string(),
            ArithmaticOp::Subtraction => "sub".to_string(),
            ArithmaticOp::Multiplication => "mul".to_string(),
            ArithmaticOp::Division => "div".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionType {
    Variable(Variable),
    Value {
        value: Value,
    },
    ConditionalOp {
        condition: Condition,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    IndexedArray {
        variable: Variable,
        index: Box<Expression>,
    },
    ArithmaticOp {
        op: ArithmaticOp,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Scope {
        statements: Vec<Statement>
    },
    If {
        location: String,
        condition: Box<Expression>,
        expression: Box<Expression>,
        else_branch: Option<Box<Expression>>
    },
    WhileLoop {
        location: String,

        condition: Box<Expression>,
        expression: Box<Expression>
    },
    ForLoop {
        location: String,

        first_statement: Box<Statement>,
        condition: Box<Expression>,
        last_statement: Box<Statement>,
        expression: Box<Expression>
    },
    ForEachLoop {
        location: String,
        element_declaration: Box<Statement>,
        element: Variable,
        iterator: Variable,
        expression: Box<Expression>,
        iterator_size: usize,
    },
    ArrayLit {
        elements: Vec<Expression>
    }
}
#[derive(Debug, Clone)]

pub struct Expression {
    pub expr_type: TypeDescription,
    pub expression: ExpressionType
}


#[derive(Debug, Clone)]
pub enum Statement {
    VariableDecleration {
        variable: Variable,
        assignment: Option<Expression>
    },
    Return {
        expression: Expression,
    },
    Finish {
        expression: Expression,
    },
    Expression(Expression),
    Assignment {
        variable: Variable,
        expression: Expression,
        array_index: Option<usize>
    },
    Empty
}

