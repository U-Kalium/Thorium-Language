use crate::{compiler, vm};

#[test]
fn variable() {
    let file = include_str!("../../examples/variables.th");

    let byte_code = compiler::compile(file);
    let result: i64 = vm::run(byte_code).unwrap();
    dbg!(result);

    assert!((result == 69))    
}

#[test]

fn array() {
    let file = include_str!("../../examples/conditional.th");
    let byte_code = compiler::compile(file);
    let result: i64 = vm::run(byte_code).unwrap();
    dbg!(result);
    assert!(result == 2)
}
#[test]

fn conditional() {
    let file = include_str!("../../examples/arrays.th");
    let byte_code = compiler::compile(file);
    let result: i64 = vm::run(byte_code).unwrap();
    dbg!(result);
    assert!(result == 69)
}

#[test]

fn numbers() {
    let file = include_str!("../../examples/numbers.thb");
    // let byte_code = compiler::compile(file);
    let result: f32 = vm::run(file.to_string()).unwrap();
    dbg!(result);
    assert!(result == 3.14)
}

#[test]
fn loops() {
    
}