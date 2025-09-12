use crate::{compiler, vm};

#[test]
fn variable() {
    let file = "
fn main() i64 {
    var a i64; let b i64 {
        return 69
    }
    a = 10
    return a
}
";

    let byte_code = compiler::compile(file);
    let result: i64 = vm::run(byte_code).unwrap();

    assert!((result == 10))    
}
