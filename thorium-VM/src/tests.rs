use super::*;
use StackValue::*;

#[test]
fn copy_instruction() {
    let byte_code = 
"
func $start \"_start\" :
    declare %a %b
    i32 push 69 420 100 
    cpy 1 %a
    insert 90
    insert 100
    insert 69
    i8 cpy 2
    get %a
endfunc
".to_string();

    let result = tokenize_and_run(byte_code).unwrap();
    let expected_stack = vec![
        I32(69),
        I32(420),
        I32(100),
        I8(69),
        I32(420),
    ];
    assert_eq!(result, expected_stack)
}