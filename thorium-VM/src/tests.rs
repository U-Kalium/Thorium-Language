use super::*;
use StackValue::*;

#[test]
fn copy_instruction() {
    let byte_code = 
"
func $start \"_start\" :
    declare %a %b %c
    i32 push 69 30 1 0
    set %c
    i32 push 420
    set stack %c + 1
    cpy %c + top top
    // insert 90
    // insert 100
    // insert 69
    // i8 cpy 2
    // get %a
endfunc
".to_string();

// 69 420 1 
// c = 0

    let result = tokenize_and_run(byte_code).unwrap();
    let expected_stack = vec![
        I32(69),
        I32(420),
        I32(420),

        // I8(69),
        // I32(420),
    ];
    // assert_eq!(result.0, expected_stack)
}