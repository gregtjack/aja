
# Aja Language

use std::io
use std::types::Result
use glfw

fn main() {
    // variables
    // let = immutable
    // var = mutable
    let x = "hello world";
    var test = add(1, 1);
    
    // using expr?
    using io {
        print("Test print.\n");
        print(x++);
    }
    
    // if expr
    if test == 2 {
        print("Passed");
    } else {
        print("fail");
    }

    // match expr
    let epic = match test {
        0 -> "bruh",
        1 -> { "bruh" }
        2 -> "nice.",
        // wildcard (_)
        _ -> "wahh??"
    }

    // ? operator
    let num = parseU64(str, 10)?; // <--- if Ok, unwraps inner value, if error, return from the func with the error
    // Maybe initially just panic if error for ease of implementation.
    
    return Result::Ok(0);
}

fn add(a, b) {
    a + b
}




