mod polish_calc;
mod parser;

use rustyline::Editor;
use crate::polish_calc::{parse, eval};

fn main() {
    let mut r1 = Editor::<()>::new().unwrap();
    loop {
        if let Ok(readline) = r1.readline(">> ") {
            if let Some(e) = parse(&readline) {
                println!("result: {}", eval(&e));
            }
        } else {
            break;
        }
    }
}
