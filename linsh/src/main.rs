mod parser;
mod typing;

use std::{env, error::Error, fs};

use nom::error::convert_error;

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("please specify file name as:\n cargo run codes/ex1.lin");
        return Err("argument is insufficient".into());
    }

    let content = fs::read_to_string(&args[1])?;

    let ast = parser::parse_expr(&content);
    println!("AST:\n{:#?}\n", ast);

    match ast {
        Ok((_, expr)) => {
            let mut ctx = typing::TypeEnv::new();
            println!("expr:\n{content}");

            let a = typing::typing(&expr, &mut ctx, 0);
            println!("'s type is {a:?}.");
        }
        Err(nom::Err::Error(e)) => {
            let msg = convert_error(content.as_str(), e);
            eprintln!("parse error\n{msg}");
            return Err(msg.into());
        }
        _ => (),
    }

    Ok(())
}
