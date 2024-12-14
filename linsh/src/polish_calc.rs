use nom::{
    branch::alt,
    character::complete::{char, one_of},
    error::ErrorKind,
    multi::{many0, many1},
    IResult,
};
use rustyline::Editor;

#[derive(Debug)]
pub enum Expr {
    Num(u64),
    Add(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
}

pub fn eval(e: &Expr) -> u64 {
    match e {
        Expr::Num(n) => *n,
        Expr::Add(e1, e2) => eval(e1) + eval(e2),
        Expr::Mul(e1, e2) => eval(e1) * eval(e2),
    }
}

pub fn parse(input: &str) -> Option<Expr> {
    match parse_expr(input) {
        Ok((_, e)) => {
            println!("AST: {:?}",e );
            Some(e)
        }
        Err(e) => {
             println!("{e}");
             None
        }
    }
}

fn parse_num(input: &str) -> IResult<&str, Expr> {
    let (remaining_input, digits) = many1(one_of("0123456789"))(input)?;
    let number_str: String = digits.into_iter().collect();

    match number_str.parse::<u64>() {
        Ok(number) => Ok((remaining_input, Expr::Num(number))),
        Err(_) => Err(nom::Err::Failure(nom::error::Error::new(
            input,
            ErrorKind::Fail,
        ))),
    }
}

fn parse_op(c: &str) -> IResult<&str, Expr> {
    let (remaining_input, op) = one_of("+*")(c)?;
    let (remaining_input, e1) = parse_expr(c)?;
    let (remaining_input, e2) = parse_expr(c)?;

    match op {
        '+' => Ok((c, Expr::Add(Box::new(e1), Box::new(e2)))),
        '-' => Ok((c, Expr::Mul(Box::new(e1), Box::new(e2)))),
        _ => todo!()
    }
}

fn parse_expr(input: &str) -> IResult<&str, Expr> {
    let (c, _) = many0(char(' '))(input)?;

    let result = alt((parse_num, parse_op))(c)?;
    Ok(result)
}
