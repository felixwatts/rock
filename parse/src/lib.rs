use std::fmt::Debug;
use std::fmt::Display;

use nom::Parser;
use nom::bytes::complete::take_while;
use nom::multi::many_m_n;
use nom::{IResult, sequence::delimited, bytes::complete::tag, multi::many0, branch::alt, character::complete::one_of};

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Op {
    Inc,
    Eq,
    Hash,
    Slash,
    Star,
    Question
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Inc => write!(f, "+")?, 
            Self::Eq => write!(f, "=")?, 
            Self::Hash => write!(f, "#")?, 
            Self::Slash => write!(f, "/")?, 
            Self::Star => write!(f, "*")?, 
            Self::Question => write!(f, "?")?
        };

        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Cell{
    pub head: Ast,
    pub tail: Ast
}

#[derive(Eq, PartialEq, Clone)]
pub enum Ast{
    Op(Op, Box<Ast>),
    Atom(u32),
    Cell(Box<Cell>),
    Variable(String)
}

impl Default for Ast{
    fn default() -> Self {
        atom(0)
    }
}

impl Debug for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ast::Op(op, arg) => write!(f, "{}{}", op, &*arg),
            Ast::Atom(n) => write!(f, "{}", n),
            Ast::Cell(cell) => write!(f, "[{} {}]", &cell.head, &cell.tail),
            Ast::Variable(name) => write!(f, "{}", &name)
        }
    }
}

pub fn parse_ast(input: &str) -> IResult<&str, Ast> {
    let (remaining_input, _) = take_while(|c: char| c.is_whitespace())(input)?;
    return alt((parse_variable, parse_op, parse_atom, parse_list))(remaining_input);
}

fn parse_op(input: &str) -> IResult<&str, Ast> {
    let (remaining_input, op) = one_of("+=#/*?")(input)?;
    let (remaining_input, subject) = parse_ast(remaining_input)?;

    let op = match op {
        '+' => Op::Inc,
        '=' => Op::Eq,
        '#' => Op::Hash,
        '/' => Op::Slash,
        '*' => Op::Star,
        '?' => Op::Question,
        _ => panic!()
    };

    Ok((remaining_input, Ast::Op(op, Box::new(subject))))
}

fn parse_variable(input: &str) -> IResult<&str, Ast> {
    let (remaining_input, name) = one_of("abcd").parse(input)?;
    Ok((remaining_input, Ast::Variable(name.to_string())))
}

fn parse_atom(input: &str) -> IResult<&str, Ast> {
    let (remaining_input, decimal_chars) = many_m_n(1, 1024, one_of("0123456789")).parse(input)?;
    let decimal_str: String = decimal_chars.into_iter().collect();
    let val = u32::from_str_radix(&decimal_str, 10).unwrap();
    Ok((remaining_input, Ast::Atom(val)))
}

fn parse_list(input: &str) -> IResult<&str, Ast> {
    let mut parse = delimited(tag("["), many0(parse_ast), tag("]"));
    let (remaining_input, asts) = parse(input)?;
    let cell = to_cell(&asts);
    Ok((remaining_input, Ast::Cell(Box::new(cell))))
}

fn to_cell(asts: &[Ast]) -> Cell {
    match asts.len() {
        2 => Cell{head: asts[0].clone(), tail: asts[1].clone()},
        _ => Cell{head: asts[0].clone(), tail: Ast::Cell(Box::new(to_cell(&asts[1..])))}
    }
}

pub fn atom(value: u32) -> Ast {
    Ast::Atom(value)
}

pub fn op(op: Op, subject: Ast) -> Ast {
    Ast::Op(op, Box::new(subject))
}

pub fn cell(head: Ast, tail: Ast) -> Ast {
    Ast::Cell(Box::new(Cell{ head, tail }))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_ast() {
        let (remaining_input, ast) = parse_ast("1").unwrap();
        assert_eq!("", remaining_input);
        assert_eq!(atom(1), ast);

        let (remaining_input, ast) = parse_ast("[1 2]").unwrap();
        assert_eq!("", remaining_input);
        assert_eq!(cell(atom(1), atom(2)), ast);

        let (remaining_input, ast) = parse_ast("[1 2 3]").unwrap();
        assert_eq!("", remaining_input);
        assert_eq!(cell(atom(1), cell(atom(2), atom(3))), ast);

        let (remaining_input, ast) = parse_ast("/1").unwrap();
        assert_eq!("", remaining_input);
        assert_eq!(op(Op::Slash, atom(1)), ast);

        let (remaining_input, ast) = parse_ast("=[1 2]").unwrap();
        assert_eq!("", remaining_input);
        assert_eq!(op(Op::Eq, cell(atom(1), atom(2))), ast);
    }
}