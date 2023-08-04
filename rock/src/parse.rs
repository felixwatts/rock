use crate::noun::Cell;
use crate::noun::Noun;
use nom::bytes::complete::take_while;
use nom::multi::many_m_n;
use nom::Parser;
use nom::{
    error::{Error, ErrorKind},
    branch::alt, bytes::complete::tag, character::complete::one_of, multi::many0,
    sequence::delimited, IResult,
};
use std::rc::Rc;

impl TryFrom<&str> for Noun {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let (remaining, noun) = parse_noun(value).map_err(|e| e.to_string())?;
        if !remaining.is_empty() {
            Err("Expected end of input".into())
        } else {
            Ok(noun)
        }
    }
}

fn parse_noun(input: &str) -> IResult<&str, Noun> {
    let (remaining_input, _) = take_while(char::is_whitespace)(input)?;
    alt((parse_atom, parse_list))(remaining_input)
}

fn parse_atom(input: &str) -> IResult<&str, Noun> {
    let (remaining_input, decimal_chars) = many_m_n(1, 11, one_of("0123456789")).parse(input)?;
    let decimal_str: String = decimal_chars.into_iter().collect();
    let val = decimal_str
        .parse::<u32>()
        .map_err(|_| nom::Err::Failure(Error::<&str>::new(input, ErrorKind::Digit)))?;
    Ok((remaining_input, Noun::Atom(val)))
}

fn parse_list(input: &str) -> IResult<&str, Noun> {
    let mut parse = delimited(tag("["), many0(parse_noun), tag("]"));
    let (remaining_input, nouns) = parse(input)?;
    let cell = to_cell(&nouns);
    Ok((remaining_input, Noun::Cell(Rc::new(cell))))
}

fn to_cell(nouns: &[Noun]) -> Cell {
    match nouns.len() {
        2 => Cell {
            head: nouns[0].clone(),
            tail: nouns[1].clone(),
        },
        _ => Cell {
            head: nouns[0].clone(),
            tail: Noun::Cell(Rc::new(to_cell(&nouns[1..]))),
        },
    }
}

#[cfg(test)]
mod test {
    use crate::noun::Noun;

    use super::*;

    #[test]
    fn test_parse_noun() {
        let (remaining_input, ast) = parse_noun("1").unwrap();
        assert_eq!("", remaining_input);
        assert_eq!(Noun::Atom(1), ast);

        let (remaining_input, ast) = parse_noun("[1 2]").unwrap();
        assert_eq!("", remaining_input);
        assert_eq!(Noun::cell(Noun::atom(1), Noun::atom(2)), ast);

        let (remaining_input, ast) = parse_noun("[1 2 3]").unwrap();
        assert_eq!("", remaining_input);
        assert_eq!(Noun::cell(Noun::atom(1), Noun::cell(Noun::atom(2), Noun::atom(3))), ast);

        let result: Result<Noun, String> = "4294967296".try_into(); // too big for u32
        result.expect_err("Parse should have failed");
    }
}
