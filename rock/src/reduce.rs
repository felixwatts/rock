use crate::noun::{atom, cell, Noun};
use macros::{expr, reduce};

#[derive(Debug, PartialEq)]
pub enum Err {
    InfiniteLoop,
}

pub type ReduceResult = Result<Noun, Err>;

const INFINITE_LOOP: ReduceResult = ReduceResult::Err(Err::InfiniteLoop);

const TRUE: ReduceResult = Ok(Noun::Atom(0));
const FALSE: ReduceResult = Ok(Noun::Atom(1));

fn to_bool(value: bool) -> ReduceResult {
    if value {
        TRUE
    } else {
        FALSE
    }
}

pub fn inc(noun: Noun) -> ReduceResult {
    match noun {
        Noun::Atom(val) => Ok(Noun::Atom(val + 1)),
        Noun::Cell(_) => INFINITE_LOOP,
    }
}

pub fn eq(noun: Noun) -> ReduceResult {
    match noun {
        Noun::Cell(cell) => to_bool(cell.head == cell.tail),
        Noun::Atom(_) => INFINITE_LOOP,
    }
}

pub fn question(noun: Noun) -> ReduceResult {
    match noun {
        Noun::Cell(_) => TRUE,
        Noun::Atom(_) => FALSE,
    }
}

pub fn slash(noun: Noun) -> ReduceResult {
    match noun {
        Noun::Cell(c) => match (&c.head, &c.tail) {
            (Noun::Atom(0), _) => INFINITE_LOOP,
            (Noun::Atom(1), a) => Ok(a.clone()),
            (Noun::Atom(2), Noun::Cell(tail_cell)) => Ok(tail_cell.head.clone()),
            (Noun::Atom(3), Noun::Cell(tail_cell)) => Ok(tail_cell.tail.clone()),
            (Noun::Atom(n), b) if matches!(b, Noun::Cell(_)) => {
                let a = Noun::Atom(n / 2);
                let addr = Noun::Atom(if n % 2 == 0 { 2 } else { 3 });
                Ok(expr!(/[addr /[a b]]))
            }
            _ => INFINITE_LOOP,
        },
        _ => INFINITE_LOOP,
    }
}

pub fn hash(noun: Noun) -> ReduceResult {
    match noun {
        Noun::Cell(root_cell) => match (&root_cell.head, &root_cell.tail) {
            (Noun::Atom(0), _) => INFINITE_LOOP,
            (Noun::Atom(1), Noun::Cell(tail_cell)) => Ok(tail_cell.head.clone()),
            (Noun::Atom(n), Noun::Cell(tail_cell)) => {
                let a = atom(n / 2);
                let b = tail_cell.head.clone();
                let c = tail_cell.tail.clone();

                if n % 2 == 0 {
                    let a_plus_a_plus_1 = atom(n + 1);
                    Ok(expr!(#[a [[b /[a_plus_a_plus_1 c]] c]]))
                } else {
                    let a_plus_a = atom(n - 1);
                    Ok(expr!(#[a [[/[a_plus_a c] b] c]]))
                }
            }
            _ => INFINITE_LOOP,
        },
        _ => INFINITE_LOOP,
    }
}

pub fn star(subject: Noun) -> ReduceResult {
    reduce!([a [b c] d] Ok(expr!([*[a b c] *[a d]])));
    reduce!([a 0 b] Ok(expr!(/[b a])));
    reduce!([_a 1 b] Ok(expr!(b)));
    reduce!([a 2 b c] Ok(expr!(*[*[a b] *[a c]])));
    reduce!([a 3 b] Ok(expr!(?*[a b])));
    reduce!([a 4 b] Ok(expr!(+*[a b])));
    reduce!([a 5 b c] Ok(expr!(=[*[a b] *[a c]])));

    reduce!([a 6 b c d] Ok(expr!(*[a *[[c d] 0 *[[2 3] 0 *[a 4 4 b]]]])));
    reduce!([a 7 b c] Ok(expr!(*[*[a b] c])));
    reduce!([a 8 b c] Ok(expr!(*[[*[a b] a] c])));
    reduce!([a 9 b c] Ok(expr!(*[*[a c] 2 [0 1] 0 b])));
    reduce!([a 10 [b c] d] Ok(expr!(#[b *[a c] *[a d]])));

    reduce!([a 11 [_b c] d] Ok(expr!(*[[*[a c] *[a d]] 0 3])));
    reduce!([a 11 _b c]  Ok(expr!(*[a c])));

    INFINITE_LOOP
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_question() {
        assert_op("[1 2]", "0", question);
        assert_op("1", "1", question);
    }

    #[test]
    fn test_inc() {
        assert_crash("[1 2]", inc);
        assert_op("42", "43", inc);
    }

    #[test]
    fn test_eq() {
        assert_crash("1", eq);
        assert_op("[1 2]", "1", eq);
        assert_op("[1 1]", "0", eq);
        assert_op("[[1 2] [1 3]]", "1", eq);
        assert_op("[[1 2] [1 2]]", "0", eq);
    }

    #[test]
    fn test_slash() {
        assert_crash("[0 1]", slash);
        assert_op("[1 2]", "2", slash);
        assert_op("[2 42 43]", "42", slash);
        assert_op("[3 42 43]", "43", slash);

        assert_op("[4 [[1 2] 3]]", "1", slash);
        assert_op("[5 [[1 2] 3]]", "2", slash);
        assert_op("[1 [531 25 99]]", "[531 [25 99]]", slash);
        assert_op("[1 [531 25 99]]", "[531 [25 99]]", slash);
        assert_op("[3 [531 25 99]]", "[25 99]", slash);
        assert_op("[6 [531 25 99]]", "25", slash);
        assert_crash("[12 [531 25 99]]", slash);
        assert_crash("42", slash);
    }

    #[test]
    fn test_hash() {
        assert_crash("[0 42 43]", hash);
        assert_op("[1 42 43]", "42", hash);
        assert_op("[2 42 [[1 2] 3]]", "[42 3]", hash);
        assert_op("[3 42 [[1 2] 3]]", "[[1 2] 42]", hash);
        assert_crash("42", hash);
    }

    fn assert_op(noun: &str, expected: &str, f: fn(Noun) -> ReduceResult) {
        let noun: Noun = noun.try_into().unwrap();
        let actual = f(noun).unwrap();
        let expected: Noun = expected.try_into().unwrap();
        assert_eq!(actual, expected);
    }

    fn assert_crash(noun: &str, f: fn(Noun) -> ReduceResult) {
        let noun: Noun = noun.try_into().unwrap();
        f(noun).expect_err("Expected crash");
    }
}
