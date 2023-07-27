use lazy_static::lazy_static;
use parse::parse_ast;
use macros::{ast, ast_test, reduce, crash};
use std::fmt::Display;

use parse::{Ast, Op, op, cell, Cell, atom};

#[derive(Debug, PartialEq)]
pub enum Err{
    InfiniteLoop
}

const True: Ast = Ast::Atom(0);
const False: Ast = Ast::Atom(1);

type ReduceResult = Result<Ast, Err>;

pub fn reduce_deep(ast: Ast) -> ReduceResult {
    Ok(
        match ast {
            Ast::Variable(..) => panic!("Variable in reduce"),
            Ast::Atom(_) => ast,
            Ast::Op(o, arg) => reduce(op(o, reduce(reduce_deep(*arg)?)?))?,
            Ast::Cell(c) => reduce(cell(reduce(reduce_deep(c.head)?)?, reduce(reduce_deep(c.tail)?)?))?
        }
    )
}

pub fn reduce(ast: Ast) -> ReduceResult {   

    reduce!("?[a b]" Ok(True.into()));
    reduce!("?a" Ok(False.into()));

    crash!("+[a b]");

    reduce!("+a" Ok(increment(&a)));

    reduce!("=[a a]" Ok(True.into()));
    reduce!("=[a b]" Ok(False.into()));

    crash!("/[0 a]");
    reduce!("/[1 a]" Ok(a));
    reduce!("/[2 a b]" Ok(a));
    reduce!("/[3 a b]" Ok(b));    
    reduce!("/[a [b c]]" {
        match a {
            Ast::Atom(val) => {
                let a = atom(val/2);

                if val % 2 == 0 {
                    // /[(a + a) b]        /[2 /[a b]]
                    Ok(ast!(/[2 /[a [b c]]]))
                } else {
                    // /[(a + a + 1) b]    /[3 /[a b]]
                    Ok(ast!(/[3 /[a [b c]]]))
                }
            },
            _ => ReduceResult::Err(Err::InfiniteLoop)
        }
    });
    crash!("/a");

    crash!("#[0 a]");
    reduce!("#[1 a b]" Ok(a));    
    reduce!("#[a b c]" {
        match a {
            Ast::Atom(val) => {
                let a = atom(val/2);

                Ok(
                    if val % 2 == 0 {
                        let d = atom(val+1); // (a + a + 1)
                        ast!(#[a [b /[d c]] c])
                    } else {
                        let d = atom(val-1); // (a + a)
                        ast!(#[a [/[d c] b] c])
                    }
                )
            },
            _ => ReduceResult::Err(Err::InfiniteLoop)
        }
    });  
    crash!("#a");  

    reduce!("*[a [b c] d]" Ok(ast!([*[a b c] *[a d]]))); 
    reduce!("*[a 0 b]" Ok(ast!(/[b a]))); 
    reduce!("*[a 1 b]" Ok(ast!(b))); 
    reduce!("*[a 2 b c]" Ok(ast!(*[*[a b] *[a c]]))); 
    reduce!("*[a 3 b]" Ok(ast!(?*[a b]))); 
    reduce!("*[a 4 b]" Ok(ast!(+*[a b]))); 
    reduce!("*[a 5 b c]" Ok(ast!(=[*[a b] *[a c]]))); 

    reduce!("*[a 6 b c d]" Ok(ast!(*[a *[[c d] 0 *[[2 3] 0 *[a 4 4 b]]]]))); 
    reduce!("*[a 7 b c]" Ok(ast!(*[*[a b] c]))); 
    reduce!("*[a 8 b c]" Ok(ast!(*[[*[a b] a] c]))); 
    reduce!("*[a 9 b c]" Ok(ast!(*[*[a c] 2 [0 1] 0 b]))); 
    reduce!("*[a 10 [b c] d]" Ok(ast!(#[b *[a c] *[a d]]))); 

    reduce!("*[a 11 [b c] d]" Ok(ast!(*[[*[a c] *[a d]] 0 3]))); 
    reduce!("*[a 11 b c]" Ok(ast!(*[a c]))); 
    reduce!("*a" ReduceResult::Err(Err::InfiniteLoop)); 

    Ok(ast)
}

fn increment(ast: &Ast) -> Ast {
    if let Ast::Atom(val) = ast {
        atom(val+1)
    } else {
        panic!("Increment non-atom")
    }
}

fn is_match(subject: &Ast, test: &Ast) -> Option<[Ast; 4]> {
    let mut assignments: [Ast; 4] = Default::default();
    let result = _is_match(subject, test, &mut assignments);
    if result {
        Some(assignments)
    } else {
        None
    }
}

fn _is_match(subject: &Ast, test: &Ast, assignments: &mut [Ast; 4]) -> bool {
    match (test, subject) {
        (_, Ast::Variable(_)) => panic!("Variable in AST"),
        (Ast::Atom(expected), Ast::Atom(actual)) => expected == actual,
        (Ast::Atom(_), _) => false,
        (Ast::Variable(name), Ast::Atom(val)) => {
            assign(assignments, name, atom(*val))
        },
        (Ast::Variable(name), Ast::Cell(c)) => {
            assign(assignments, name, cell(c.head.clone(), c.tail.clone()))
        },
        (Ast::Variable(_), _) => false,
        (Ast::Op(op_test, arg_test), Ast::Op(op_subject, arg_subject)) => op_test == op_subject && _is_match(&arg_subject, &arg_test, assignments),
        (Ast::Op(..), _) => false,
        (Ast::Cell(cell_test), Ast::Cell(cell_subject)) => _is_match(&cell_subject.head, &cell_test.head, assignments) && _is_match(&cell_subject.tail, &cell_test.tail, assignments),
        (Ast::Cell(..), _) => false
    }
}

fn assign(assignments: &mut [Ast; 4], name: &char, val: Ast) -> bool {
    let index = variable_name_to_index(name);
    match &assignments[index] {
        Ast::Atom(0) => {
            assignments[index] = val;
            true
        },
        existing => existing == &val
    }
}

fn variable_name_to_index(name: &char) -> usize {
    match name {
        'a' => 0,
        'b' => 1,
        'c' => 2,
        'd' => 3,
        _ => panic!("Unexpected variable name")
    }
}

 #[cfg(test)]
mod test {
    use parse::parse_ast;

    use super::*;

    #[test]
    fn test_is_match() {
        let (_, test) = parse_ast("?[a b]").unwrap();
        let (_, subject) = parse_ast("1").unwrap();
        assert!(is_match(&subject, &test).is_none());

        let (_, test) = parse_ast("?[a b]").unwrap();
        let (_, subject) = parse_ast("?[1 2]").unwrap();
        let result = is_match(&subject, &test);
        assert!(result.is_some());
        let assignments = result.unwrap();
        assert_eq!(atom(1), assignments[0]);
        assert_eq!(atom(2), assignments[1]);
    }

    #[test]
    fn test_reduce_completely() {
        assert_reduces_to("1", "1");

        assert_reduces_to("?1", "1");
        assert_reduces_to("?[1 2]", "0");

        assert_reduces_to("+41", "42");
        assert_reduces_to_infinite_loop("+[1 2]");

        assert_reduces_to("=[1 1]", "0");
        assert_reduces_to("=[1 2]", "1");

        assert_reduces_to_infinite_loop("/[0 1]");
        assert_reduces_to("/[1 2]", "2");
        assert_reduces_to("/[2 42 43]", "42");
        assert_reduces_to("/[3 42 43]", "43");

        assert_reduces_to("/[4 [[1 2] 3]]", "1");
        assert_reduces_to("/[5 [[1 2] 3]]", "2");
        assert_reduces_to("/[1 [531 25 99]]", "[531 [25 99]]");
        assert_reduces_to("/[1 [531 25 99]]", "[531 [25 99]]");
        assert_reduces_to("/[3 [531 25 99]]", "[25 99]");
        assert_reduces_to("/[6 [531 25 99]]", "25");
        assert_reduces_to_infinite_loop("/[12 [531 25 99]]");

        assert_reduces_to_infinite_loop("/42");

        assert_reduces_to_infinite_loop("#[0 42 43]");
        assert_reduces_to("#[1 42 43]", "42");
        assert_reduces_to("#[2 42 [[1 2] 3]]", "[42 3]");
        assert_reduces_to("#[3 42 [[1 2] 3]]", "[[1 2] 42]");
        assert_reduces_to_infinite_loop("#42");

        assert_reduces_to("+ + 40", "+ + 40");
        assert_reduces_completely_to("+ + 40", "42");

        assert_reduces_to("/[7 [1 [2 3]]]", "3");
        assert_reduces_to("#[6 5 1 2 3]", "[1 [5 3]]");


    }

    fn assert_reduces_to(input: &str, output: &str) {
        let ast = parse_ast(input).unwrap().1;
        let result = reduce(ast).unwrap();
        assert_eq!(parse_ast(output).unwrap().1, result);
    }

    fn assert_reduces_completely_to(input: &str, output: &str) {
        let ast = parse_ast(input).unwrap().1;
        let result = reduce_deep(ast).unwrap();
        assert_eq!(parse_ast(output).unwrap().1, result);
    }

    fn assert_reduces_to_infinite_loop(input: &str) {
        let ast = parse_ast(input).unwrap().1;
        let result = reduce(ast);
        assert_eq!(ReduceResult::Err(Err::InfiniteLoop), result);
    }
}