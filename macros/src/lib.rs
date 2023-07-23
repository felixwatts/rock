use proc_macro::TokenStream;
use parse::{parse_ast, Ast, Op};
use quote::{quote, format_ident};
use syn::{Expr, token, parse::{ParseStream, Parse}, braced, LitStr, parse_macro_input};

#[proc_macro]
pub fn ast(input: TokenStream) -> TokenStream {
    _ast(input.into()).into()
}

fn _ast(input: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let s = input.to_string();
    let (remainder, ast) = parse_ast(&s).expect("Invalid AST");
    assert_eq!(0, remainder.len(), "Invalid AST");
    let constructor_code = ast_to_constructor(&ast);
    constructor_code
}

struct Reducer{
    test: LitStr,
    code: Expr
}

impl Parse for Reducer {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Reducer {
            test: input.parse()?,
            code: input.parse()?,
        })
    }
}

struct Crasher{
    test: LitStr
}

impl Parse for Crasher {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Crasher {
            test: input.parse()?
        })
    }
}

#[proc_macro]
pub fn reduce(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as Reducer);

    let content = input.code;
    let test = input.test;

    let (_, test) = parse_ast(&test.value()).expect("Invalid AST");
    let test = ast_to_test(&test);

    quote!{
        let test = #test;
        if let Some(mut assignments) = is_match(&ast, &test) {
            let a = assignments[0].clone();
            let b = assignments[1].clone();
            let c = assignments[2].clone();
            let d = assignments[3].clone();
    
            return #content
        }
    }.into()
}

#[proc_macro]
pub fn crash(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as Crasher);

    let test = input.test;

    let (_, test) = parse_ast(&test.value()).expect("Invalid AST");
    let test = ast_to_test(&test);

    quote!{
        let test = #test;
        if let Some(assignments) = is_match(&ast, &test) {
            return ReduceResult::Err(Err::InfiniteLoop);
        }
    }.into()
}

#[proc_macro]
pub fn ast_test(input: TokenStream) -> TokenStream {
    _ast_test(input.into()).into()
}

fn _ast_test(input: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let s = input.to_string();
    let (remainder, ast) = parse_ast(&s).expect("Invalid AST");
    assert_eq!(0, remainder.len(), "Invalid AST");
    let constructor_code = ast_to_test(&ast);
    constructor_code
}

fn ast_to_test(ast: &Ast) -> proc_macro2::TokenStream {
    match ast {
        Ast::Variable(name) => {
            quote!{ Ast::Variable(#name.to_string()) }
        },
        Ast::Atom(val) => quote!{ Ast::Atom(#val) },
        Ast::Cell(cell) => {
            let head = ast_to_test(&cell.head);
            let tail = ast_to_test(&cell.tail);
            quote!{ Ast::Cell(Box::new(Cell{ head: #head, tail: #tail })) }
        },
        Ast::Op(op, arg) => {
            let arg = ast_to_test(arg);
            let op = op_to_constructor(op);
            quote!{ Ast::Op(#op, Box::new(#arg)) }
        }
    }
}

fn ast_to_constructor(ast: &Ast) -> proc_macro2::TokenStream {
    match ast {
        Ast::Variable(name) => {
            let name_ident = format_ident!("{}", name);
            quote!{ #name_ident.clone() }
        },
        Ast::Atom(val) => quote!{ Ast::Atom(#val) },
        Ast::Cell(cell) => {
            let head = ast_to_constructor(&cell.head);
            let tail = ast_to_constructor(&cell.tail);
            quote!{ Ast::Cell(Box::new(Cell{ head: #head, tail: #tail })) }
        },
        Ast::Op(op, arg) => {
            let arg = ast_to_constructor(arg);
            let op = op_to_constructor(op);
            quote!{ reduce(Ast::Op(#op, Box::new(#arg)))? }
        }
    }
}

fn op_to_constructor(op: &Op) -> proc_macro2::TokenStream {
    match op {
        Op::Eq => quote!{ Op::Eq },
        Op::Hash => quote!{ Op::Hash },
        Op::Inc => quote!{ Op::Inc },
        Op::Question => quote!{ Op::Question },
        Op::Slash => quote!{ Op::Slash },
        Op::Star => quote!{ Op::Star },
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test(){
        // #[a [/[(a + a) c] b] c]
        let stream: proc_macro2::TokenStream = "#[a 2]".parse().unwrap();
        _ast(stream);
    }
}