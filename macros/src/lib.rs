use proc_macro::TokenStream;
use proc_macro2::{Ident, Punct};
use quote::quote;
use syn::{
    bracketed,
    parse::{Parse, ParseBuffer},
    parse_macro_input, token, LitInt, Result, Token,
};

#[proc_macro]
pub fn expr(tokens: TokenStream) -> TokenStream {
    let expr: Expr = parse_macro_input!(tokens);
    expr.to_constructor_code().into()
}

#[proc_macro]
pub fn reduce(tokens: TokenStream) -> TokenStream {
    let expr: Test = parse_macro_input!(tokens);
    let variable_decl_code = expr.expr.to_test_variable_declaration_code();
    let test_code = expr.expr.to_test_code();
    let result = expr.result;

    quote! {
        {
            let subject = subject.clone();

            #variable_decl_code;
            if {
                #test_code
            } {
                return #result
            }
        }
    }
    .into()
}

struct Test {
    expr: Expr,
    result: syn::Expr,
}

impl Parse for Test {
    fn parse(input: &ParseBuffer) -> Result<Self> {
        Ok(Test {
            expr: input.parse()?,
            result: input.parse()?,
        })
    }
}

#[derive(Clone)]
struct Cell {
    _bracket_token: token::Bracket,
    head: Box<Expr>,
    tail: Box<Expr>,
}

impl Parse for Cell {
    fn parse(input: &ParseBuffer) -> Result<Self> {
        let content;
        let _bracket_token = bracketed!(content in input);

        let mut parts = vec![];
        while !content.is_empty() {
            let part: Expr = content.parse()?;
            parts.push(part);
        }

        let cell = list_to_cell(&parts[..]);

        Ok(cell)
    }
}

fn list_to_cell(list: &[Expr]) -> Cell {
    match list.len() {
        2 => Cell {
            _bracket_token: Default::default(),
            head: Box::new(list[0].clone()),
            tail: Box::new(list[1].clone()),
        },
        _ => Cell {
            _bracket_token: Default::default(),
            head: Box::new(list[0].clone()),
            tail: Box::new(Expr::Cell(list_to_cell(&list[1..]))),
        },
    }
}

impl Cell {
    fn to_constructor_code(&self) -> proc_macro2::TokenStream {
        let head_code = self.head.to_constructor_code();
        let tail_code = self.tail.to_constructor_code();
        quote! {
            Noun::cell(#head_code, #tail_code)
        }
    }
}

impl Expr {
    fn to_constructor_code(&self) -> proc_macro2::TokenStream {
        match self {
            Expr::Atom(lit_int) => quote! {  Noun::atom(#lit_int) },
            Expr::Variable(ident) => quote! { #ident.clone() },
            Expr::Cell(cell) => cell.to_constructor_code(),
            Expr::Op(op) => op.to_constructor_code(),
        }
    }

    fn to_test_variable_declaration_code(&self) -> proc_macro2::TokenStream {
        match self {
            Expr::Variable(ident) => quote! { let mut #ident =  Noun::atom(0); },
            Expr::Cell(cell) => {
                let head = cell.head.to_test_variable_declaration_code();
                let tail = cell.tail.to_test_variable_declaration_code();
                quote! {
                    #head
                    #tail
                }
            }
            Expr::Op(op) => op.expr.to_test_variable_declaration_code(),
            _ => quote! {},
        }
    }

    fn to_test_code(&self) -> proc_macro2::TokenStream {
        match self {
            Expr::Atom(lit_int) => quote! {
                matches!(subject, Noun::Atom(#lit_int))
            },
            Expr::Variable(ident) => quote! {
                #ident = subject;
                true
            },
            Expr::Cell(cell) => {
                let head_code = cell.head.to_test_code();
                let tail_code = cell.tail.to_test_code();
                quote! {
                    if let Noun::Cell(cell_content) = subject {
                        ({
                            let subject = cell_content.head.clone();
                            #head_code
                        })
                        &&
                        ({
                            let subject = cell_content.tail.clone();
                            #tail_code
                        })
                    } else {
                        false
                    }
                }
            }
            Expr::Op(_) => panic!("Op in test"),
        }
    }
}

#[derive(Clone)]
enum Expr {
    Atom(LitInt),
    Cell(Cell),
    Variable(Ident),
    Op(Op),
}

impl Parse for Expr {
    fn parse(input: &ParseBuffer) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(token::Bracket) {
            Ok(Expr::Cell(input.parse()?))
        } else if lookahead.peek(LitInt) {
            Ok(Expr::Atom(input.parse()?))
        } else if lookahead.peek(syn::Ident) {
            Ok(Expr::Variable(input.parse()?))
        } else if 
            lookahead.peek(Token![*])
            || lookahead.peek(Token![/])
            || lookahead.peek(Token![=])
            || lookahead.peek(Token![+])
            || lookahead.peek(Token![?])
            || lookahead.peek(Token![#]) {
            Ok(Expr::Op(input.parse()?))
        } else {
            Err(lookahead.error())
        }
    }
}

#[derive(Clone)]
struct Op {
    op: Punct,
    expr: Box<Expr>,
}

impl Parse for Op {
    fn parse(input: &ParseBuffer) -> Result<Self> {
        Ok(Op {
            op: input.parse()?,
            expr: input.parse()?,
        })
    }
}

impl Op {
    fn to_constructor_code(&self) -> proc_macro2::TokenStream {
        let expr_code = self.expr.to_constructor_code();

        match self.op.as_char() {
            '*' => quote! { star(#expr_code)? },
            '/' => quote! { slash(#expr_code)? },
            '?' => quote! { question(#expr_code)? },
            '#' => quote! { hash(#expr_code)? },
            '+' => quote! { inc(#expr_code)? },
            '=' => quote! { eq(#expr_code)? },
            _ => panic!(),
        }
    }
}
