use std::fmt::Debug;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Cell {
    pub head: Noun,
    pub tail: Noun,
}

#[derive(Eq, PartialEq, Clone)]
pub enum Noun {
    Atom(u32),
    Cell(Rc<Cell>),
}

pub fn cell(head: Noun, tail: Noun) -> Noun {
    Noun::Cell(Rc::new(Cell { head, tail }))
}

pub fn atom(val: u32) -> Noun {
    Noun::Atom(val)
}

impl Debug for Noun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Noun::Atom(val) => write!(f, "{}", val),
            Noun::Cell(cell) => write!(f, "[{} {}]", cell.head, cell.tail),
        }
    }
}

impl Display for Noun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
