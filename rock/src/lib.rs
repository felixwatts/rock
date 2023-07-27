use noun::Noun;
use reduce::ReduceResult;

pub mod noun;
pub mod parse;
pub mod reduce;

pub fn nock(noun: Noun) -> ReduceResult {
    reduce::star(noun)
}
