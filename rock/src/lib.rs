use noun::Noun;
use reduce::Result;

pub mod noun;
pub mod parse;
pub mod reduce;

pub fn nock(noun: Noun) -> Result {
    reduce::star(noun)
}
