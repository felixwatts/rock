use rand::distributions::{Uniform, WeightedIndex};
use rock::noun::Noun;
use rock::reduce::{Result, hash, slash};
use rock::noun::{cell, atom};
use macros::expr;
use rand::prelude::*;
use lazy_static::lazy_static;

const POPULATION: usize = 50;
const MUT_RATE: f32 = 0.2;

lazy_static! {
    static ref RANGE_ZERO_ONE: rand::distributions::Uniform<f32> = Uniform::from(0.0..1.0);
    static ref SELECTION_DIST: WeightedIndex<usize> = WeightedIndex::new(&mut (1..=POPULATION).rev()).unwrap();
}

fn run(f: Noun, a: Noun) -> Result {
    let fun = expr!([a [f [0 1]]]);
    rock::nock(fun)
}

fn len(noun: &Noun) -> u32 {
    match noun {
        Noun::Atom(_) => 1,
        Noun::Cell(cell) => len(&cell.head) + len(&cell.tail)
    }
}

fn random_noun() -> Noun {
    match rand::random() {
        true => random_atom(),
        false => random_cell()
    }
}

fn random_atom() -> Noun {
    atom(rand::random())
}

fn random_cell() -> Noun {
    let head = random_noun();
    let tail = random_noun();
    cell(head, tail)
}

fn random_address_of(noun: &Noun) -> Noun {
    let len = len(noun);
    let addr = rand::thread_rng().gen_range(1..=len);
    atom(addr)
}

fn mutate(noun: Noun) -> Result {
    if RANGE_ZERO_ONE.sample(&mut rand::thread_rng()) < MUT_RATE {
        let addr = random_address_of(&noun);
        let repl = random_noun();
        Ok(expr!(#[addr repl noun]))
    } else {
        Ok(noun)
    }
}

fn cross(mother: &Noun, father: &Noun) -> Result {
    let addr_m = random_address_of(mother);
    let addr_f = random_address_of(father);

    let piece_f = expr!(/[addr_f father]);

    let child = expr!(#[addr_m piece_f mother]);

    let child = mutate(child)?;

    Ok(child)
}

fn fitness(noun: &Noun) -> u32 {
    let mut fitness = 0;
    for n in 0..10 {
        let target = n*n;
        let actual = run(noun.clone(), atom(n)); // TODO
        let diff = match actual {
            Err(_) => return u32::MAX,
            Ok(actual) => match actual {
                Noun::Atom(actual) => if actual > target {
                    actual - target
                } else {
                    target - actual
                },
                _ => return u32::MAX
            }
        };
        fitness += diff;
    }
    fitness
}

pub struct Gen(Vec<Noun>);

impl Gen {
    pub fn new() -> Self {
        let mut pop = vec![];
        for _ in 0..POPULATION {
            pop.push(random_noun())
        }
        Self(pop)
    }

    pub fn step(mut self) -> Self {
        self.rank();

        let mut next = vec![self.0[0].clone()];
        for _ in 1..POPULATION {
            let mother = self.select();
            let father = self.select();

            let child = cross(mother, father).unwrap_or(atom(0));
            next.push(child);
        }

        Gen(next)
    }

    fn rank(&mut self) {
        self.0.sort_by_cached_key(|i| fitness(i));
    }

    fn select(&self) -> &Noun {
        let index = SELECTION_DIST.sample(&mut rand::thread_rng());
        &self.0[index]
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_1() {
        let formula = expr!(4);
        let arg = expr!(100);
        let output = run(formula, arg);
        println!("{:?}", &output);
    }

    #[test]
    fn test_evo() {
        let mut pop = Gen::new();
        for _ in 0..1000 {
            pop = pop.step();
        }

        pop.rank();

        println!("{}", &fitness(&pop.0[0]));
    }
}