use std::{env, io, mem};
use std::io::BufRead;

struct Iterate<T, F> {
    next: T,
    iter: F,
}

impl<T: Clone, F: FnMut(T) -> T> Iterator for Iterate<T, F> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        let next = self.next.clone();
        let mut next = (self.iter)(next);
        mem::swap(&mut self.next, &mut next);
        Some(next)
    }
}

fn iterate<T, F>(iter: F, init: T) -> impl Iterator<Item = T>
  where T: Clone, F: FnMut(T) -> T {
    Iterate { next: init, iter }
}

#[inline]
fn fuel(mass: i32) -> i32 {
    mass / 3 - 2
}

#[inline]
fn total_fuel(mass: i32) -> i32 {
    iterate(fuel, mass).skip(1).take_while(|f| *f > 0).sum()
}

fn main() {
    if let Some(prob) = env::args().skip(1).next() {
        let stdin = io::stdin();
        let masses = stdin.lock().lines()
            .map(|l| l.unwrap().trim_end().parse::<i32>().unwrap());
        match prob.as_str() {
            "1" => println!("{}", masses.map(fuel).sum::<i32>()),
            "2" => println!("{}", masses.map(total_fuel).sum::<i32>()),
            _ => eprintln!("Usage: problem1 1|2 <input.txt"),
        }
    } else {
        eprintln!("Usage: intcode 1|2 <input.txt");
    }
}
