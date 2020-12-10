use std::{
    error::Error,
    io::{self, BufRead},
};

fn diff(xs: &[u32]) -> impl Iterator<Item=u32> + '_ {
    xs.iter().scan(0, |last, &x| {
        let ret = x - *last;
        *last = x;
        Some(ret)
    })
}

fn chains(last: u32, target: u32, xs: &[u32]) -> u64 {
    match xs.split_first() {
        Some((&x, xs)) => {
            if x - last > 3 {
                0
            } else {
                chains(x, target, xs) + chains(last, target, xs)
            }
        },

        None => {
            if last == target {
                1
            } else {
                0
            }
        },
    }
}

fn full_chains(xs: &[u32]) -> u64 {
    if let Some(&target) = xs.last() {
        chains(0, target, xs)
    } else {
        0
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();
    let mut adapters = Vec::new();
    for l in stdin.lock().lines() {
        adapters.push(l?.parse::<u32>()?);
    }
    adapters.sort();
    adapters.push(adapters.last().ok_or("empty")? + 3);

    let diffs: Vec<u32> = diff(&adapters).collect();
    let ones = diffs.iter().filter(|&&x| x == 1).count();
    let threes = diffs.iter().filter(|&&x| x == 3).count();
    println!("{}", ones * threes);
    println!("{}", full_chains(&adapters));

    Ok(())
}
