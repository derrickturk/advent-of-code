use std::{
    collections::HashMap,
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

fn chains(last: u32, target: u32, xs: &[u32],
  memo: &mut HashMap<(u32, *const u32), u64>) -> u64 {
    if let Some(val) = memo.get(&(last, xs.as_ptr())) {
        *val
    } else {
        let val = match xs.split_first() {
            Some((&x, xs)) => {
                if x - last > 3 {
                    0
                } else {
                    let left = chains(x, target, xs, memo);
                    left + chains(last, target, xs, memo)
                }
            },

            None => {
                if last == target {
                    1
                } else {
                    0
                }
            },
        };
        memo.insert((last, xs.as_ptr()), val);
        val
    }
}

fn full_chains(xs: &[u32]) -> u64 {
    if let Some(&target) = xs.last() {
        let mut memo = HashMap::new();
        chains(0, target, xs, &mut memo)
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
