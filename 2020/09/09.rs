use std::{
    error::Error,
    env,
    io::{self, BufRead},
    ops::Range,
};

fn first_naughty_number(xs: &[u64], preamble: usize) -> Option<u64> {
    if xs.len() <= preamble {
        return None;
    }

    for i in preamble..xs.len() {
        let z = xs[i];
        let mut found = false;
        for (j, &x) in xs[(i - preamble)..i].iter().enumerate() {
            for (k, &y) in xs[(i - preamble)..i].iter().enumerate() {
                if j != k && x + y == z {
                    found = true;
                    break;
                }
            }
            if found {
                break;
            }
        }
        if !found {
            return Some(z);
        }
    }

    None
}

fn subset_sum(xs: &[u64], target: u64) -> Option<Range<usize>> {
    let mut sum = 0;
    let mut end = 0;
    for begin in 0..xs.len() {
        while sum < target && end < xs.len() {
            sum += xs[end];
            end += 1;
        }

        if sum == target {
            return Some(begin..end);
        }

        sum -= xs[begin];
    }

    None
}

fn main() -> Result<(), Box<dyn Error>> {
    let preamble: usize = env::args().nth(1)
        .ok_or("specify preamble length")?
        .parse()?;

    let stdin = io::stdin();

    /* u64 because if any are negative our "cute" interview-problem solution,
     *   beloved of smug hackermen, eats dirt
     */
    // there just has to be a way to write this with a .collect() one-liner
    let mut xmas: Vec<u64> = Vec::new();
    for l in stdin.lock().lines() {
        xmas.push(l?.parse()?);
    }

    if let Some(x) = first_naughty_number(&xmas, preamble) {
        println!("first naughty number: {}", x);

        if let Some(rng) = subset_sum(&xmas, x) {
            let min = xmas[rng.clone()].iter().min()
              .ok_or("empty subset")?;
            let max = xmas[rng.clone()].iter().max()
              .ok_or("empty subset")?;
            println!("min + max = {}", min + max);
        } else {
            println!("no subarray sums to {}", x);
        }
    } else {
        println!("no naughty numbers");
    }

    Ok(())
}
