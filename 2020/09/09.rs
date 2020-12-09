use std::{
    error::Error,
    env,
    io::{self, BufRead},
};

fn first_naughty_number(xs: &[i64], preamble: usize) -> Option<i64> {
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

fn main() -> Result<(), Box<dyn Error>> {
    let preamble: usize = env::args().nth(1)
        .ok_or("specify preamble length")?
        .parse()?;

    let stdin = io::stdin();

    // there just has to be a way to write with a .collect() one-liner
    let mut xmas: Vec<i64> = Vec::new();
    for l in stdin.lock().lines() {
        xmas.push(l?.parse()?);
    }

    if let Some(x) = first_naughty_number(&xmas, preamble) {
        println!("first naughty number: {}", x);
    } else {
        println!("no naughty numbers");
    }

    Ok(())
}
