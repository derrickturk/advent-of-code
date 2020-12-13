/*
▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄
██░▄▄▄██░██░██░▄▄▀██░█▀▄████░▄▀▄░█░▄▄▀█▄▄░▄▄██░██░
██░▄▄███░██░██░█████░▄▀█████░█░█░█░▀▀░███░████░▄▄░
██░█████▄▀▀▄██░▀▀▄██░██░████░███░█░██░███░████░██░
▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
*/

// https://shainer.github.io/crypto/math/2017/10/22/chinese-remainder-theorem.html
// https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
// ^^^ none of the above actually implement a working program for this
// this thing gets the right answer except for the last equation, so WTF knows
// I just don't care any more!

use std::{
    error::Error,
    io::{self, Read},
};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Bus {
    X,
    RealBus(u32),
}

fn gauss(buses: &[(usize, u32)]) -> Option<u64> {
    let mut result: u64 = 0;
    let moduli_product: u64 = buses.iter().map(|&(_, n)| n as u64).product();
    for &(i, n) in buses {
        let b = moduli_product / n as u64;
        println!("solving for n mod {} == {}",
          n as u64,
          ((n as i32 - i as i32) % n as i32) as u64);
        println!("have invmod({}, {}) = {}", b, n as u64, invmod(b, n as u64)?);
        result +=
          ((n as i32 - i as i32) % n as i32) as u64
          * b
          * invmod(b, n as u64)?;
    }
    Some(result % moduli_product)
}

fn extended_euclid(a: u64, b: u64) -> (i64, i64, i64) {
    let (mut old_r, mut r) = (a as i64, b as i64);
    let (mut old_s, mut s) = (1, 0);
    let (mut old_t, mut t) = (0, 1);

    while r != 0 {
        let q = old_r / r;

        // fuck the lack of destructuring assignment and fuck
        //   whoever came up with these variable names
        let old_r_ = r;
        let r_ = old_r - q * r;
        old_r = old_r_;
        r = r_;

        let old_s_ = s;
        let s_ = old_s - q * s;
        old_s = old_s_;
        s = s_;

        let old_t_ = t;
        let t_ = old_t - q * t;
        old_t = old_t_;
        t = t_;
    }

    (old_r, old_s, old_t)
}

fn invmod(a: u64, m: u64) -> Option<u64> {
    let (gcd, x, _) = extended_euclid(a, m);
    if gcd == 1 {
        let r = x % m as i64;
        if r < 0 {
            Some((r.abs() as u64 + m) % m)
        } else {
            Some(r as u64)
        }
    } else {
        None
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut iter = input.as_str().lines();
    let _ = iter.next();

    let buses = match iter.next() {
        Some(line) => {
            let mut result = Vec::new();
            for b in line.split(',') {
                result.push(match b {
                    "x" => Bus::X,
                    num => Bus::RealBus(num.parse()?),
                });
            }
            result
        },

        None => Err("Invalid input.")?,
    };

    let mut sched = Vec::new();
    for (i, &b) in buses.iter().enumerate() {
        if let Bus::RealBus(n) = b {
            sched.push((i, n));
        }
    };

    println!("{}", gauss(&sched).ok_or("gauss failed")?);
    println!("from z3 import *");
    println!("(n,) = Ints('n')");
    print!("print(solve(n >= 0");
    for &(i, n) in sched.iter() {
        print!(", n % {} == {}", n, (n as i32 - i as i32) % n as i32);
    }
    println!("))");

    Ok(())
}
