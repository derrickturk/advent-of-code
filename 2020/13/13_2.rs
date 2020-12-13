/*
▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄
██░▄▄▄██░██░██░▄▄▀██░█▀▄████░▄▀▄░█░▄▄▀█▄▄░▄▄██░██░
██░▄▄███░██░██░█████░▄▀█████░█░█░█░▀▀░███░████░▄▄░
██░█████▄▀▀▄██░▀▀▄██░██░████░███░█░██░███░████░██░
▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀

 * first, the iterative solution to the Chinese Remainder Theorem:
 * https://crypto.stanford.edu/pbc/notes/numbertheory/crt.html
 * second, a fast and correct (rare!) modular inverse from my homeboy Knuth;
 * https://www.di-mgt.com.au/euclidean.html
 * there's a maniac out there with some broken Python which mumbles
 *   about the extended Euclidean algorithm - ignore him! (I lost 2 hours
 *   and my hair turned gray.)
 */

use std::{
    error::Error,
    io::{self, Read},
};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Bus {
    X,
    RealBus(u32),
}

fn crt(buses: &[(usize, u32)]) -> Option<u64> {
    let mut result: u64 = 0;
    let moduli_product: u64 = buses.iter().map(|&(_, n)| n as u64).product();
    for &(i, n) in buses {
        let a = ((n as i64 - i as i64) % n as i64) as u64;
        let b = moduli_product / n as u64;
        result += a * b * modinv(b, n as u64)?;
    }
    Some(result % moduli_product)
}

fn modinv(u: u64, v: u64) -> Option<u64> {
    let mut u1 = 1u64;
    let mut u3 = u;
    let mut v1 = 0u64;
    let mut v3 = v;
    let mut t1: u64;
    let mut t3: u64;
    let mut q: u64;
    let mut iter = 1i64;

    while v3 != 0 {
        q = u3 / v3;
        t3 = u3 % v3;
        t1 = u1 + q * v1;
        u1 = v1;
        v1 = t1;
        u3 = v3;
        v3 = t3;
        iter = -iter;
    }

    if u3 != 1 {
        None
    } else if iter < 0 {
        Some(v - u1)
    } else {
        Some(u1)
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

    println!("{}", crt(&sched).ok_or("crt failed")?);

    /* this was a fun approach, but doesn't scale:
    println!("from z3 import *");
    println!("(n,) = Ints('n')");
    print!("print(solve(n >= 0");
    for &(i, n) in sched.iter() {
        print!(", n % {} == {}", n, (n as i32 - i as i32) % n as i32);
    }
    println!("))");
    */

    Ok(())
}
