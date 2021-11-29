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

// equations x = an (mod mn) => (a, m)
pub fn crt(eqns: &[(u64, u64)]) -> Option<u64> {
    let mut result: u64 = 0;
    let moduli_product: u64 = eqns.iter().map(|&(_, m)| m).product();
    for &(a, m) in eqns {
        let b = moduli_product / m;
        result += a * b * modinv(b, m)?;
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
    let mut odd_iter = false;

    while v3 != 0 {
        q = u3 / v3;
        t3 = u3 % v3;
        t1 = u1 + q * v1;
        u1 = v1;
        v1 = t1;
        u3 = v3;
        v3 = t3;
        odd_iter = !odd_iter;
    }

    if u3 != 1 {
        None
    } else if odd_iter {
        Some(v - u1)
    } else {
        Some(u1)
    }
}
