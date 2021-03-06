use std::{
    error::Error,
    io::{self, BufRead},
};

fn main() -> Result<(), Box<dyn Error>> {
    let mut sum = 0u32;

    let mut all_set = !0u32;
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let line = line?;
        if line.is_empty() {
            sum += all_set.count_ones();
            all_set = !0u32;
            continue;
        }

        let mut set = 0u32;
        for c in line.bytes() {
            if !c.is_ascii_lowercase() {
                return Err(format!("invalid character: {}", c))?;
            }
            set |= 1 << (c - b'a');
        }
        all_set &= set;
    }

    sum += all_set.count_ones();
    println!("{}", sum);

    Ok(())
}
