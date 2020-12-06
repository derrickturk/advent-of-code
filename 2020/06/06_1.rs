use std::{
    error::Error,
    io::{self, BufRead},
};



fn main() -> Result<(), Box<dyn Error>> {
    let mut counts = Vec::new();
    let mut set = 0u32;

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let line = line?;
        if line.is_empty() {
            counts.push(set.count_ones());
            set = 0u32;
            continue;
        }

        for c in line.bytes() {
            if !c.is_ascii_lowercase() {
                return Err(format!("invalid character: {}", c))?;
            }
            set |= 1 << (c - b'a');
        }
    }

    counts.push(set.count_ones());

    println!("{}", counts.iter().sum::<u32>());

    Ok(())
}
