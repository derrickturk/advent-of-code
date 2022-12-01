use std::{
    io::{BufRead, stdin},
    error::Error,
};

fn main() -> Result<(), Box<dyn Error>> {
    let stdin = stdin().lock();
    let mut this_elf = 0;
    let mut elves = Vec::new();
    for l in stdin.lines() {
        let l = l?;
        if l == "" {
            elves.push(this_elf);
            this_elf = 0;
        } else {
            this_elf += l.parse::<u32>()?;
        }
    }
    elves.push(this_elf);
    elves.sort_unstable_by(|a, b| b.cmp(a));
    println!("{}", elves[0]);
    println!("{}", elves[0] + elves[1] + elves[2]);
    Ok(())
}
