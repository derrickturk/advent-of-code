use std::env;

fn last_elf_standing(n: usize) -> usize {
    let mut solvent = vec![true; n];
    let mut i = 0;
    loop {
        if !solvent[i] {
            i = (i + 1) % n;
            continue;
        }

        let mut j = i;
        loop {
            j = (j + 1) % n;

            if i == j { // wrapped around, we have a winner
                return i;
            }

            if solvent[j] {
                solvent[j] = false;
                break;
            }
        }

        i = (i + 1) % n;
    }
}

fn main() {
    match env::args().nth(1) {
        Some(sz) => if let Ok(sz) = sz.parse() {
            dbg!(last_elf_standing(sz) + 1);
        }
        _ => {},
    }
}
