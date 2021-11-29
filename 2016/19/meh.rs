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

fn elf_circlejerk(n: usize) -> usize {
    let mut curpos: Vec<Option<usize>> = (0..n).map(|i| Some(i)).collect();
    let mut i = 0;
    let mut m = n;
    loop {
        if let Some(pos) = curpos[i] {
            let target_pos = (pos + m / 2) % m;
            if let Some(j) = curpos.iter().position(|&p| p == Some(target_pos)) {
                curpos[j] = None;
                for entry in &mut curpos[j + 1..] {
                    *entry = entry.map(|k| k - 1);
                }
                m -= 1;
            }
        }

        if m == 1 {
            return i;
        }

        i = (i + 1) % n;
    }
}

fn main() {
    match env::args().nth(1) {
        Some(sz) => if let Ok(sz) = sz.parse() {
            dbg!(last_elf_standing(sz) + 1);
            dbg!(elf_circlejerk(sz) + 1);
        }
        _ => {},
    }
}
