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
    struct Elf {
        prev: usize,
        next: usize,
    }

    let mut elves: Vec<_> = (0..n).map(|i| Elf {
        prev: if i == 0 { n - 1 } else { i - 1 },
        next: if i == n - 1 { 0 } else { i + 1 },
    }).collect();

    let mut elim = n / 2;
    let mut m = n;

    loop {
        if m == 1 {
            return elim;
        }

        let next_elim = if m % 2 == 0 {
            elves[elim].next
        } else {
            elves[elves[elim].next].next
        };

        let prev = elves[elim].prev;
        let next = elves[elim].next;
        elves[prev].next = next;
        elves[next].prev = prev;
        m -= 1;

        elim = next_elim;
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
