use std::env;

struct GameState {
    scores: Vec<u8>,
    i1: usize,
    i2: usize,
}

impl GameState {
    fn new() -> Self {
        GameState {
            scores: vec![3, 7],
            i1: 0,
            i2: 1,
        }
    }

    #[inline]
    fn step(&mut self) {
        let combined = self.scores[self.i1] + self.scores[self.i2];
        if combined >= 10 {
            self.scores.push(1);
            self.scores.push(combined - 10);
        } else {
            self.scores.push(combined);
        }

        self.i1 = (self.i1 + 1 + self.scores[self.i1] as usize)
            % self.scores.len();
        self.i2 = (self.i2 + 1 + self.scores[self.i2] as usize)
            % self.scores.len();
    }

    fn run_til_len(&mut self, len: usize) {
        while self.scores.len() < len {
            self.step();
        }
    }

    fn run_til_match(&mut self, needle: &[u8]) -> usize {
        while self.scores.len() < needle.len() + 1 {
            self.step();
        }

        loop {
            let ls = self.scores.len();
            let ln = needle.len();
            if &self.scores[(ls - ln - 1)..(ls - 1)] == needle {
                return ls - ln - 1;
            }
            if &self.scores[(ls - ln)..ls] == needle {
                return ls - ln;
            }
            self.step();
        }
    }
}

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} input", args[0]);
        return;
    }

    // pt 1
    match args[1].parse() {
        Ok(num) => {
            let mut s = GameState::new();
            s.run_til_len(num + 10);
            for i in &s.scores[num..] {
                print!("{}", i);
            }
            println!();
        },

        Err(_) => {
            eprintln!("Invalid input: {}", args[1]);
            return;
        },
    };

    // pt 2
    let needle: Vec<u8> = args[1].chars().map(|c| c as u8 - b'0').collect();
    let mut s = GameState::new();
    let n = s.run_til_match(&needle);
    println!("{}", n);
}
