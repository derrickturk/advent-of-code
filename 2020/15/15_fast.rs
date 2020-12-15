use std::{
    collections::HashMap,
    env,
    error::Error,
    io::{self, Read},
};

struct Game<'a> {
    i: usize,
    last_age: usize,
    last_turn: HashMap<i32, usize>,
    new: bool,
    seed: &'a [i32],
}

impl<'a> Game<'a> {
    fn new(start: &'a [i32]) -> Self {
        Self {
            i: 0,
            last_age: 0,
            last_turn: HashMap::new(),
            new: false,
            seed: start,
        }
    }
}

impl<'a> Iterator for Game<'a> {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        if self.i < self.seed.len() {
            let val = self.seed[self.i];

            if let Some(&turn) = self.last_turn.get(&val) {
                self.last_age = self.i - turn;
                self.new = false;
            } else {
                self.new = true;
            }

            self.last_turn.insert(val, self.i);
            self.i += 1;
            Some(val)
        } else {
            let val = if self.new {
                0
            } else {
               self.last_age as i32
            };

            if let Some(&turn) = self.last_turn.get(&val) {
                self.last_age = self.i - turn;
                self.new = false;
            } else {
                self.new = true;
            }

            self.last_turn.insert(val, self.i);
            self.i += 1;
            Some(val)
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let turn: usize = env::args().nth(1)
      .ok_or("provide turn as argument")?
      .parse()?;

    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let nums: Vec<i32> = input.as_str().trim().split(',')
      .map(|s| s.parse::<i32>())
      .collect::<Result<_, _>>()?;

    println!("{}", Game::new(&nums[..]).nth(turn - 1).unwrap());
    Ok(())
}
