use std::{
    error::Error,
    io::{self, Read},
};

#[derive(Debug, Copy, Clone)]
pub struct Fish([usize; 9]);

impl Fish {
    pub fn parse(spec: &str) -> Option<Fish> {
        let mut fish = [0; 9];
        for n in spec.trim().split(',').map(|s| s.parse::<usize>().ok()) {
            let n = n?;
            fish[n] += 1;
        }
        Some(Fish(fish))
    }

    pub fn step(&mut self) {
        let spawners = self.0[0];
        for i in 0..8 {
            self.0[i] = self.0[i + 1];
        }
        self.0[6] += spawners;
        self.0[8] = spawners;
    }

    pub fn count(&self) -> usize {
        self.0.iter().sum()
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut fish = String::new();
    io::stdin().read_to_string(&mut fish)?;
    let mut fish = Fish::parse(&fish).ok_or("Couldn't parse fish spec.")?;
    let mut fish2 = fish;

    for _ in 0..80 {
        fish.step();
    }

    println!("{}", fish.count());

    for _ in 0..256 {
        fish2.step();
    }

    println!("{}", fish2.count());

    Ok(())
}
