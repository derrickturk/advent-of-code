use std::{
    collections::HashMap,
    error::Error,
    io::{self, Read},
};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Seat {
    Empty,
    Occupied,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Direction { N, S, E, W, NW, NE, SW, SE }

const CORNERS_OF_THE_EARTH: &[Direction] = &[
    Direction::N,
    Direction::S,
    Direction::E,
    Direction::W,
    Direction::NW,
    Direction::NE,
    Direction::SW,
    Direction::SE,
];

struct World {
    world: HashMap<(usize, usize), Seat>,
    rows: usize,
    columns: usize,
}

impl World {
    pub fn parse(description: &str) -> Option<World> {
        let mut world = HashMap::new();
        let mut rows = 0;
        let mut columns = 0;
        for (i, line) in description.lines().enumerate() {
            for (j, c) in line.chars().enumerate() {
                match c {
                    '.' => { },
                    'L' => { world.insert((i, j), Seat::Empty); },
                    '#' => { world.insert((i, j), Seat::Occupied); },
                    _ => return None,
                };
                columns = columns.max(j + 1);
            }
            rows = rows.max(i + 1);
        }
        Some(World { world, rows, columns })
    }

    // return new world and # of changes
    pub fn step(&self) -> (World, usize) {
        let mut new_world = HashMap::new();
        let mut changes = 0;
        for (&(x, y), &state) in self.world.iter() {
            let new_state = match state {
                Seat::Empty => {
                    if self.occupied_visible(x, y) == 0 {
                        changes += 1;
                        Seat::Occupied
                    } else {
                        Seat::Empty
                    }
                },

                Seat::Occupied => {
                    if self.occupied_visible(x, y) >= 5 {
                        changes += 1;
                        Seat::Empty
                    } else {
                        Seat::Occupied
                    }
                },
            };
            new_world.insert((x, y), new_state);
        }
        (World {
            world: new_world,
            rows: self.rows,
            columns: self.columns
        }, changes)
    }

    pub fn occupied(&self) -> usize {
        self.world.values().filter(|&&s| s == Seat::Occupied).count()
    }

    pub fn dump(&self) {
        for i in 0..self.rows {
            for j in 0..self.columns {
                let c = match self.world.get(&(i, j)) {
                    Some(&Seat::Empty) => 'L',
                    Some(&Seat::Occupied) => '#',
                    None => '.',
                };
                print!("{}", c);
            }
            println!();
        }
    }

    fn occupied_visible(&self, x: usize, y: usize) -> usize {
        let mut count = 0;
        for &dir in CORNERS_OF_THE_EARTH {
            let (mut i, mut j) = (x, y);
            while let Some((new_i, new_j)) = self.next_in_direction(i, j, dir) {
                if let Some(&seat) = self.world.get(&(new_i, new_j)) {
                    if seat == Seat::Occupied {
                        count += 1;
                    }
                    // either way, break - it's first seat, not first occupied!
                    break;
                }
                i = new_i;
                j = new_j;
            }
        }
        count
    }

    fn next_in_direction(&self, x: usize, y: usize, dir: Direction
      ) -> Option<(usize, usize)> {
        match dir {
            Direction::N => {
                if x != 0 {
                    Some((x - 1, y))
                } else {
                    None
                }
            },

            Direction::S => {
                if x != self.rows - 1 {
                    Some((x + 1, y))
                } else {
                    None
                }
            },

            Direction::E => {
                if y != self.columns - 1 {
                    Some((x, y + 1))
                } else {
                    None
                }
            },

            Direction::W => {
                if y != 0 {
                    Some((x, y - 1))
                } else {
                    None
                }
            },

            Direction::NW => {
                if x != 0 && y != 0 {
                    Some((x - 1, y - 1))
                } else {
                    None
                }
            },

            Direction::NE => {
                if x != 0 && y != self.columns - 1 {
                    Some((x - 1, y + 1))
                } else {
                    None
                }
            },

            Direction::SW => {
                if x != self.rows - 1 && y != 0 {
                    Some((x + 1, y - 1))
                } else {
                    None
                }
            },

            Direction::SE => {
                if x != self.rows - 1 && y != self.columns - 1 {
                    Some((x + 1, y + 1))
                } else {
                    None
                }
            },
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut desc = String::new();
    io::stdin().read_to_string(&mut desc)?;
    let mut world = World::parse(&desc).ok_or("invalid input")?;

    let mut last_changes = 1;
    while last_changes > 0 {
        println!();
        world.dump();
        let (new_world, changes) = world.step();
        world = new_world;
        last_changes = changes;
    }

    println!("{}", world.occupied());

    Ok(())
}
