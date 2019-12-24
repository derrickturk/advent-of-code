use std::{
    collections::HashSet,
    io::{self, BufRead},
};

const WORLD_SIZE: usize = 5;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Cell {
    Empty,
    Bug,
}

impl Cell {
    #[inline]
    fn bug_count(&self) -> u8 {
        match self {
            Cell::Empty => 0,
            Cell::Bug => 1,
        }
    }
}

type World = [[Cell; WORLD_SIZE]; WORLD_SIZE];

fn biodiversity(world: &World) -> i32 {
    let mut sum = 0i32;
    for (i, row) in world.iter().enumerate() {
        for (j, cell) in row.iter().enumerate() {
            match cell {
                Cell::Empty => { },
                Cell::Bug => {
                    sum |= 1 << (i * WORLD_SIZE + j);
                },
            };
        }
    }
    sum
}

#[inline]
fn count_adjacent(world: &World, i: usize, j: usize) -> u8 {
    let mut count = 0;

    if i > 0 {
        count += world[i - 1][j].bug_count();
    }

    if i < WORLD_SIZE - 1 {
        count += world[i + 1][j].bug_count();
    }

    if j > 0 {
        count += world[i][j - 1].bug_count();
    }

    if j < WORLD_SIZE - 1 {
        count += world[i][j + 1].bug_count();
    }

    count
}

fn step(world: &World) -> World {
    let mut next_world = world.clone();
    for i in 0..WORLD_SIZE {
        for j in 0..WORLD_SIZE {
            match world[i][j] {
                Cell::Bug => {
                    if count_adjacent(world, i, j) != 1 {
                        next_world[i][j] = Cell::Empty;
                    }
                },

                Cell::Empty => {
                    let count = count_adjacent(world, i, j);
                    if count == 1 || count == 2 {
                        next_world[i][j] = Cell::Bug;
                    }
                },
            }
        }
    }
    next_world
}

fn parse_world(read: &mut impl BufRead) -> Result<Option<World>, io::Error> {
    let mut world = [[Cell::Empty; WORLD_SIZE]; WORLD_SIZE];

    for (i, l) in read.lines().enumerate() {
        let l = l?;

        if i == WORLD_SIZE {
            return Ok(None);
        }

        if l.len() != WORLD_SIZE {
            return Ok(None);
        }

        for (j, c) in l.chars().enumerate() {
            world[i][j] = match c {
                '.' => Cell::Empty,
                '#' => Cell::Bug,
                _ => return Ok(None),
            };
        }
    }

    Ok(Some(world))
}

fn main() -> Result<(), io::Error> {
    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let mut world = parse_world(&mut stdin)?.expect("invalid input");
    let mut seen = HashSet::new();
    seen.insert(biodiversity(&world));
    loop {
        world = step(&world);
        let div = biodiversity(&world);
        if !seen.insert(div) {
            println!("repeat w div = {}", div);
            break;
        }
    }
    Ok(())
}
