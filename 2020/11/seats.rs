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

type World = HashMap<(usize, usize), Seat>;

fn parse(description: &str) -> Option<World> {
    let mut world = HashMap::new();
    for (i, line) in description.lines().enumerate() {
        for (j, c) in line.chars().enumerate() {
            match c {
                '.' => { },
                'L' => { world.insert((i, j), Seat::Empty); },
                '#' => { world.insert((i, j), Seat::Occupied); },
                _ => return None,
            };
        }
    }
    Some(world) // ain't it, kid?
}

fn occupied_adjacent(x: usize, y: usize, world: &World) -> usize {
    let mut count = 0;

    for i in x.saturating_sub(1)..=(x + 1) {
        for j in y.saturating_sub(1)..=(y + 1) {
            if i == x && j == y {
                continue;
            }

            if let Some(Seat::Occupied) = world.get(&(i, j)) {
                count += 1;
            }
        }
    }
    count
}

// return new world and # of changes
fn step(world: &World) -> (World, usize) {
    let mut new_world = HashMap::new();
    let mut changes = 0;
    for (&(x, y), &state) in world.iter() {
        let new_state = match state {
            Seat::Empty => {
                if occupied_adjacent(x, y, world) == 0 {
                    changes += 1;
                    Seat::Occupied
                } else {
                    Seat::Empty
                }
            },

            Seat::Occupied => {
                if occupied_adjacent(x, y, world) >= 4 {
                    changes += 1;
                    Seat::Empty
                } else {
                    Seat::Occupied
                }
            },
        };
        new_world.insert((x, y), new_state);
    }
    (new_world, changes)
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut desc = String::new();
    io::stdin().read_to_string(&mut desc)?;
    let mut world = parse(&desc).ok_or("invalid input")?;

    let mut last_changes = 1;
    while last_changes > 0 {
        let (new_world, changes) = step(&world);
        world = new_world;
        last_changes = changes;
    }

    let occupied = world.values().filter(|&&s| s == Seat::Occupied).count();
    println!("{}", occupied);

    Ok(())
}
