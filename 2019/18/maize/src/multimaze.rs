use std::{
    collections::HashMap,
    io::{self, BufRead},
};

use crate::maze::*;

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
pub struct MultiState {
    pub positions: [(usize, usize); 4],
    pub keys: KeySet,
}

impl MultiState {
    pub fn valid_moves<'w>(&self, world: &'w World
      ) -> impl Iterator<Item=MultiState> + 'w {
        let keys = self.keys;
        let positions = self.positions;
        positions.into_iter().enumerate().flat_map(move |(i, (x, y))| {
            let this_guy = State { x, y, keys };
            this_guy.valid_moves(world)
              .map(move |State { x, y, keys: new_keys }| {
                let mut new_positions = positions;
                new_positions[i] = (x, y);
                new_positions.sort_unstable();
                MultiState { positions: new_positions, keys: new_keys }
            })
        })
    }
}

pub fn multify_world(mut world: World, initial: State) -> (World, MultiState) {
    let State { x, y, keys } = initial;
    world.remove(&(x, y));
    world.remove(&(x + 1, y));
    world.remove(&(x - 1, y));
    world.remove(&(x, y + 1));
    world.remove(&(x, y - 1));
    let positions = [
        (x - 1, y - 1),
        (x - 1, y + 1),
        (x + 1, y - 1),
        (x + 1, y + 1), 
    ];
    for p in &positions {
        world.insert(*p, Cell::Open);
    }
    (world, MultiState { positions, keys })
}

pub fn parse_world_multi<B: BufRead>(buf: &mut B
  ) -> io::Result<Option<(World, MultiState)>> {
    let mut world = HashMap::new();
    let mut start = Vec::new();
    for (y, l) in buf.lines().enumerate() {
        for (x, c) in l?.as_str().chars().enumerate() {
            let cell = match c {
                '#' => continue,
                '.' => Cell::Open,
                '@' => {
                    start.push((x, y));
                    continue;
                },
                k if k.is_ascii_lowercase() => Cell::Key(k),
                d if d.is_ascii_uppercase() => Cell::Door(d),
                c => {
                    dbg!(c);
                    return Ok(None);
                },
            };
            world.insert((x, y), cell);
        }
    }

    if start.len() == 4 {
        let mut positions = [start[0], start[1], start[2], start[3]];
        positions.sort_unstable();
        let init = MultiState {
            positions,
            keys: KeySet::new()
        };
        Ok(Some((world, init)))
    } else {
        Ok(None)
    }
}
