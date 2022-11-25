use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
    io::{self, BufRead},
};

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Cell {
    Open,
    Key(char),
    Door(char),
}

impl Display for Cell {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let c = match self {
            Cell::Open => '.',
            Cell::Key(c) => *c,
            Cell::Door(c) => *c,
        };
        write!(f, "{}", c)
    }
}

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
pub struct KeySet(u32);

impl KeySet {
    pub fn new() -> Self {
        Self(0)
    }

    pub fn insert(&mut self, mut key: char) {
        if key.is_ascii_alphabetic() {
            key.make_ascii_uppercase();
            self.0 |= 1 << (key as u8 - 'A' as u8);
        }
    }

    pub fn contains(&self, mut key: char) -> bool {
        if key.is_ascii_alphabetic() {
            key.make_ascii_uppercase();
            (self.0 & 1 << (key as u8 - 'A' as u8)) != 0
        } else {
            false
        }
    }

    pub fn iter(&self) -> impl Iterator<Item=char> {
        let map = self.0;
        (0..26).into_iter()
          .filter(move |i| map & (1 << i) != 0)
          .map(|i| ('A' as u8 + i) as char)
    }
}

impl Display for KeySet {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "<")?;
        let mut sep = "";
        for k in self.iter() {
            write!(f, "{}{}", sep, k)?;
            sep = ", ";
        }
        write!(f, ">")
    }
}

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
pub struct State {
    pub x: usize,
    pub y: usize,
    pub keys: KeySet,
}

impl State {
    pub fn neighbor_positions(&self) -> impl Iterator<Item=(usize, usize)> {
        [
            (self.x + 1, self.y),
            (self.x - 1, self.y),
            (self.x, self.y + 1),
            (self.x, self.y - 1),
        ].into_iter()
    }

    pub fn valid_moves<'w>(&self, world: &'w World
      ) -> impl Iterator<Item=State> + 'w {
        let keys = self.keys;
        self.neighbor_positions().filter_map(move |pos@(x, y)| {
            match world.get(&pos) {
                Some(Cell::Open) =>
                    Some(State { x, y, keys }),

                Some(Cell::Door(d)) => {
                    if keys.contains(*d) {
                        Some(State { x, y, keys })
                    } else {
                        None
                    }
                },

                Some(Cell::Key(k)) => {
                    let mut new_keys = keys;
                    new_keys.insert(*k);
                    Some(State { x, y, keys: new_keys })
                },

                None => None,
            }
        })
    }
}

pub type World = HashMap<(usize, usize), Cell>;

pub fn parse_world<B: BufRead>(buf: &mut B
  ) -> io::Result<Option<(World, State)>> {
    let mut world = HashMap::new();
    let mut start = None;
    for (y, l) in buf.lines().enumerate() {
        for (x, c) in l?.as_str().chars().enumerate() {
            let cell = match c {
                '#' => continue,
                '.' => Cell::Open,
                '@' => {
                    start = Some((x, y));
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

    if let Some((x, y)) = start {
        let init = State { x, y, keys: KeySet::new() };
        Ok(Some((world, init)))
    } else {
        Ok(None)
    }
}

pub fn all_keys(world: &World) -> KeySet {
    let mut keys = KeySet::new();
    for cell in world.values() {
        match cell {
            Cell::Key(k) => keys.insert(*k),
            _ => { },
        };
    }
    keys
}
