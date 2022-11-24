use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
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

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
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

#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
pub struct State {
    pub x: i32,
    pub y: i32,
    pub keys: KeySet,
}

pub type World = HashMap<(i32, i32), Cell>;
