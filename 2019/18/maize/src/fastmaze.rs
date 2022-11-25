use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::{self, Display, Formatter},
    io::{self, BufRead},
};

use crate::{
    maze::{self, Cell, KeySet, State, World},
    multimaze,
};

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
pub enum Point {
    Start(u8),
    Key(char),
    Door(char),
}

impl Display for Point {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let c = match self {
            Point::Start(_) => '@',
            Point::Key(c) => *c,
            Point::Door(c) => *c,
        };
        write!(f, "{}", c)
    }
}

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
pub struct FastState {
    pub point: Point,
    pub keys: KeySet,
}

impl FastState {
    pub fn valid_moves<'w>(&self, world: &'w FastWorld
      ) -> impl Iterator<Item=(usize, FastState)> + 'w {
        let keys = self.keys;
        world[&self.point].iter().cloned()
          .filter_map(move |(cost, point)| {
              match point {
                  Point::Key(k) => {
                      let mut new_keys = keys;
                      new_keys.insert(k);
                      Some((cost, FastState { point, keys: new_keys }))
                  },

                  Point::Door(d) if keys.contains(d) => {
                      Some((cost, FastState { point, keys }))
                  },

                  _ => None,
              }
          })
    }
}

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
pub struct FastMultiState {
    pub points: [Point; 4],
    pub keys: KeySet,
}

impl FastMultiState {
    pub fn valid_moves<'w>(&self, world: &'w FastWorld
      ) -> impl Iterator<Item=(usize, FastMultiState)> + 'w {
        let keys = self.keys;
        let points = self.points;
        points.into_iter().enumerate().flat_map(move |(i, point)| {
            let this_guy = FastState { point, keys };
            this_guy.valid_moves(world)
              .map(move |(cost, FastState { point, keys: new_keys })| {
                  let mut new_points = points;
                  new_points[i] = point;
                  (cost, FastMultiState { points: new_points, keys: new_keys })
              })
        })
    }
}

pub type FastWorld = HashMap<Point, Vec<(usize, Point)>>;

pub fn compile_fastworld(world: &World, initial: &State
  ) -> (FastWorld, FastState) {
    let mut fast_world = HashMap::new();
    fast_world.insert(Point::Start(0),
      find_nearest(initial.x, initial.y, world));
    for (&(x, y), c) in world {
        match c {
            Cell::Door(d) => {
                fast_world.insert(Point::Door(*d), find_nearest(x, y, world));
            },
            Cell::Key(k) => {
                fast_world.insert(Point::Key(*k), find_nearest(x, y, world));
            },
            _ => { },
        };
    }
    (fast_world, FastState { point: Point::Start(0), keys: initial.keys })
}

pub fn compile_multify_fastworld(world: World, initial: State
  ) -> (FastWorld, FastMultiState) {
    let (world, initial) = multimaze::multify_world(world, initial);

    let mut fast_world = HashMap::new();

    for (i, &(x, y)) in initial.positions.iter().enumerate() {
        fast_world.insert(Point::Start(i as u8), find_nearest(x, y, &world));
    }

    for (&(x, y), c) in &world {
        match c {
            Cell::Door(d) => {
                fast_world.insert(Point::Door(*d), find_nearest(x, y, &world));
            },
            Cell::Key(k) => {
                fast_world.insert(Point::Key(*k), find_nearest(x, y, &world));
            },
            _ => { },
        };
    }

    let points = [
        Point::Start(0),
        Point::Start(1),
        Point::Start(2),
        Point::Start(3),
    ];

    (fast_world, FastMultiState { points, keys: initial.keys })
}

pub fn parse_fastworld<B: BufRead>(buf: &mut B
  ) -> io::Result<Option<(FastWorld, FastState)>> {
    match maze::parse_world(buf)? {
        Some((world, init)) => Ok(Some(compile_fastworld(&world, &init))),
        None => Ok(None),
    }
}

pub fn all_keys(world: &FastWorld) -> KeySet {
    let mut keys = KeySet::new();
    for point in world.keys() {
        match point {
            Point::Key(k) => keys.insert(*k),
            _ => { },
        };
    }
    keys
}

fn adjacent(x: usize, y: usize) -> [(usize, usize); 4] {
    [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
}

fn find_nearest(x: usize, y: usize, world: &World) -> Vec<(usize, Point)> {
    let mut seen = HashSet::new();
    let mut to_visit = VecDeque::new();
    let mut neighbors: HashMap<Point, usize> = HashMap::new();

    seen.insert((x, y));
    for (new_x, new_y) in adjacent(x, y) {
        to_visit.push_back((new_x, new_y, 1));
    }

    while let Some((x, y, cost)) = to_visit.pop_front() {
        seen.insert((x, y));
        match world.get(&(x, y)) {
            Some(Cell::Door(d)) => {
                neighbors.entry(Point::Door(*d))
                  .and_modify(|old_cost| {
                      *old_cost = (*old_cost).min(cost)
                  })
                  .or_insert(cost);
            },

            Some(Cell::Key(k)) => {
                neighbors.entry(Point::Key(*k))
                  .and_modify(|old_cost| {
                      *old_cost = (*old_cost).min(cost)
                  })
                  .or_insert(cost);
            },

            Some(Cell::Open) => {
                let next = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)];
                for (new_x, new_y) in next {
                    if !seen.contains(&(new_x, new_y)) {
                        to_visit.push_back((new_x, new_y, cost + 1));
                    }
                }
            },

            None => { },
        }
    }

    let mut neighbors: Vec<(usize, Point)> = neighbors.into_iter()
      .map(|(pt, cost)| (cost, pt))
      .collect();
    neighbors.sort_unstable();
    neighbors
}
