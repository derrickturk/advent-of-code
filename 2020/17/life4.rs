use std::{
    collections::{HashSet, VecDeque},
    error::Error,
    io::{self, BufRead},
};

pub struct World(HashSet<(i64, i64, i64, i64)>);

impl World {
    pub fn new() -> Self {
        Self(HashSet::new())
    }

    pub fn parse<R: BufRead>(r: &mut R) -> io::Result<Option<Self>> {
        let mut world = HashSet::new();
        for (i, l) in r.lines().enumerate() {
            for (j, c) in l?.chars().enumerate() {
                match c {
                    '.' => { },
                    '#' => { world.insert((i as i64, j as i64, 0, 0)); },
                    _ => return Ok(None),
                };
            }
        }
        Ok(Some(Self(world)))
    }

    pub fn step(&mut self) {
        let mut new_world = HashSet::new();
        let mut consider_these: VecDeque<_> = self.0.iter().cloned().collect();
        let mut have_considered = HashSet::new();

        while let Some(cell) = consider_these.pop_front() {
            if have_considered.contains(&cell) {
                continue;
            }

            let live = self.is_live(cell);

            let mut count = 0;
            for n in neighbors(cell) {
                if self.is_live(n) {
                    count += 1;
                }

                if live { // neighbors of live cells need to be considered
                    consider_these.push_back(n);
                }
            }

            if live && (count == 2 || count == 3) {
                new_world.insert(cell);
            } else if !live && count == 3 {
                new_world.insert(cell);
            }

            have_considered.insert(cell);
        }

        self.0 = new_world;
    }

    #[inline]
    pub fn is_live(&self, xyzw: (i64, i64, i64, i64)) -> bool {
        self.0.contains(&xyzw)
    }

    #[inline]
    pub fn live_count(&self) -> usize {
        self.0.len()
    }
}

fn neighbors(xyzw: (i64, i64, i64, i64)
  ) -> impl Iterator<Item=(i64, i64, i64, i64)> {
    let (x, y, z, w) = xyzw;
    (x - 1 ..= x + 1).flat_map(move |i| {
        (y - 1 ..= y + 1).flat_map(move |j| {
            (z - 1 ..= z + 1).flat_map(move |k| {
                (w - 1 ..= w + 1).filter_map(move |l| {
                    if (i, j, k, l) == (x, y, z, w) {
                        None
                    } else {
                        Some((i, j, k, l))
                    }
                })
            })
        })
    })
}

fn main() -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();
    let mut world = World::parse(&mut stdin.lock())?
        .ok_or("bad input")?;

    for _ in 0..6 {
        world.step();
    }

    println!("live count: {}", world.live_count());

    Ok(())
}
