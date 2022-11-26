use std::{
    collections::{HashMap, HashSet, VecDeque},
    io::{self, BufRead},
};

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
pub enum Side {
    Inside,
    Outside,
}

impl Side {
    pub fn opposite(&self) -> Self {
        match self {
            Side::Inside => Side::Outside,
            Side::Outside => Side::Inside,
        }
    }
}

pub type Portal = (Side, [char; 2]);
pub type Donut = HashMap<Portal, Vec<(usize, Portal)>>;

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
enum RawCell {
    Open,
    Letter(char),
}

type RawDonut = HashMap<(usize, usize), RawCell>;

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
enum ProofedCell {
    Open,
    Portal(Portal),
}

type ProofedDonut = HashMap<(usize, usize), ProofedCell>;

pub fn parse_donut<B: BufRead>(buf: &mut B) -> io::Result<Option<Donut>> {
    if let Some(donut) = parse_raw_donut(buf)? {
        Ok(Some(bake_donut(&proof_donut(&donut))))
    } else {
        Ok(None)
    }
}

fn parse_raw_donut<B: BufRead>(buf: &mut B
  ) -> io::Result<Option<RawDonut>> {
    let mut donut = HashMap::new();
    for (y, l) in buf.lines().enumerate() {
        for (x, c) in l?.chars().enumerate() {
            donut.insert((x, y), match c {
                '#' | ' ' => continue,
                '.' => RawCell::Open,
                c if c.is_ascii_uppercase() => RawCell::Letter(c),
                _ => return Ok(None),
            });
        }
    }
    Ok(Some(donut))
}

fn proof_donut(raw: &RawDonut) -> ProofedDonut {
    let min_x = raw.keys().map(|(x, _)| *x).min().unwrap_or(0);
    let max_x = raw.keys().map(|(x, _)| *x).max().unwrap_or(0);
    let min_y = raw.keys().map(|(_, y)| *y).min().unwrap_or(0);
    let max_y = raw.keys().map(|(_, y)| *y).max().unwrap_or(0);

    let mut donut: ProofedDonut = HashMap::new();
    for (k@&(x, y), &v) in raw {
        match v {
            RawCell::Open => {
                donut.entry(*k).or_insert(ProofedCell::Open);
            },

            // only do anything if we're the guy next to the .
            RawCell::Letter(l) => {
                if x != min_x && x != max_x {
                    match (raw.get(&(x - 1, y)), raw.get(&(x + 1, y))) {
                        // KL.
                        (Some(&RawCell::Letter(k)), Some(&RawCell::Open)) => {
                            let side = if (x - 1) == min_x {
                                Side::Outside
                            } else {
                                Side::Inside
                            };
                            donut.insert((x + 1, y),
                              ProofedCell::Portal((side, [k, l])));
                            continue;
                        }

                        // .LM
                        (Some(&RawCell::Open), Some(&RawCell::Letter(m))) => {
                            let side = if (x + 1) == max_x {
                                Side::Outside
                            } else {
                                Side::Inside
                            };
                            donut.insert((x - 1, y),
                              ProofedCell::Portal((side, [l, m])));
                            continue;
                        },

                        _ => { },
                    }
                }

                if y != min_y && y != max_y {
                    match (raw.get(&(x, y - 1)), raw.get(&(x, y + 1))) {
                        // KL.
                        (Some(&RawCell::Letter(k)), Some(&RawCell::Open)) => {
                            let side = if (y - 1) == min_y {
                                Side::Outside
                            } else {
                                Side::Inside
                            };
                            donut.insert((x, y + 1),
                              ProofedCell::Portal((side, [k, l])));
                        }

                        // .LM
                        (Some(&RawCell::Open), Some(&RawCell::Letter(m))) => {
                            let side = if (y + 1) == max_y {
                                Side::Outside
                            } else {
                                Side::Inside
                            };

                            donut.insert((x, y - 1),
                              ProofedCell::Portal((side, [l, m])));
                        },

                        _ => { },
                    }
                }
            },
        }
    }
    donut
}

fn bake_donut(proofed: &ProofedDonut) -> Donut {
    let mut donut = HashMap::new();
    for (&(x, y), p) in proofed {
        match p {
            &ProofedCell::Portal(p) => {
                donut.insert(p, find_neighbors(x, y, &proofed));
            },
            _ => { },
        };
    }

    let mut to_portalize = Vec::new();
    for p@&(side, lbl) in donut.keys() {
        let oppo = (side.opposite(), lbl);
        if donut.contains_key(&oppo) {
            to_portalize.push((*p, oppo));
        }
    }

    for (from, to) in to_portalize {
        donut.get_mut(&from).unwrap().insert(0, (1, to));
    }

    donut
}

fn adjacent(x: usize, y: usize) -> [(usize, usize); 4] {
    [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
}

fn find_neighbors(x: usize, y: usize, proofed: &ProofedDonut
  ) -> Vec<(usize, Portal)> {
    let mut seen = HashSet::new();
    let mut to_visit = VecDeque::new();
    let mut neighbors: HashMap<Portal, usize> = HashMap::new();

    seen.insert((x, y));
    for (new_x, new_y) in adjacent(x, y) {
        to_visit.push_back((new_x, new_y, 1));
    }

    while let Some((x, y, cost)) = to_visit.pop_front() {
        seen.insert((x, y));
        match proofed.get(&(x, y)) {
            Some(&ProofedCell::Portal(p)) => {
                neighbors.entry(p).and_modify(|old_cost| {
                    *old_cost = (*old_cost).min(cost);
                })
                .or_insert(cost);
            },

            Some(ProofedCell::Open) => {
                for (new_x, new_y) in adjacent(x, y) {
                    if !seen.contains(&(new_x, new_y)) {
                        to_visit.push_back((new_x, new_y, cost + 1));
                    }
                }
            },

            None => { },
        }
    }

    let mut neighbors: Vec<(usize, Portal)> = neighbors.into_iter()
      .map(|(p, cost)| (cost, p))
      .collect();
    neighbors.sort_unstable();
    neighbors
}
