use std::{
    collections::HashMap,
    io::{self, BufRead},
};

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
pub enum Side {
    Inside,
    Outside,
}

pub type Portal = (Side, [char; 2]);
pub type Donut = HashMap<Portal, Vec<Portal>>;

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
pub(crate) enum RawCell {
    Open,
    Letter(char),
}

pub(crate) type RawDonut = HashMap<(usize, usize), RawCell>;

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
pub(crate) enum ProofedCell {
    Open,
    Portal(Portal),
}

pub(crate) type ProofedDonut = HashMap<(usize, usize), ProofedCell>;

pub(crate) fn parse_raw_donut<B: BufRead>(buf: &mut B
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

pub(crate) fn proof_donut(raw: &RawDonut) -> ProofedDonut {
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
