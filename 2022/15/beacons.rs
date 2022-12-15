// run pp.awk to preprocess input
use std::{
    collections::HashSet,
    error::Error,
    io::{stdin, BufRead},
};

/*
const PART1_ROW: i32 = 10;
const PART2_MAX: i32 = 20;
*/
const PART1_ROW: i32 = 2000000;
const PART2_MAX: i32 = 4000000;

fn manhattan(a: (i32, i32), b: (i32, i32)) -> u32 {
    (a.0 - b.0).abs() as u32 + (a.1 - b.1).abs() as u32
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Reading {
    sensor: (i32, i32),
    beacon: (i32, i32),
    distance: u32,
}

fn impossible(readings: &[Reading], row: i32) -> usize {
    let mut seen = HashSet::new();
    for r in readings {
        let y_dist = (r.sensor.1 - row).abs() as u32;
        if  y_dist <= r.distance {
            for i in 0..=(r.distance - y_dist) {
                let x_right = r.sensor.0 + i as i32;
                let x_left = r.sensor.0 - i as i32;
                if row != r.beacon.1 {
                    seen.insert(x_left);
                    seen.insert(x_right);
                } else {
                    if x_left != r.beacon.0 {
                        seen.insert(x_left);
                    }
                    if x_right != r.beacon.0 {
                        seen.insert(x_right);
                    }
                }
            }
        }
    }
    seen.len()
}

fn find(readings: &[Reading], max: i32) -> Option<(i32, i32)> {
    for j in 0..=max {
        let mut i = 0;
        loop {
            let mut good = true;

            for r in readings {
                if manhattan(r.sensor, (i, j)) <= r.distance {
                    good = false;
                    let y_dist = (r.sensor.1 - j).abs() as u32;
                    i = r.sensor.0 + (r.distance - y_dist + 1) as i32;
                    break;
                }
            }

            if good {
                return Some((i, j));
            }

            if i > max {
                break;
            }
        }
    }
    None
}

fn parse(line: &str) -> Option<Reading> {
    let mut split = line.split(' ');
    let x0 = split.next()?.parse().ok()?;
    let y0 = split.next()?.parse().ok()?;
    let x1 = split.next()?.parse().ok()?;
    let y1 = split.next()?.parse().ok()?;
    if split.next().is_none() {
        Some(Reading {
            sensor: (x0, y0),
            beacon: (x1, y1),
            distance: manhattan((x0, y0), (x1, y1)),
        })
    } else {
        None
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let stdin = stdin().lock();
    let mut readings: Vec<Reading> = stdin.lines().map(|l| {
        let l = l.ok()?;
        parse(&l)
    }).collect::<Option<Vec<_>>>().ok_or("io fail or bad input or w/e")?;
    readings.sort(); // by (x0, ...)
    println!("{}", impossible(&readings, PART1_ROW));
    if let Some((x, y)) = find(&readings, PART2_MAX) {
        println!("{}, {}, {}", x, y, x as u64 * 4000000 + y as u64);
    } else {
        eprintln!("no part 2 solution found");
    }
    Ok(())
}
