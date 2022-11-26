use std::{
    error::Error,
    io,
};

use aocrs::dijkstra::cost_to_win;

mod donut;
use donut::*;

const INIT: Portal = (Side::Outside, ['A', 'A']);
const TINI: Portal = (Side::Outside, ['Z', 'Z']);

fn main() -> Result<(), Box<dyn Error>> {
    let mut stdin = io::stdin().lock();
    let donut = parse_donut(&mut stdin)?
      .ok_or("Invalid donut description.")?;
    let steps = cost_to_win(INIT, |p| {
        if !donut.contains_key(p) {
            dbg!(p);
        }
        donut[p].iter().cloned()
    }, |p| p == &TINI)
      .ok_or("No path to ZZ.")?;
    println!("{}", steps);
    Ok(())
}
