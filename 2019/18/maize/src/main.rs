use std::{
    error::Error,
    io,
};

use aocrs::dijkstra::cost_to_win;

mod maze;
use maze::*;

fn main() -> Result<(), Box<dyn Error>> {
    let mut stdin = io::stdin().lock();
    let (world, init) = parse_world(&mut stdin)?
      .ok_or("Invalid world description.")?;
    let all = all_keys(&world);

    let moves = cost_to_win(init,
      |s| s.valid_moves(&world).map(|t| (1, t)),
      |t| t.keys == all)
      .ok_or("No path to win!")?;

    println!("{}", moves);

    Ok(())
}
