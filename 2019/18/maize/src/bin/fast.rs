use std::{
    error::Error,
    io,
};

use aocrs::dijkstra::cost_to_win;

use maize::{
    fastmaze::*,
    // multimaze::*,
};

fn main() -> Result<(), Box<dyn Error>> {
    let mut stdin = io::stdin().lock();
    let (world, init) = parse_fastworld(&mut stdin)?
      .ok_or("Invalid world description.")?;
    let all = all_keys(&world);

    /*
    dbg!(world);
    dbg!(init);
    */

    let moves = cost_to_win(init,
      |s| s.valid_moves(&world),
      |t| t.keys == all)
      .ok_or("No path to win!")?;

    println!("{}", moves);

    /*
    let (world, init) = multify_world(world, init);

    let moves = cost_to_win(init,
      |s| s.valid_moves(&world).map(|t| (1, t)),
      |t| t.keys == all)
      .ok_or("No path to win!")?;

    println!("{}", moves);
    */

    Ok(())
}
