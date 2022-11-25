use std::{
    error::Error,
    io,
};

use aocrs::dijkstra::cost_to_win;

use maize::{
    maze::parse_world,
    fastmaze::*,
};

fn main() -> Result<(), Box<dyn Error>> {
    let mut stdin = io::stdin().lock();
    let (world, init) = parse_world(&mut stdin)?
      .ok_or("Invalid world description.")?;
    let (fastworld, fastinit) = compile_fastworld(&world, &init);
    let all = all_keys(&fastworld);

    let moves = cost_to_win(fastinit,
      |s| s.valid_moves(&fastworld),
      |t| t.keys == all)
      .ok_or("No path to win!")?;

    println!("{}", moves);

    let (fastworld, fastinit) = compile_multify_fastworld(world, init);

    let moves = cost_to_win(fastinit,
      |s| s.valid_moves(&fastworld),
      |t| t.keys == all)
      .ok_or("No path to win!")?;

    println!("{}", moves);

    Ok(())
}
