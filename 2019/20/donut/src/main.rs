use std::{
    error::Error,
    io,
};

mod donut;
use donut::*;

fn main() -> Result<(), Box<dyn Error>> {
    let mut stdin = io::stdin().lock();
    let donut = parse_raw_donut(&mut stdin)?
      .ok_or("Invalid donut description.")?;
    let donut = proof_donut(&donut);
    dbg!(donut);
    Ok(())
}
