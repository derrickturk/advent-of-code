use std::{
    error::Error,
    io,
};

mod maze;
use maze::*;

fn main() -> Result<(), Box<dyn Error>> {
    let mut ks = KeySet::new();
    ks.insert('a');
    ks.insert('c');
    ks.insert('z');
    println!("{}", ks);
    dbg!(ks.contains('C'));

    let mut stdin = io::stdin().lock();
    let world = parse_world(&mut stdin)?
      .ok_or("Invalid world description.")?;
    dbg!(world);

    Ok(())
}
