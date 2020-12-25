use std::{
    env,
    error::Error,
};

fn transform(subject: usize, loop_size: usize) -> usize {
    let mut value = 1;
    for _ in 0..loop_size {
        value *= subject;
        value %= 20201227;
    }
    value
}

/*
t = (((1 * 7 % 20201227) * 7 % 20201227) * 7 % 20201227) ..N

pubkey = (1 * 7 % 20201227)
pubkey = 1 * 7 - ((1 * 7) / 20201227)

pubkey = (1 * 7 % 20201227) * 7 % 20201227
pubkey = (1 * 7 % 20201227) * 7 - ((1 * 7) / 20201227)

pubkey = ((1 * 7 % 20201227) * 7 % 20201227) * 7 % 20201227
pubkey = 
*/

fn find_loop_size(pubkey: usize) -> usize {
    let mut loop_size = 0;
    let mut value = 1;
    loop {
        if value == pubkey {
            return loop_size;
        }

        value *= 7;
        value %= 20201227;

        loop_size += 1;
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let card_pubkey = env::args().nth(1)
      .ok_or("missing card pubkey")?
      .parse()?;
    let door_pubkey = env::args().nth(2)
      .ok_or("missing door pubkey")?
      .parse()?;

    let card_loopsize = find_loop_size(card_pubkey);
    let door_loopsize = find_loop_size(door_pubkey);

    println!("{}, {}", card_loopsize, door_loopsize);
    println!("{}", transform(door_pubkey, card_loopsize));
    Ok(())
}
