use std::{
    collections::VecDeque,
    env,
    error::Error,
};

const ROLLS: [(u8, u64); 7] = [
    (3, 1),
    (4, 3),
    (5, 6),
    (6, 7),
    (7, 6),
    (8, 3),
    (9, 1),
];

fn play(i1: u8, i2: u8) -> (u64, u64) {
    let mut w1 = 0;
    let mut w2 = 0;
    let mut to_play = VecDeque::from([(i1, 0, i2, 0, true, 1)]);
    while let Some((i1, s1, i2, s2, p1, mul)) = to_play.pop_front() {
        for (r, next_mult) in ROLLS {
            let mul = mul * next_mult;
            if p1 {
                let i1 = (i1 - 1 + r) % 10 + 1;
                let s1 = s1 + i1;
                if s1 >= 21 {
                    w1 += mul;
                } else {
                    to_play.push_back((i1, s1, i2, s2, false, mul));
                }
            } else {
                let i2 = (i2 - 1 + r) % 10 + 1;
                let s2 = s2 + i2;
                if s2 >= 21 {
                    w2 += mul;
                } else {
                    to_play.push_back((i1, s1, i2, s2, true, mul));
                }
            }
        }
    }

    (w1, w2)
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = env::args();
    let _ = args.next();
    let i1 = args.next().ok_or("nope")?.parse::<u8>()?;
    let i2 = args.next().ok_or("nope")?.parse::<u8>()?;

    dbg!(play(i1, i2));

    Ok(())
}
