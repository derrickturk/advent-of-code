use std::{
    error::Error,
    io::{self, Read},
};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Bus {
    X,
    RealBus(u32),
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut iter = input.as_str().lines();
    let timestamp = match iter.next() {
        Some(time) => time.parse::<u32>()?,
        None => Err("Invalid input.")?,
    };

    let buses = match iter.next() {
        Some(line) => {
            let mut result = Vec::new();
            for b in line.split(',') {
                result.push(match b {
                    "x" => Bus::X,
                    num => Bus::RealBus(num.parse()?),
                });
            }
            result
        },

        None => Err("Invalid input.")?,
    };

    let earliest = buses.iter().filter_map(|b| {
        match b {
            &Bus::X => None,
            &Bus::RealBus(num) => Some(num),
        }
    }).min_by_key(|num| num - timestamp % num).ok_or("empty schedule")?;

    let wait = earliest - timestamp % earliest;

    println!("{}", earliest * wait);

    Ok(())
}
