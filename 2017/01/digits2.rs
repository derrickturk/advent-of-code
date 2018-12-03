use std::io;

fn pairs<'a>(xs: &'a [u32]) -> impl Iterator<Item=(u32, u32)> + 'a {
    xs.iter().cloned()
        .zip(xs.iter().cloned().chain(xs.iter().cloned()).skip(xs.len() / 2))
}

fn checksum(xs: &[u32]) -> u32 {
    pairs(xs).filter(|(x, x_next)| x == x_next).map(|(x, _)| x).sum()
}

fn parse(s: &str) -> Option<Vec<u32>> {
    s.chars().map(|c| c.to_digit(10)).collect()
}

fn main() -> io::Result<()> {
    let mut line = String::new();
    io::stdin().read_line(&mut line)?;
    match parse(line.trim()) {
        Some(digits) => println!("{}", checksum(&digits)),
        None => println!("invalid input"),
    };
    Ok(())
}
