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
        donut[p].iter().cloned()
    }, |p| p == &TINI)
      .ok_or("No path to ZZ.")?;
    println!("{}", steps);

    let steps = cost_to_win((0, INIT), |&(n, p)| {
        donut[&p].iter().cloned()
          .filter_map(move |(cost, q)| {
              if n == 0 {
                  match (p, q) {
                      (_, (Side::Outside, ['Z', 'Z'])) =>
                          Some((cost, (n, q))),
                      ((Side::Outside, ['A', 'A']), (Side::Inside, _)) =>
                          Some((cost, (n, q))),
                      ((Side::Outside, _), _) => None,
                      ((Side::Inside, l), (Side::Outside, m)) if l == m =>
                          Some((cost, (n + 1, q))),
                      ((Side::Inside, _), (Side::Inside, _)) =>
                          Some((cost, (n, q))),
                      (_, (Side::Outside, _)) => None
                  }
              } else {
                  match (p, q) {
                      (_, (Side::Outside, ['A', 'A'])) => None,
                      (_, (Side::Outside, ['Z', 'Z'])) => None,
                      ((Side::Outside, l), (Side::Inside, m)) if l == m =>
                          Some((cost, (n - 1, q))),
                      ((Side::Inside, l), (Side::Outside, m)) if l == m =>
                          Some((cost, (n + 1, q))),
                      _ => Some((cost, (n, q)))
                  }
              }
          })
    }, |&(n, p)| n == 0 && p == TINI)
      .ok_or("No path to 0ZZ")?;
    println!("{}", steps);

    Ok(())
}
