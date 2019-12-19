use std::{
    collections::HashSet,
    io,
    thread,
};

use futures::{
    future::{BoxFuture, FutureExt},
    executor::block_on,
    stream,
};

use intcode::*;

async fn query(program: &[i64], x: i64, y: i64) -> Result<bool, IntCodeError> {
    if x < 0 || y < 0 {
        return Ok(false);
    }
    let mut state = ProgramState::<ExpandoVec>::from(program.to_vec());
    let mut out = Vec::new();
    execute(&mut state, &mut stream::iter([x, y].iter().cloned()),
        &mut out).await?;
    match &out[..] {
        [o] => Ok(*o != 0),
        _ => Err(IntCodeError::OutputError),
    }
}

/* find the bottom right point of an n x n grid inside a flood fill
 * starting at x, y
 * actually it's an odd flood fill: . -> .
 *                                  | \
 *                                  v  \
 *                                  .    .
 */
fn ff_square<'a>(program: &'a [i64], seen: &'a mut HashSet<(i64, i64)>,
      x: i64, y: i64, n: i64
      ) -> BoxFuture<'a, Result<Option<(i64, i64)>, IntCodeError>> {
    async move {
        if seen.contains(&(x, n)) {
            return Ok(None);
        }

        if seen.contains(&(x - n, y - n)) && seen.contains(&(x - n, y))
              && seen.contains(&(x, y - n)) {
            return Ok(Some((x, y)));
        }

        if query(program, x, y).await? {
            seen.insert((x, y));
            if let Some(res) = ff_square(program, seen, x + 1, y, n).await? {
                Ok(Some(res))
            } else if let Some(res) = ff_square(program, seen, x, y + 1, n).await? {
                Ok(Some(res))
            } else if let Some(res) = ff_square(program, seen, x + 1, y + 1, n).await? {
                Ok(Some(res))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }.boxed()
}

async fn problem2(program: &[i64]) -> Result<(), IntCodeError> {
    let mut seen = HashSet::new();
    let pos = ff_square(program, &mut seen, 6, 5, 100).await?;
    for (x, y) in seen {
        println!("saw ({}, {})", x, y);
    };
    if let Some((x, y)) = pos {
        println!("bottom right at ({}, {})", x, y);
        println!("top left at ({}, {})", x - 99, y - 99);
        println!("answer is {}", (x - 99) * 10000 + (y - 99));
    } else {
        eprintln!("no solution found");
    }
    Ok(())
}

fn main() -> Result<(), IntCodeError> {
    let mut program = String::new();
    io::stdin().read_line(&mut program)
        .expect("failed to read program from stdin");
    let program: Vec<i64> = program.trim_end().split(',')
        .map(|word| word.parse().map_err(|_| IntCodeError::ParseError))
        .collect::<Result<_, _>>()?;
    let bigboy = thread::Builder::new()
        .stack_size(1000 * 1024 * 1024)
        .spawn(move || block_on(problem2(&program[..])))
        .expect("failed to launch bigboy thread");
    bigboy.join().expect("failed to join bigboy thread")
}
