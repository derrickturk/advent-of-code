use std::{
    collections::{HashSet, VecDeque},
    io,
};

use futures::{
    executor::block_on,
    stream,
};

use intcode::*;

async fn query(program: &[i64], (x, y): (i64, i64)
      ) -> Result<bool, IntCodeError> {
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
async fn ff_square(program: &[i64], x: i64, y: i64, n: i64
      ) -> Result<Option<(i64, i64)>, IntCodeError> {
    if !query(program, (x, y)).await? {
        return Ok(None);
    }

    let mut seen = HashSet::new();
    let mut todo = VecDeque::new();

    seen.insert((x, y));
    todo.push_back((x, y));

    while let Some((x, y)) = todo.pop_front() {
        if seen.contains(&(x - n + 1, y - n + 1))
              && seen.contains(&(x - n + 1, y))
              && seen.contains(&(x, y - n + 1)) {
            return Ok(Some((x, y)));
        }

        let south = (x, y + 1);
        if !seen.contains(&south) && query(program, south).await? {
            seen.insert(south);
            todo.push_back(south);
        }

        let east = (x + 1, y);
        if !seen.contains(&east) && query(program, east).await? {
            seen.insert(east);
            todo.push_back(east);
        }
    }

    Ok(None)
}

async fn problem2(program: &[i64]) -> Result<(), IntCodeError> {
    let pos = ff_square(program, 10, 9, 100).await?;
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
    block_on(problem2(&program[..]))
}
