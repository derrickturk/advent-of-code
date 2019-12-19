use std::{
    io,
};

use futures::{
    executor::block_on,
    stream,
};

use intcode::*;

async fn problem1(program: Vec<i64>) -> Result<(), IntCodeError> {
    let mut output = Vec::new();
    for x in 0i64..50 {
        for y in 0i64..50 {
            let mut state = ProgramState::<ExpandoVec>::from(program.clone());
            execute(&mut state, &mut stream::iter([x, y].iter().cloned()),
                &mut output).await?;
        }
    }

    println!("count: {}", output.into_iter().sum::<i64>());

    Ok(())
}

fn main() -> Result<(), IntCodeError> {
    let mut program = String::new();
    io::stdin().read_line(&mut program)
        .expect("failed to read program from stdin");
    let program: Vec<i64> = program.trim_end().split(',')
        .map(|word| word.parse().map_err(|_| IntCodeError::ParseError))
        .collect::<Result<_, _>>()?;
    block_on(problem1(program))
}
