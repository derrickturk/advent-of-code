use std::io;

use futures::{
    executor::block_on,
};

use intcode::*;

async fn problem1(program: Vec<i64>) -> Result<(), IntCodeError> {
    let mut state = ProgramState::from(program);
    let output = execute_static_io(&mut state, &[1]).await?;

    for o in output {
        println!("output: {}", o);
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
    block_on(problem1(program))
}
