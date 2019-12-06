use std::io;
use futures::executor::block_on;

use intcode::*;

async fn problem1(mut program: Vec<i32>) -> Result<(), IntCodeError> {
    program[1] = 12;
    program[2] = 2;

    let mut state = ProgramState::from(program);
    execute(&mut state).await?;

    println!("final state: {:?}", state.memory());
    println!("{}", state.memory()[0]);

    Ok(())
}

fn main() -> Result<(), IntCodeError> {
    let mut program = String::new();
    io::stdin().read_line(&mut program)
        .expect("failed to read program from stdin");
    let program: Vec<i32> = program.trim_end().split(',')
        .map(|word| word.parse().map_err(|_| IntCodeError::ParseError))
        .collect::<Result<_, _>>()?;
    block_on(problem1(program))
}
