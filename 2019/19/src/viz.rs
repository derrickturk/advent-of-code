use std::{
    io,
};

use futures::{
    executor::block_on,
    stream,
};

use intcode::*;

async fn viz(program: Vec<i64>, n: i64) -> Result<(), IntCodeError> {
    for y in 0..n {
        for x in 0..n {
            let mut output = Vec::new();
            let mut state = ProgramState::<ExpandoVec>::from(program.clone());
            execute(&mut state, &mut stream::iter([x, y].iter().cloned()),
                &mut output).await?;
            match &output[..] {
                [1] => print!("#"),
                _ => print!(" "),
            };
        }
        println!();
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
    block_on(viz(program, 1158))
}
