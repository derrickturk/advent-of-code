use std::io;
use futures::{
    executor::{block_on, ThreadPool},
    channel::mpsc,
    stream::StreamExt,
    sink::SinkExt,
    task::{SpawnExt},
};

use intcode::*;

const IO_BUF_SZ: usize = 512;

async fn problem1(pool: &ThreadPool, program: Vec<i32>
      ) -> Result<(), IntCodeError> {
    let mut state = ProgramState::from(program);
    let (mut input_send, mut input_recv) = mpsc::channel(IO_BUF_SZ);
    let (mut output_send, mut output_recv) = mpsc::channel(IO_BUF_SZ);

    let exec_handle = pool.spawn_with_handle(async move {
        let res = execute(&mut state, &mut input_recv, &mut output_send).await;
        drop(output_send);
        res
    }).expect("failed to spawn task on ThreadPool");

    input_send.send(1).await
        .expect("unexpected error writing to input channel");

    let output_handle = pool.spawn_with_handle(async move {
        while let Some(out) = output_recv.next().await {
            println!("output: {}", out);
        }
    }).expect("failed to spawn task on ThreadPool");

    drop(input_send);
    exec_handle.await?;
    output_handle.await;

    Ok(())
}

fn main() -> Result<(), IntCodeError> {
    let mut program = String::new();
    io::stdin().read_line(&mut program)
        .expect("failed to read program from stdin");
    let pool = ThreadPool::new().expect("failed to launch ThreadPool");
    let program: Vec<i32> = program.trim_end().split(',')
        .map(|word| word.parse().map_err(|_| IntCodeError::ParseError))
        .collect::<Result<_, _>>()?;
    block_on(problem1(&pool, program))
}
