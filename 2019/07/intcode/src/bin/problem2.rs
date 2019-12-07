use std::io;

use futures::{
    executor::{block_on, ThreadPool},
    channel::mpsc,
    future::try_join_all,
    stream::StreamExt,
    sink::SinkExt,
    task::{SpawnExt},
};

use permutohedron::LexicalPermutation;

use intcode::*;

const IO_BUF_SZ: usize = 512;

async fn run_with_phases(pool: &ThreadPool, program: &[i32], phases: &[u8]
      ) -> Result<Option<i32>, IntCodeError> {
    let mut exec_handles = Vec::new();

    let (mut pipe_in_send, mut input_recv) = mpsc::channel(IO_BUF_SZ); 
    let mut input_send = pipe_in_send.clone();

    for phase in phases {
        let mut state = ProgramState::from(program.to_vec());
        let (mut output_send, output_recv) = mpsc::channel(IO_BUF_SZ);

        input_send.send(*phase as i32).await
            .expect("unexpected error writing to input channel");
        input_send = output_send.clone();

        exec_handles.push(pool.spawn_with_handle(async move {
            let res = execute(&mut state,
                &mut input_recv, &mut output_send).await;
            drop(output_send);
            res
        }).expect("failed to launch program on ThreadPool"));

        input_recv = output_recv;
    }

    drop(input_send);
    let mut pipe_out_recv = input_recv;

    pipe_in_send.send(0).await
        .expect("unexpected error writing to input channel");

    let output_handle = pool.spawn_with_handle(async move {
        let mut last_output = None;
        while let Some(output) = pipe_out_recv.next().await {
            last_output = Some(output);
            if let Err(_) = pipe_in_send.send(output).await {
                // the head program halted, dropping the receiver -
                //   this means we have seen our last output
                break;
            }
        }
        last_output
    }).expect("failed to launch output feedback on ThreadPool");

    try_join_all(exec_handles).await?;

    Ok(output_handle.await)
}

async fn problem2(pool: &ThreadPool, program: &[i32]
      ) -> Result<(), IntCodeError> {
    let mut phases = [5u8, 6u8, 7u8, 8u8, 9u8];
    let mut max_output = std::i32::MIN;
    let mut max_phases = phases.clone();

    loop {
        if let Some(output) = run_with_phases(pool, program, &phases).await? {
            if output > max_output {
                max_output = output;
                max_phases = phases.clone();
            }
        }

        if !phases.next_permutation() {
            break;
        }
    }

    println!("max output {} @ phases {:?}", max_output, max_phases);

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
    block_on(problem2(&pool, &program[..]))
}
