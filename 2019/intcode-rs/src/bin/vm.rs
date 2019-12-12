use std::{
    io::{self, Read},
    fs::File,
    path::PathBuf,
};

use futures::{
    executor::{block_on, ThreadPool},
    channel::mpsc,
    stream::StreamExt,
    sink::SinkExt,
    task::{SpawnError, SpawnExt},
};

use structopt::StructOpt;

use intcode::*;

#[derive(StructOpt, Debug)]
struct Options {
    #[structopt(short, long, default_value="512")]
    io_buf: usize,

    #[structopt(short, long)]
    sparse_ext_mem: bool,

    #[structopt(name="FILE", parse(from_os_str))]
    input_file: PathBuf,
}

#[derive(Debug)]
enum Error {
    IntCodeError(IntCodeError),
    IOError(io::Error),
    SpawnError(SpawnError),
}

impl From<IntCodeError> for Error {
    fn from(other: IntCodeError) -> Self {
        Error::IntCodeError(other)
    }
}

impl From<io::Error> for Error {
    fn from(other: io::Error) -> Self {
        Error::IOError(other)
    }
}

impl From<SpawnError> for Error {
    fn from(other: SpawnError) -> Self {
        Error::SpawnError(other)
    }
}

async fn run_vm(pool: &ThreadPool, program: Vec<i64>, options: &Options,
      ) -> Result<(), Error> {
    let (mut input_send, mut input_recv) = mpsc::channel(options.io_buf);
    let (mut output_send, mut output_recv) = mpsc::channel(options.io_buf);

    let exec_handle = if options.sparse_ext_mem {
        let mut state = ProgramState::<ExpandoSparse>::from(program);
        pool.spawn_with_handle(async move {
            let res = execute(&mut state, &mut input_recv, &mut output_send).await;
            drop(output_send);
            res
        })?
    } else {
        let mut state = ProgramState::<ExpandoVec>::from(program);
        pool.spawn_with_handle(async move {
            let res = execute(&mut state, &mut input_recv, &mut output_send).await;
            drop(output_send);
            res
        })?
    };

    let input_handle = pool.spawn_with_handle(async move {
        let mut line = String::new();
        while io::stdin().read_line(&mut line)? != 0 {
            let input = line.trim_end().parse::<i64>()
                .map_err(|_| IntCodeError::ParseError)?;
            match input_send.send(input).await {
                Ok(_) => { },
                Err(_) => { break; },
            }
            line.clear();
        }
        drop(input_send);
        Ok::<(), Error>(())
    })?;

    let output_handle = pool.spawn_with_handle(async move {
        while let Some(out) = output_recv.next().await {
            println!("{}", out);
        }
    })?;

    exec_handle.await?;
    drop(input_handle);
    output_handle.await;

    Ok(())
}

fn main() -> Result<(), Error> {
    let options = Options::from_args();

    let mut program = String::new();
    File::open(&options.input_file)?.read_to_string(&mut program)?;

    let pool = ThreadPool::new()?;
    let program: Vec<i64> = program.trim_end().split(',')
        .map(|word| word.parse().map_err(|_| IntCodeError::ParseError))
        .collect::<Result<_, _>>()?;

    block_on(run_vm(&pool, program, &options))
}
