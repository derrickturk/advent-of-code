use std::{
    io::{self, Read, BufRead, BufReader},
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
    buf_size: usize,

    #[structopt(short, long)]
    extended_sparse: bool,

    #[structopt(short="s", long)]
    start_input: Option<Vec<i64>>,

    #[structopt(short="f", long, parse(from_os_str))]
    start_input_file: Option<PathBuf>,

    #[structopt(name="FILE", parse(from_os_str))]
    input_file: PathBuf,
}

#[derive(Debug)]
enum Error {
    IntCodeError(IntCodeError),
    IOError(io::Error),
    SpawnError(SpawnError),
    StartInputError,
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
    let (mut input_send, mut input_recv) = mpsc::channel(options.buf_size);
    let (mut output_send, mut output_recv) = mpsc::channel(options.buf_size);

    let exec_handle = if options.extended_sparse {
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

    if let Some(input) = &options.start_input {
        for i in input {
            input_send.send(*i).await.map_err(|_| Error::StartInputError)?;
        }
    }

    if let Some(input_file) = &options.start_input_file {
        let file = File::open(input_file)?;
        for line in BufReader::new(file).lines() {
            let i = line?.trim_end().parse::<i64>()
                .map_err(|_| IntCodeError::ParseError)?;
            input_send.send(i).await.map_err(|_| Error::StartInputError)?;
        }
    }

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
