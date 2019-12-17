use std::{
    io::{self, BufRead, BufReader},
    fs::File,
    path::PathBuf,
};

use structopt::StructOpt;

use intcode::*;

#[derive(StructOpt, Debug)]
struct Options {
    // as in, strip them
    #[structopt(short, long)]
    line_addrs: bool,

    #[structopt(short, long, parse(from_os_str))]
    output_file: Option<PathBuf>,

    #[structopt(short, long, parse(from_os_str))]
    map_file: Option<PathBuf>,

    #[structopt(name="FILE", parse(from_os_str))]
    input_file: Option<PathBuf>,
}

#[derive(Debug)]
enum Error {
    ParseError(asm_parser::ParseError),
    AsmError(asm::AsmError),
    IOError(io::Error),
}

impl From<asm_parser::ParseError> for Error {
    fn from(other: asm_parser::ParseError) -> Self {
        Error::ParseError(other)
    }
}

impl From<asm::AsmError> for Error {
    fn from(other: asm::AsmError) -> Self {
        Error::AsmError(other)
    }
}

impl From<io::Error> for Error {
    fn from(other: io::Error) -> Self {
        Error::IOError(other)
    }
}

fn main() -> Result<(), Error> {
    let options = Options::from_args();

    let asm: Vec<String> = if let Some(path) = options.input_file {
        let file = File::open(path)?;
        BufReader::new(file).lines().collect::<Result<_,_>>()?
    } else {
        let stdin = io::stdin();
        stdin.lock().lines().collect::<Result<_,_>>()?
    };

    let stmts = if options.line_addrs {
        asm_parser::parse(asm.iter().map(|s| s.as_str().trim_start_matches(
            |c: char| c.is_ascii_digit() || c == '\t')))?
    } else {
        asm_parser::parse(asm.iter().map(|s| s.as_str()))?
    };

    let program = asm::assemble(&stmts[..])?;

    if let Some(path) = options.output_file {
        asm::emit_program(&mut File::create(path)?, &program[..])?;
    } else {
        asm::emit_program(&mut io::stdout(), &program[..])?;
    }

    if let Some(path) = options.map_file {
        map_file::write_map(&mut File::create(path)?,
            &asm::extract_labels(&stmts[..])?)?;
    }

    Ok(())
}
