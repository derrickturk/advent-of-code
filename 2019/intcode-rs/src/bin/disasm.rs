use std::{
    io::{self, Read, BufReader},
    fs::File,
    path::PathBuf,
};

use structopt::StructOpt;

use intcode::*;

#[derive(StructOpt, Debug)]
struct Options {
    #[structopt(short, long)]
    line_addrs: bool,

    #[structopt(short, long)]
    autolabel: bool,

    #[structopt(short, long, parse(from_os_str))]
    output_file: Option<PathBuf>,

    #[structopt(short, long, parse(from_os_str))]
    map_file: Option<PathBuf>,

    #[structopt(name="FILE", parse(from_os_str))]
    input_file: Option<PathBuf>,
}

#[derive(Debug)]
enum Error {
    DisAsmError(disasm::DisAsmError),
    MapFileError(map_file::MapFileError),
    IOError(io::Error),
}

impl From<disasm::DisAsmError> for Error {
    fn from(other: disasm::DisAsmError) -> Self {
        Error::DisAsmError(other)
    }
}

impl From<map_file::MapFileError> for Error {
    fn from(other: map_file::MapFileError) -> Self {
        Error::MapFileError(other)
    }
}

impl From<io::Error> for Error {
    fn from(other: io::Error) -> Self {
        Error::IOError(other)
    }
}

fn main() -> Result<(), Error> {
    let options = Options::from_args();

    let mut program = String::new();
    if let Some(path) = options.input_file {
        File::open(path)?.read_to_string(&mut program)?;
    } else {
        io::stdin().read_line(&mut program)?;
    }

    let program: Vec<i64> = program.trim_end().split(',')
        .map(|word| word.parse()
            .map_err(|_| disasm::DisAsmError::from(IntCodeError::ParseError)))
        .collect::<Result<_, _>>()?;

    let labels = if let Some(path) = options.map_file {
        Some(map_file::read_map(&mut BufReader::new(File::open(path)?))?)
    } else {
        None
    };

    let disasm_opts = disasm::DisAsmOpts {
        line_addrs: options.line_addrs,
        autolabel: options.autolabel,
        labels: labels,
    };

    if let Some(path) = options.output_file {
        disasm::disassemble_memory(&mut File::create(path)?, &program[..],
          &disasm_opts)?;
    } else {
        disasm::disassemble_memory(&mut io::stdout(), &program[..],
          &disasm_opts)?;
    }

    Ok(())
}
