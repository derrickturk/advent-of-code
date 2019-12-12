use std::{
    io::{self, Read},
    fs::File,
    path::PathBuf,
};

use structopt::StructOpt;

use intcode::*;

#[derive(StructOpt, Debug)]
struct Options {
    #[structopt(short, long, parse(from_os_str))]
    output_file: Option<PathBuf>,

    #[structopt(name="FILE", parse(from_os_str))]
    input_file: Option<PathBuf>,
}

#[derive(Debug)]
enum Error {
    AsmError(asm::AsmError),
    IOError(io::Error),
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

    /*
    let mut program = String::new();
    if let Some(path) = options.input_file {
        File::open(path)?.read_to_string(&mut program)?;
    } else {
        io::stdin().read_line(&mut program)?;
    }

    let program: Vec<i64> = program.trim_end().split(',')
        .map(|word| word.parse()
            .map_err(|_| asm::AsmError::from(IntCodeError::ParseError)))
        .collect::<Result<_, _>>()?;
    */

    let program = asm::assemble(&[
        asm::Labeled {
            item: asm::Stmt::Instr(asm::Instruction::Add, vec![
                asm::Labeled {
                    item: asm::Operand::Immediate(asm::Word::Number(3)),
                    label: None,
                },
                asm::Labeled {
                    item: asm::Operand::Immediate(asm::Word::Number(7)),
                    label: None,
                },
                asm::Labeled {
                    item: asm::Operand::Position(asm::Word::Label(String::from("begin"))),
                    label: None,
                },
            ]),
            label: Some((0usize, String::from("begin"))),
        },
    ])?;

    if let Some(path) = options.output_file {
        asm::emit_program(&mut File::create(path)?, &program[..])?;
    } else {
        asm::emit_program(&mut io::stdout(), &program[..])?;
    }

    Ok(())
}
