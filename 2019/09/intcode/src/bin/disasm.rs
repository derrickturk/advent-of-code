use std::io;

use intcode::*;

fn main() -> Result<(), asm::DisAsmError> {
    let mut program = String::new();
    io::stdin().read_line(&mut program)
        .expect("failed to read program from stdin");
    let program: Vec<i64> = program.trim_end().split(',')
        .map(|word| word.parse().map_err(|_| IntCodeError::ParseError))
        .collect::<Result<_, _>>()?;
    asm::disassemble_memory(&mut io::stdout(), &program[..], &asm::DisAsmOpts {
        line_addrs: true,
        labels: true,
    })
}
