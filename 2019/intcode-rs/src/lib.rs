use std::{
    marker::Unpin,
};

use futures::{
    stream::{self, Stream},
    sink::Sink,
};

#[derive(Debug, Copy, Clone)]
pub enum IntCodeError {
    ParseError,
    UnknownOpCode(i64, i64),
    UnknownAddrMode(i64, i64),
    InvalidAddress(i64),
    ImmediateWrite,
    OutOfInput,
    OutputError,
}

mod memory;
pub use memory::*;

mod opcode;
pub use opcode::*;

pub mod asm;
pub mod asm_parser;
pub mod disasm;

pub async fn execute<T: ExpandoMemory>(program: &mut ProgramState<T>,
      input: &mut (impl Stream<Item = i64> + Unpin),
      output: &mut (impl Sink<i64> + Unpin)
      ) -> Result<(), IntCodeError> {
    loop {
        let opcode = OpCode::parse_next(program)?;
        match opcode {
            OpCode::Halt => return Ok(()),
            _ => opcode.eval(program, input, output).await?,
        }
    }
}

pub async fn execute_static_io<T: ExpandoMemory>(program: &mut ProgramState<T>,
      input: &[i64]) -> Result<Vec<i64>, IntCodeError> {
    let mut output_buf = Vec::new();
    execute(program, &mut stream::iter(input.iter().cloned()),
          &mut output_buf).await?;
    Ok(output_buf)
}
