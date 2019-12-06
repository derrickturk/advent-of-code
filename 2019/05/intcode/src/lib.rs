use std::{
    convert::TryInto,
};

use futures::{
    stream::{Stream},
    sink::{Sink},
};

#[derive(Debug, Copy, Clone)]
pub enum IntCodeError {
    ParseError,
    OutOfProgram,
    UnknownOpCode(i32),
    InvalidAddress(i32),
}

#[derive(Debug, Clone)]
pub struct ProgramState {
    memory: Vec<i32>,
    ip: usize,
}

impl ProgramState {
    #[inline]
    pub fn from(memory: Vec<i32>) -> Self {
        ProgramState { memory, ip: 0 }
    }

    #[inline]
    pub fn memory(&self) -> &[i32] {
        &self.memory[..]
    }

    #[inline]
    pub fn ip(&self) -> usize {
        self.ip
    }
}

#[derive(Debug, Copy, Clone)]
pub enum OpCode {
    Add(usize, usize, usize),
    Mul(usize, usize, usize),
    Halt,
}

impl OpCode {
    pub fn parse_next(program: &ProgramState) -> Result<OpCode, IntCodeError> {
        let mem = &program.memory;
        let ip = program.ip;

        match *mem.get(ip).ok_or(IntCodeError::OutOfProgram)? {
            1 => {
                Ok(OpCode::Add(
                    indirect_address(mem, ip + 1)?,
                    indirect_address(mem, ip + 2)?,
                    indirect_address(mem, ip + 3)?,
                ))
            },

            2 => {
                Ok(OpCode::Mul(
                    indirect_address(mem, ip + 1)?,
                    indirect_address(mem, ip + 2)?,
                    indirect_address(mem, ip + 3)?,
                ))
            },

            99 => Ok(OpCode::Halt),

            n => Err(IntCodeError::UnknownOpCode(n)),
        }
    }

    pub async fn eval(&self, program: &mut ProgramState,
          input: &mut impl Stream<Item = i32>,
          output: &mut impl Sink<i32>
          ) -> Result<(), IntCodeError> {
        let mem = &mut program.memory;
        let ip = &mut program.ip;

        match *self {
            OpCode::Add(src1, src2, dst) => {
                let arg1 = *mem.get(src1)
                    .ok_or(IntCodeError::InvalidAddress(src1 as i32))?;
                let arg2 = *mem.get(src2)
                    .ok_or(IntCodeError::InvalidAddress(src2 as i32))?;
                let dst = mem.get_mut(dst)
                    .ok_or(IntCodeError::InvalidAddress(dst as i32))?;
                *dst = arg1 + arg2;
            },

            OpCode::Mul(src1, src2, dst) => {
                let arg1 = *mem.get(src1)
                    .ok_or(IntCodeError::InvalidAddress(src1 as i32))?;
                let arg2 = *mem.get(src2)
                    .ok_or(IntCodeError::InvalidAddress(src2 as i32))?;
                let dst = mem.get_mut(dst)
                    .ok_or(IntCodeError::InvalidAddress(dst as i32))?;
                *dst = arg1 * arg2;
            },

            OpCode::Halt => {},
        }

        *ip += self.ip_step();

        Ok(())
    }

    fn ip_step(&self) -> usize {
        match self {
            OpCode::Add(..) => 4,
            OpCode::Mul(..) => 4,
            OpCode::Halt => 1,
        }
    }
}

pub async fn execute(program: &mut ProgramState,
      input: &mut impl Stream<Item = i32>,
      output: &mut impl Sink<i32>
      ) -> Result<(), IntCodeError> {
    loop {
        let opcode = OpCode::parse_next(program)?;
        match opcode {
            OpCode::Halt => return Ok(()),
            _ => opcode.eval(program, input, output).await?,
        }
    }
}

#[inline]
fn indirect_address(memory: &[i32], ptr: usize)
  -> Result<usize, IntCodeError> {
    let addr = *memory.get(ptr).ok_or(IntCodeError::OutOfProgram)?;
    addr.try_into().map_err(|_| IntCodeError::InvalidAddress(addr))
}
