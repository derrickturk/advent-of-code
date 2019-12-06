use std::{
    convert::TryInto,
    marker::Unpin,
};

use futures::{
    stream::{Stream, StreamExt},
    sink::{Sink, SinkExt},
};

#[derive(Debug, Copy, Clone)]
pub enum IntCodeError {
    ParseError,
    OutOfProgram,
    UnknownOpCode(i32),
    UnknownAddrMode(i32),
    InvalidAddress(i32),
    OutOfInput,
    OutputError,
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
pub enum Operand {
    Immediate(i32),
    Indirect(usize),
}

impl Operand {
    #[inline]
    pub fn fetch(&self, memory: &[i32]) -> Result<i32, IntCodeError> {
        match *self {
            Operand::Immediate(n) => Ok(n),
            Operand::Indirect(ptr) => Ok(*memory.get(ptr)
                .ok_or(IntCodeError::InvalidAddress(ptr as i32))?),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum OpCode {
    Add(Operand, Operand, usize),
    Mul(Operand, Operand, usize),
    Input(usize),
    Output(Operand),
    JmpTrue(Operand, Operand),
    JmpFalse(Operand, Operand),
    Less(Operand, Operand, usize),
    Eql(Operand, Operand, usize),
    Halt,
}

impl OpCode {
    pub fn parse_next(program: &ProgramState) -> Result<OpCode, IntCodeError> {
        let mem = &program.memory;
        let ip = program.ip;

        let instr = *mem.get(ip).ok_or(IntCodeError::OutOfProgram)?;
        let opcode = instr % 100;
        let addr_modes = instr / 100;

        match opcode {
            1 => {
                Ok(OpCode::Add(
                    operand(addr_modes, 0, mem, ip + 1)?,
                    operand(addr_modes, 1, mem, ip + 2)?,
                    indirect_address(mem, ip + 3)?,
                ))
            },

            2 => {
                Ok(OpCode::Mul(
                    operand(addr_modes, 0, mem, ip + 1)?,
                    operand(addr_modes, 1, mem, ip + 2)?,
                    indirect_address(mem, ip + 3)?,
                ))
            },

            3 => {
                Ok(OpCode::Input(
                    indirect_address(mem, ip + 1)?,
                ))
            },

            4 => {
                Ok(OpCode::Output(
                    operand(addr_modes, 0, mem, ip + 1)?,
                ))
            },

            5 => {
                Ok(OpCode::JmpTrue(
                    operand(addr_modes, 0, mem, ip + 1)?,
                    operand(addr_modes, 1, mem, ip + 2)?,
                ))
            },

            6 => {
                Ok(OpCode::JmpFalse(
                    operand(addr_modes, 0, mem, ip + 1)?,
                    operand(addr_modes, 1, mem, ip + 2)?,
                ))
            },

            7 => {
                Ok(OpCode::Less(
                    operand(addr_modes, 0, mem, ip + 1)?,
                    operand(addr_modes, 1, mem, ip + 2)?,
                    indirect_address(mem, ip + 3)?,
                ))
            },

            8 => {
                Ok(OpCode::Eql(
                    operand(addr_modes, 0, mem, ip + 1)?,
                    operand(addr_modes, 1, mem, ip + 2)?,
                    indirect_address(mem, ip + 3)?,
                ))
            },

            99 => Ok(OpCode::Halt),

            n => Err(IntCodeError::UnknownOpCode(n)),
        }
    }

    pub async fn eval(&self, program: &mut ProgramState,
          input: &mut (impl Stream<Item = i32> + Unpin),
          output: &mut (impl Sink<i32> + Unpin)
          ) -> Result<(), IntCodeError> {
        let mem = &mut program.memory;
        let ip = &mut program.ip;

        let mut jmp_ip = None;

        match *self {
            OpCode::Add(src1, src2, dst) => {
                let arg1 = src1.fetch(mem)?;
                let arg2 = src2.fetch(mem)?;
                let dst = mem.get_mut(dst)
                    .ok_or(IntCodeError::InvalidAddress(dst as i32))?;
                *dst = arg1 + arg2;
            },

            OpCode::Mul(src1, src2, dst) => {
                let arg1 = src1.fetch(mem)?;
                let arg2 = src2.fetch(mem)?;
                let dst = mem.get_mut(dst)
                    .ok_or(IntCodeError::InvalidAddress(dst as i32))?;
                *dst = arg1 * arg2;
            },

            OpCode::Input(dst) => {
                let dst = mem.get_mut(dst)
                    .ok_or(IntCodeError::InvalidAddress(dst as i32))?;
                *dst = input.next().await.ok_or(IntCodeError::OutOfInput)?;
            },

            OpCode::Output(src) => {
                let val = src.fetch(mem)?;
                output.send(val).await.map_err(|_| IntCodeError::OutputError)?;
            },

            OpCode::JmpTrue(cnd, dst) => {
                if cnd.fetch(mem)? != 0 {
                    let dst = dst.fetch(mem)?;
                    jmp_ip = Some(dst.try_into()
                        .map_err(|_| IntCodeError::InvalidAddress(dst))?);
                }
            },

            OpCode::JmpFalse(cnd, dst) => {
                if cnd.fetch(mem)? == 0 {
                    let dst = dst.fetch(mem)?;
                    jmp_ip = Some(dst.try_into()
                        .map_err(|_| IntCodeError::InvalidAddress(dst))?);
                }
            },

            OpCode::Less(src1, src2, dst) => {
                let arg1 = src1.fetch(mem)?;
                let arg2 = src2.fetch(mem)?;
                let dst = mem.get_mut(dst)
                    .ok_or(IntCodeError::InvalidAddress(dst as i32))?;
                *dst = (arg1 < arg2).into();
            },

            OpCode::Eql(src1, src2, dst) => {
                let arg1 = src1.fetch(mem)?;
                let arg2 = src2.fetch(mem)?;
                let dst = mem.get_mut(dst)
                    .ok_or(IntCodeError::InvalidAddress(dst as i32))?;
                *dst = (arg1 == arg2).into();
            },

            OpCode::Halt => {},
        }

        if let Some(jmp_ip) = jmp_ip {
            *ip = jmp_ip;
        } else {
            *ip += self.ip_step();
        }

        Ok(())
    }

    #[inline]
    fn ip_step(&self) -> usize {
        match self {
            OpCode::Add(..) => 4,
            OpCode::Mul(..) => 4,
            OpCode::Input(..) => 2,
            OpCode::Output(..) => 2,
            OpCode::JmpTrue(..) => 3,
            OpCode::JmpFalse(..) => 3,
            OpCode::Less(..) => 4,
            OpCode::Eql(..) => 4,
            OpCode::Halt => 1,
        }
    }
}

pub async fn execute(program: &mut ProgramState,
      input: &mut (impl Stream<Item = i32> + Unpin),
      output: &mut (impl Sink<i32> + Unpin)
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

#[inline]
fn operand(addr_modes: i32, operand_index: usize, memory: &[i32], ptr: usize
      ) -> Result<Operand, IntCodeError> {
    let addr_mode = if operand_index > 0 {
        addr_modes / (10 * operand_index as i32) % 10
    } else {
        addr_modes % 10
    };

    match addr_mode {
        0 => Ok(Operand::Indirect(indirect_address(memory, ptr)?)),
        1 => Ok(Operand::Immediate(
                *memory.get(ptr).ok_or(IntCodeError::OutOfProgram)?)),
        _ => Err(IntCodeError::UnknownAddrMode(addr_mode)),
    }
}
