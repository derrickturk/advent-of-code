use std::{
    convert::TryInto,
    fmt::Debug,
    marker::Unpin,
};

use futures::{
    stream::{self, Stream, StreamExt},
    sink::{Sink, SinkExt},
};

#[derive(Debug, Copy, Clone)]
pub enum IntCodeError {
    ParseError,
    UnknownOpCode(i64),
    UnknownAddrMode(i64),
    InvalidAddress(i64),
    ImmediateWrite,
    OutOfInput,
    OutputError,
}

pub trait ExpandoMemory: Debug {
    fn from(memory: Vec<i64>) -> Self;
    fn get(&mut self, index: usize) -> &i64;
    fn get_mut(&mut self, index: usize) -> &mut i64;
}

#[derive(Debug, Clone)]
pub struct ExpandoVec(pub Vec<i64>);

impl ExpandoMemory for ExpandoVec {
    fn from(vec: Vec<i64>) -> Self {
        Self(vec)
    }

    fn get(&mut self, index: usize) -> &i64 {
        if index >= self.0.len() {
            self.0.resize(index + 1, 0);
        }
        unsafe { self.0.get_unchecked(index) }
    }

    fn get_mut(&mut self, index: usize) -> &mut i64 {
        if index >= self.0.len() {
            self.0.resize(index + 1, 0);
        }
        unsafe { self.0.get_unchecked_mut(index) }
    }
}

#[derive(Debug, Clone)]
pub struct ProgramState<T: ExpandoMemory> {
    memory: T,
    ip: usize,
    relative_base: i64,
}

impl<T: ExpandoMemory> ProgramState<T> {
    #[inline]
    pub fn from(memory: Vec<i64>) -> Self {
        ProgramState { memory: T::from(memory), ip: 0, relative_base: 0, }
    }

    #[inline]
    pub fn memory(&self) -> &T {
        &self.memory
    }

    #[inline]
    pub fn ip(&self) -> usize {
        self.ip
    }

    #[inline]
    pub fn relative_base(&self) -> i64 {
        self.relative_base
    }
}

#[derive(Debug, Copy, Clone)]
pub enum WriteOperand {
    Position(usize),
    Relative(i64),
}

impl WriteOperand {
    #[inline]
    pub fn fetch<T: ExpandoMemory>(&self, program: &mut ProgramState<T>
          ) -> Result<i64, IntCodeError> {
        match *self {
            WriteOperand::Position(ptr) => Ok(*program.memory.get(ptr)),
            WriteOperand::Relative(offset) => {
                let ptr = program.relative_base + offset;
                let ptr: usize = ptr.try_into()
                    .map_err(|_| IntCodeError::InvalidAddress(ptr))?;
                Ok(*program.memory.get(ptr))
            },
        }
    }

    #[inline]
    pub fn fetch_mut<'a, T: ExpandoMemory>(&self,
        program: &'a mut ProgramState<T>) -> Result<&'a mut i64, IntCodeError> {
        match *self {
            WriteOperand::Position(ptr) => Ok(program.memory.get_mut(ptr)),
            WriteOperand::Relative(offset) => {
                let ptr = program.relative_base + offset;
                let ptr: usize = ptr.try_into()
                    .map_err(|_| IntCodeError::InvalidAddress(ptr))?;
                Ok(program.memory.get_mut(ptr))
            },
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ReadOperand {
    Immediate(i64),
    Memory(WriteOperand),
}

impl ReadOperand {
    #[inline]
    pub fn fetch<T: ExpandoMemory>(&self, program: &mut ProgramState<T>
          ) -> Result<i64, IntCodeError> {
        match *self {
            ReadOperand::Immediate(n) => Ok(n),
            ReadOperand::Memory(op) => op.fetch(program),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum OpCode {
    Add(ReadOperand, ReadOperand, WriteOperand),
    Mul(ReadOperand, ReadOperand, WriteOperand),
    Input(WriteOperand),
    Output(ReadOperand),
    JmpTrue(ReadOperand, ReadOperand),
    JmpFalse(ReadOperand, ReadOperand),
    Less(ReadOperand, ReadOperand, WriteOperand),
    Eql(ReadOperand, ReadOperand, WriteOperand),
    AdjustRelBase(ReadOperand),
    Halt,
}

impl OpCode {
    pub fn parse_next<T: ExpandoMemory>(program: &mut ProgramState<T>
          ) -> Result<OpCode, IntCodeError> {
        let mem = &mut program.memory;
        let ip = program.ip;

        let instr = *mem.get(ip);
        let opcode = instr % 100;
        let addr_modes = instr / 100;

        match opcode {
            1 => {
                Ok(OpCode::Add(
                    read_operand(addr_modes, 0, mem, ip + 1)?,
                    read_operand(addr_modes, 1, mem, ip + 2)?,
                    write_operand(addr_modes, 2, mem, ip + 3)?,
                ))
            },

            2 => {
                Ok(OpCode::Mul(
                    read_operand(addr_modes, 0, mem, ip + 1)?,
                    read_operand(addr_modes, 1, mem, ip + 2)?,
                    write_operand(addr_modes, 2, mem, ip + 3)?,
                ))
            },

            3 => {
                Ok(OpCode::Input(
                    write_operand(addr_modes, 0, mem, ip + 1)?
                ))
            },

            4 => {
                Ok(OpCode::Output(
                    read_operand(addr_modes, 0, mem, ip + 1)?,
                ))
            },

            5 => {
                Ok(OpCode::JmpTrue(
                    read_operand(addr_modes, 0, mem, ip + 1)?,
                    read_operand(addr_modes, 1, mem, ip + 2)?,
                ))
            },

            6 => {
                Ok(OpCode::JmpFalse(
                    read_operand(addr_modes, 0, mem, ip + 1)?,
                    read_operand(addr_modes, 1, mem, ip + 2)?,
                ))
            },

            7 => {
                Ok(OpCode::Less(
                    read_operand(addr_modes, 0, mem, ip + 1)?,
                    read_operand(addr_modes, 1, mem, ip + 2)?,
                    write_operand(addr_modes, 2, mem, ip + 3)?,
                ))
            },

            8 => {
                Ok(OpCode::Eql(
                    read_operand(addr_modes, 0, mem, ip + 1)?,
                    read_operand(addr_modes, 1, mem, ip + 2)?,
                    write_operand(addr_modes, 2, mem, ip + 3)?,
                ))
            },

            9 => {
                Ok(OpCode::AdjustRelBase(
                    read_operand(addr_modes, 0, mem, ip + 1)?,
                ))
            },

            99 => Ok(OpCode::Halt),

            n => Err(IntCodeError::UnknownOpCode(n)),
        }
    }

    pub async fn eval<T: ExpandoMemory>(&self, program: &mut ProgramState<T>,
          input: &mut (impl Stream<Item = i64> + Unpin),
          output: &mut (impl Sink<i64> + Unpin)
          ) -> Result<(), IntCodeError> {
        let mut jmp_ip = None;

        match *self {
            OpCode::Add(src1, src2, dst) => {
                let arg1 = src1.fetch(program)?;
                let arg2 = src2.fetch(program)?;
                let dst = dst.fetch_mut(program)?;
                *dst = arg1 + arg2;
            },

            OpCode::Mul(src1, src2, dst) => {
                let arg1 = src1.fetch(program)?;
                let arg2 = src2.fetch(program)?;
                let dst = dst.fetch_mut(program)?;
                *dst = arg1 * arg2;
            },

            OpCode::Input(dst) => {
                let dst = dst.fetch_mut(program)?;
                *dst = input.next().await.ok_or(IntCodeError::OutOfInput)?;
            },

            OpCode::Output(src) => {
                let val = src.fetch(program)?;
                output.send(val).await.map_err(|_| IntCodeError::OutputError)?;
            },

            OpCode::JmpTrue(cnd, dst) => {
                if cnd.fetch(program)? != 0 {
                    let dst = dst.fetch(program)?;
                    jmp_ip = Some(dst.try_into()
                        .map_err(|_| IntCodeError::InvalidAddress(dst))?);
                }
            },

            OpCode::JmpFalse(cnd, dst) => {
                if cnd.fetch(program)? == 0 {
                    let dst = dst.fetch(program)?;
                    jmp_ip = Some(dst.try_into()
                        .map_err(|_| IntCodeError::InvalidAddress(dst))?);
                }
            },

            OpCode::Less(src1, src2, dst) => {
                let arg1 = src1.fetch(program)?;
                let arg2 = src2.fetch(program)?;
                let dst = dst.fetch_mut(program)?;
                *dst = (arg1 < arg2).into();
            },

            OpCode::Eql(src1, src2, dst) => {
                let arg1 = src1.fetch(program)?;
                let arg2 = src2.fetch(program)?;
                let dst = dst.fetch_mut(program)?;
                *dst = (arg1 == arg2).into();
            },

            OpCode::AdjustRelBase(offset) => {
                let offset = offset.fetch(program)?;
                program.relative_base += offset;
            },

            OpCode::Halt => {},
        }

        if let Some(jmp_ip) = jmp_ip {
            program.ip = jmp_ip;
        } else {
            program.ip += self.ip_step();
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
            OpCode::AdjustRelBase(..) => 2,
            OpCode::Halt => 1,
        }
    }
}

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

#[inline]
fn indirect_address<T: ExpandoMemory>(memory: &mut T, ptr: usize)
  -> Result<usize, IntCodeError> {
    let addr = *memory.get(ptr);
    addr.try_into().map_err(|_| IntCodeError::InvalidAddress(addr))
}

#[inline]
fn read_operand<T: ExpandoMemory>(addr_modes: i64, operand_index: u8,
    memory: &mut T, ptr: usize) -> Result<ReadOperand, IntCodeError> {
    let addr_mode = if operand_index > 0 {
        addr_modes / 10i64.pow(operand_index.into()) % 10
    } else {
        addr_modes % 10
    };

    match addr_mode {
        0 => Ok(ReadOperand::Memory(
                WriteOperand::Position(indirect_address(memory, ptr)?))),
        1 => Ok(ReadOperand::Immediate(*memory.get(ptr))),
        2 => Ok(ReadOperand::Memory(
                WriteOperand::Relative(*memory.get(ptr)))),
        _ => Err(IntCodeError::UnknownAddrMode(addr_mode)),
    }
}

#[inline]
fn write_operand<T: ExpandoMemory>(addr_modes: i64, operand_index: u8,
    memory: &mut T, ptr: usize) -> Result<WriteOperand, IntCodeError> {
    let addr_mode = if operand_index > 0 {
        addr_modes / 10i64.pow(operand_index.into()) % 10
    } else {
        addr_modes % 10
    };

    match addr_mode {
        0 => Ok(WriteOperand::Position(indirect_address(memory, ptr)?)),
        1 => Err(IntCodeError::ImmediateWrite),
        2 => Ok(WriteOperand::Relative(*memory.get(ptr))),
        _ => Err(IntCodeError::UnknownAddrMode(addr_mode)),
    }
}
