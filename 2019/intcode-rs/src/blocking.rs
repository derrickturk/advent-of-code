use std::convert::TryInto;

use super::{
    IntCodeError,
    opcode::{OpCode, WriteOperand},
    memory::{ExpandoMemory, ProgramState},
};

#[derive(Clone, Debug)]
pub struct Unblocked<T>(pub ProgramState<T>);

#[derive(Clone, Debug)]
pub struct WaitInput<T>(WriteOperand, ProgramState<T>);

#[derive(Clone, Debug)]
pub struct WaitOutput<T>(i64, ProgramState<T>);

#[derive(Clone, Debug)]
pub enum BlockingProgramState<T> {
    Unblocked(Unblocked<T>),
    WaitInput(WaitInput<T>),
    WaitOutput(WaitOutput<T>),
    Halted(ProgramState<T>),
}

impl<T: ExpandoMemory> Unblocked<T> {
    pub fn step(mut self) -> Result<BlockingProgramState<T>, IntCodeError> {
        let program = &mut self.0;
        let opcode = OpCode::parse_next(program)?;
        let mut jmp_ip = None;

        match opcode {
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
                program.ip += opcode.width();
                return Ok(BlockingProgramState::WaitInput(
                  WaitInput(dst, self.0)));
            },

            OpCode::Output(src) => {
                let val = src.fetch(program)?;
                program.ip += opcode.width();
                return Ok(BlockingProgramState::WaitOutput(
                  WaitOutput(val, self.0)));
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

            OpCode::Halt => {
                return Ok(BlockingProgramState::Halted(self.0));
            },
        }

        if let Some(jmp_ip) = jmp_ip {
            program.ip = jmp_ip;
        } else {
            program.ip += opcode.width();
        }

        Ok(BlockingProgramState::Unblocked(self))
    }
}

impl<T: ExpandoMemory> WaitInput<T> {
    pub fn step(self, input: i64) -> Result<Unblocked<T>, IntCodeError> {
        let WaitInput(dst, mut program) = self;
        *(dst.fetch_mut(&mut program)?) = input;
        Ok(Unblocked(program))
    }
}

impl<T: ExpandoMemory> WaitOutput<T> {
    pub fn step(self) -> Result<Unblocked<T>, IntCodeError> {
        let WaitOutput(_, program) = self;
        Ok(Unblocked(program))
    }
}
