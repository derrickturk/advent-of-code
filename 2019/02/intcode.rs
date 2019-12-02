use std::{
    io,
    convert::TryInto,
};

#[derive(Debug, Copy, Clone)]
enum IntCodeError {
    ParseError,
    OutOfProgram,
    UnknownOpCode(i32),
    InvalidAddress(i32),
}

#[derive(Debug, Copy, Clone)]
enum OpCode {
    Add(usize, usize, usize),
    Mul(usize, usize, usize),
    Halt,
}

#[inline]
fn indirect_address(program: &mut [i32], ptr: usize)
  -> Result<usize, IntCodeError> {
    let addr = *program.get(ptr).ok_or(IntCodeError::OutOfProgram)?;
    addr.try_into().map_err(|_| IntCodeError::InvalidAddress(addr))
}

impl OpCode {
    fn parse(program: &mut [i32], ip: usize)
      -> Result<(OpCode, usize), IntCodeError> {
        match *program.get(ip).ok_or(IntCodeError::OutOfProgram)? {
            1 => {
                let opcode = OpCode::Add(
                    indirect_address(program, ip + 1)?,
                    indirect_address(program, ip + 2)?,
                    indirect_address(program, ip + 3)?,
                );
                Ok((opcode, ip + 4))
            },

            2 => {
                let opcode = OpCode::Mul(
                    indirect_address(program, ip + 1)?,
                    indirect_address(program, ip + 2)?,
                    indirect_address(program, ip + 3)?,
                );
                Ok((opcode, ip + 4))
            },

            99 => Ok((OpCode::Halt, ip)),

            n => Err(IntCodeError::UnknownOpCode(n)),
        }
    }

    fn eval(&self, program: &mut [i32]) -> Result<(), IntCodeError> {
        match *self {
            OpCode::Add(src1, src2, dst) => {
                let arg1 = *program.get(src1)
                    .ok_or(IntCodeError::InvalidAddress(src1 as i32))?;
                let arg2 = *program.get(src2)
                    .ok_or(IntCodeError::InvalidAddress(src2 as i32))?;
                let dst = program.get_mut(dst)
                    .ok_or(IntCodeError::InvalidAddress(dst as i32))?;
                *dst = arg1 + arg2;
            },

            OpCode::Mul(src1, src2, dst) => {
                let arg1 = *program.get(src1)
                    .ok_or(IntCodeError::InvalidAddress(src1 as i32))?;
                let arg2 = *program.get(src2)
                    .ok_or(IntCodeError::InvalidAddress(src2 as i32))?;
                let dst = program.get_mut(dst)
                    .ok_or(IntCodeError::InvalidAddress(dst as i32))?;
                *dst = arg1 * arg2;
            },

            OpCode::Halt => {},
        }

        Ok(())
    }
}

fn execute(program: &mut [i32]) -> Result<(), IntCodeError> {
    let mut ip = 0;
    loop {
        let (opcode, next_ip) = OpCode::parse(program, ip)?;
        ip = next_ip;
        match opcode {
            OpCode::Halt => return Ok(()),
            _ => opcode.eval(program)?,
        }
    }
}

fn problem1(program: &mut [i32]) -> Result<(), IntCodeError> {
    program[1] = 12;
    program[2] = 2;
    execute(program)?;
    println!("final state: {:?}", program);
    Ok(())
}

fn main() -> Result<(), IntCodeError> {
    let mut program = String::new();
    io::stdin().read_line(&mut program)
        .expect("failed to read program from stdin");
    let mut program: Vec<i32> = program.trim_end().split(',')
        .map(|word| word.parse().map_err(|_| IntCodeError::ParseError))
        .collect::<Result<_, _>>()?;

    problem1(&mut program[..])
}
