use std::{
    io,
    env,
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
    fn parse(program: &mut [i32], ip: usize) -> Result<OpCode, IntCodeError> {
        match *program.get(ip).ok_or(IntCodeError::OutOfProgram)? {
            1 => {
                Ok(OpCode::Add(
                    indirect_address(program, ip + 1)?,
                    indirect_address(program, ip + 2)?,
                    indirect_address(program, ip + 3)?,
                ))
            },

            2 => {
                Ok(OpCode::Mul(
                    indirect_address(program, ip + 1)?,
                    indirect_address(program, ip + 2)?,
                    indirect_address(program, ip + 3)?,
                ))
            },

            99 => Ok(OpCode::Halt),

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

    fn ip_step(&self) -> usize {
        match self {
            OpCode::Add(..) => 4,
            OpCode::Mul(..) => 4,
            OpCode::Halt => 1,
        }
    }
}

fn execute(program: &mut [i32]) -> Result<i32, IntCodeError> {
    let mut ip = 0;
    loop {
        let opcode = OpCode::parse(program, ip)?;
        match opcode {
            OpCode::Halt => return Ok(program[0]),
            _ => opcode.eval(program)?,
        }
        ip += opcode.ip_step();
    }
}

fn problem1(program: &mut [i32]) -> Result<(), IntCodeError> {
    program[1] = 12;
    program[2] = 2;
    let output = execute(program)?;
    println!("final state: {:?}", program);
    println!("{}", output);
    Ok(())
}

fn problem2(program: &[i32]) {
    for noun in 0..=99 {
        for verb in 0..=99 {
            let mut this_prog = program.to_owned();
            this_prog[1] = noun;
            this_prog[2] = verb;

            if let Ok(19690720) = execute(&mut this_prog[..]) {
                println!("{}", 100 * noun + verb);
                return;
            }
        }
    }
    eprintln!("no solution found.");
}

fn main() -> Result<(), IntCodeError> {
    if let Some(prob) = env::args().skip(1).next() {
        let mut program = String::new();
        io::stdin().read_line(&mut program)
            .expect("failed to read program from stdin");
        let mut program: Vec<i32> = program.trim_end().split(',')
            .map(|word| word.parse().map_err(|_| IntCodeError::ParseError))
            .collect::<Result<_, _>>()?;

        match prob.as_str() {
            "1" => problem1(&mut program[..]),
            "2" => Ok(problem2(&program[..])),
            _ => {
                eprintln!("Usage: intcode 1|2 <input.txt");
                Ok(())
            }
        }
    } else {
        eprintln!("Usage: intcode 1|2 <input.txt");
        Ok(())
    }
}
