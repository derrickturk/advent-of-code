mod gameboiii {

use std::{
    convert::TryInto,
    error::Error,
    fmt,
};

#[derive(Copy, Clone, Debug)]
pub enum Op {
    Nop,
    Acc,
    Jmp,
}

#[derive(Copy, Clone, Debug)]
pub struct Instr {
    pub op: Op,
    pub val: isize,
}

impl Instr {
    pub fn parse(instr: &str) -> Result<Self, GameBoiiiError> {
        if instr.len() < 5 {
            return Err(GameBoiiiError::ParseError(instr.to_owned()));
        }

        let (op, rest) = instr.split_at(3);
        let op = match op {
            "nop" => Op::Nop,
            "acc" => Op::Acc,
            "jmp" => Op::Jmp,
            _ => return Err(GameBoiiiError::ParseError(instr.to_owned())),
        };

        let val = rest.trim_start().parse::<isize>()
          .map_err(|_| GameBoiiiError::ParseError(instr.to_owned()))?;

        Ok(Self { op, val })
    }
}

#[derive(Clone, Debug)]
pub struct GameBoiii {
    memory: Vec<Instr>,
    ip: usize,
    acc: isize,
}

impl GameBoiii {
    pub fn new(program: Vec<Instr>) -> Self {
        Self {
            memory: program,
            ip: 0,
            acc: 0,
        }
    }

    pub fn ip(&self) -> usize {
        self.ip
    }

    pub fn acc(&self) -> isize {
        self.acc
    }

    /* step the program if possible, returning the address of the
     *   instruction just executed
     */
    pub fn step(&mut self) -> Result<Option<usize>, GameBoiiiError> {
        if self.ip == self.memory.len() {
            Ok(None)
        } else {
            let instr = &self.memory[self.ip];
            let old_ip = self.ip;
            match instr.op {
                Op::Nop => { self.ip += 1 },
                Op::Acc => {
                    self.acc += instr.val;
                    self.ip += 1;
                },
                Op::Jmp => {
                    let dst = self.ip as isize + instr.val;
                    self.ip = dst.try_into()
                      .map_err(|_| GameBoiiiError::SegFault(dst))?;
                },
            }
            Ok(Some(old_ip))
        }
    }
}

#[derive(Clone, Debug)]
pub enum GameBoiiiError {
    ParseError(String),
    SegFault(isize),
}

impl fmt::Display for GameBoiiiError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GameBoiiiError::ParseError(line) =>
                write!(f, "Invalid instruction: {}", line),
            GameBoiiiError::SegFault(ip) =>
                write!(f, "Invalid IP: {}", ip),
        }
    }
}

impl Error for GameBoiiiError { }

}

use std::{
    collections::HashSet,
    error::Error,
    io::{self, BufRead},
};

fn main() -> Result<(), Box<dyn Error>> {
    use gameboiii::*;

    let stdin = io::stdin();
    // I don't know how to combine Box<dyn Error> with ...map()...collect()
    let mut prog = Vec::new();
    for line in stdin.lock().lines() {
        prog.push(Instr::parse(&line?)?);
    }

    let mut console = GameBoiii::new(prog);
    let mut seen_ip = HashSet::new();
    while let Some(ip) = console.step()? {
        seen_ip.insert(ip);
        if seen_ip.contains(&console.ip()) {
            println!("{}", console.acc());
            break;
        }
    }

    Ok(())
}
