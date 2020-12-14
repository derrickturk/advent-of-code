use std::{
    error::Error,
    io::{self, Read},
};

mod syntax {
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum Instruction {
        SetMask {
            ones: u64,
            zeros: u64,
        },

        StoreMem {
            val: u64,
            addr: usize,
        },
    }

    pub type Program = Vec<Instruction>;
}

const ALL_ZEROS_36: u64 = 0;
const ALL_ONES_36: u64 = 0b111111111111111111111111111111111111;

mod parse {
    use super::{ALL_ZEROS_36, ALL_ONES_36};
    use syntax::{Instruction, Program};

    const NEWLINES: &[char] = &['\r', '\n'];

    pub fn set_mask(input: &str) -> Option<(Instruction, &str)> {
        let mut rest = input.strip_prefix("mask = ")?;
        let mut ones = ALL_ZEROS_36; // to be OR'd
        let mut zeros = ALL_ONES_36; // to be AND'd
        for i in (0..36).rev() {
            if rest.is_empty() {
                return None;
            }

            let (first, new_rest) = rest.split_at(1);
            match first {
                "X" => { },
                "0" => { zeros ^= 1 << i; },
                "1" => { ones |= 1 << i; },
                _ => return None,
            };
            rest = new_rest;
        }

        let rest = rest.trim_start_matches(NEWLINES);
        Some((Instruction::SetMask { ones, zeros }, rest))
    }

    pub fn store_mem(input: &str) -> Option<(Instruction, &str)> {
        let rest = input.strip_prefix("mem[")?;
        let close_addr_idx = rest.find(']')?;
        let (addr, rest) = rest.split_at(close_addr_idx);
        let addr = addr.parse::<usize>().ok()?;
        let rest = rest.strip_prefix("] = ")?;
        let newline_idx = rest.find(NEWLINES)?;
        let (val, rest) = rest.split_at(newline_idx);
        let val = val.parse::<u64>().ok()?;
        let rest = rest.trim_start_matches(NEWLINES);
        Some((Instruction::StoreMem { val, addr }, rest))
    }

    pub fn program(input: &str) -> Option<(Program, &str)> {
        let mut prog = Vec::new();
        let mut rest = input;
        loop {
            rest = {
                if let Some((inst, rest)) = set_mask(rest) {
                    prog.push(inst);
                    rest
                } else if let Some((inst, rest)) = store_mem(rest) {
                    prog.push(inst);
                    rest
                } else {
                    return Some((prog, rest))
                }
            };
        }
    }

    #[inline]
    pub fn only_program(input: &str) -> Option<Program> {
        if let Some((prog, rest)) = program(input) {
            if rest.is_empty() {
                Some(prog)
            } else {
                None
            }
        } else {
            None
        }
    }
}

mod memory {
    use std::collections::HashMap;

    use super::{ALL_ZEROS_36, ALL_ONES_36};
    use syntax::{Instruction, Program};

    pub struct Memory(HashMap<usize, u64>);

    impl Memory {
        pub fn new() -> Self {
            Self(HashMap::new())
        }

        pub fn run_program(&mut self, program: &Program) {
            let mut mask_ones = ALL_ZEROS_36; // to be OR'd
            let mut mask_zeros = ALL_ONES_36; // to be AND'd
            for &inst in program.iter() {
                match inst {
                    Instruction::SetMask { ones, zeros } => {
                        mask_ones = ones;
                        mask_zeros = zeros;
                    },

                    Instruction::StoreMem { val, addr } => {
                        self.0.insert(addr, (val | mask_ones) & mask_zeros);
                    },
                }
            }
        }

        #[inline]
        pub fn total(&self) -> u64 {
            self.0.iter().map(|(_, &x)| x).sum()
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut prog = String::new();
    io::stdin().read_to_string(&mut prog)?;

    let prog = parse::only_program(&prog).ok_or("Invalid program input.")?;
    let mut mem = memory::Memory::new();
    mem.run_program(&prog);
    println!("{}", mem.total());

    Ok(())
}
