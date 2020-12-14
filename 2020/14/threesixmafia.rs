use std::{
    error::Error,
    io::{self, Read},
};

mod syntax {
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum Instruction {
        SetMask {
            ones: u64,
            zeros: u64, // only for part 1 - tricky rules change!
            floating: u64, // only for part 2
        },

        StoreMem {
            val: u64,
            addr: u64,
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
        let mut floating = ALL_ZEROS_36; // for part 2
        for i in (0..36).rev() {
            if rest.is_empty() {
                return None;
            }

            let (first, new_rest) = rest.split_at(1);
            match first {
                "X" => { floating |= 1 << i; },
                "0" => { zeros ^= 1 << i; },
                "1" => { ones |= 1 << i; },
                _ => return None,
            };
            rest = new_rest;
        }

        let rest = rest.trim_start_matches(NEWLINES);
        Some((Instruction::SetMask { ones, zeros, floating }, rest))
    }

    pub fn store_mem(input: &str) -> Option<(Instruction, &str)> {
        let rest = input.strip_prefix("mem[")?;
        let close_addr_idx = rest.find(']')?;
        let (addr, rest) = rest.split_at(close_addr_idx);
        let addr = addr.parse::<u64>().ok()?;
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

    pub struct Memory(HashMap<u64, u64>);

    impl Memory {
        pub fn new() -> Self {
            Self(HashMap::new())
        }

        pub fn run_program(&mut self, program: &Program) {
            let mut mask_ones = ALL_ZEROS_36; // to be OR'd
            let mut mask_zeros = ALL_ONES_36; // to be AND'd
            for &inst in program.iter() {
                match inst {
                    Instruction::SetMask { ones, zeros, .. } => {
                        mask_ones = ones;
                        mask_zeros = zeros;
                    },

                    Instruction::StoreMem { val, addr } => {
                        self.0.insert(addr, (val | mask_ones) & mask_zeros);
                    },
                }
            }
        }

        pub fn run_program_v2(&mut self, program: &Program) {
            let mut mask_ones = ALL_ZEROS_36; // to be OR'd
            let mut mask_floating = ALL_ZEROS_36;

            for &inst in program.iter() {
                match inst {
                    Instruction::SetMask { ones, zeros: _, floating } => {
                        mask_ones = ones;
                        mask_floating = floating;
                    },

                    Instruction::StoreMem { val, addr } => {
                        let addr = addr | mask_ones;
                        let mut addrs = Vec::new();
                        floating_addrs_into(addr, mask_floating, &mut addrs);
                        for addr in addrs {
                            self.0.insert(addr, val);
                        }
                    },
                }
            }
        }

        #[inline]
        pub fn total(&self) -> u64 {
            self.0.iter().map(|(_, &x)| x).sum()
        }

        #[inline]
        pub fn dump(&self) {
            for (k, v) in self.0.iter() {
                println!("memory[{}] = {}", k, v);
            }
        }
    }

    fn floating_addrs_into(addr: u64, floating: u64, vec: &mut Vec<u64>) {
        if floating == 0 {
            vec.push(addr);
        }

        for i in (0..36).rev() {
            let i_bit = 1 << i;
            if floating & i_bit != 0 {
                let rest_floating = floating ^ i_bit;
                let high = addr | i_bit;
                let low = addr & !i_bit;
                floating_addrs_into(high, rest_floating, vec);
                floating_addrs_into(low, rest_floating, vec);
                break;
            }
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

    let mut mem = memory::Memory::new();
    mem.run_program_v2(&prog);
    // mem.dump();
    println!("{}", mem.total());

    Ok(())
}
