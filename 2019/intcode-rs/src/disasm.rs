use std::{
    collections::hash_map::HashMap,
    convert::TryInto,
    io::{self, Write},
};

use super::{
    IntCodeError,
    ReadOperand,
    WriteOperand,
    OpCode,
    ProgramState,
    ExpandoVec,
};

pub type LabelMap = HashMap<usize, String>;

#[derive(Debug)]
pub enum DisAsmError {
    IntCodeError(IntCodeError),
    IOError(io::Error),
}

impl From<IntCodeError> for DisAsmError {
    fn from(other: IntCodeError) -> Self {
        DisAsmError::IntCodeError(other)
    }
}

impl From<io::Error> for DisAsmError {
    fn from(other: io::Error) -> Self {
        DisAsmError::IOError(other)
    }
}

trait DisAsm {
    fn disassemble_at(&self, writer: &mut impl Write,
          labels: &LabelMap, ip: usize) -> Result<(), DisAsmError>;
}

impl DisAsm for WriteOperand {
    fn disassemble_at(&self, writer: &mut impl Write,
          labels: &LabelMap, ip: usize) -> Result<(), DisAsmError> {
        if let Some(lbl) = labels.get(&ip) {
            write!(writer, "{}: ", lbl)?;
        }

        match *self {
            WriteOperand::Position(ptr) => match labels.get(&ptr) {
                None => write!(writer, "{}", ptr)?,
                Some(lbl) => write!(writer, "{}", lbl)?,
            }
            WriteOperand::Relative(offset) => write!(writer, "({})", offset)?,
        };

        Ok(())
    }
}

impl DisAsm for ReadOperand {
    fn disassemble_at(&self, writer: &mut impl Write,
          labels: &LabelMap, ip: usize) -> Result<(), DisAsmError> {
        match *self {
            ReadOperand::Immediate(n) => {
                if let Some(lbl) = labels.get(&ip) {
                    write!(writer, "{}: ", lbl)?;
                }

                write!(writer, "${}", n)?;
                Ok(())
            },

            ReadOperand::Memory(op) => op.disassemble_at(writer, labels, ip),
        }
    }
}

impl DisAsm for OpCode {
    fn disassemble_at(&self, writer: &mut impl Write,
          labels: &LabelMap, ip: usize) -> Result<(), DisAsmError> {
        if let Some(lbl) = labels.get(&ip) {
            write!(writer, "{}: ", lbl)?;
        }

        match self {
            OpCode::Add(src1, src2, dst) => {
                write!(writer, "add ")?;
                src1.disassemble_at(writer, labels, ip + 1)?;
                write!(writer, ", ")?;
                src2.disassemble_at(writer, labels, ip + 2)?;
                write!(writer, ", ")?;
                dst.disassemble_at(writer, labels, ip + 3)?;
            },

            OpCode::Mul(src1, src2, dst) => {
                write!(writer, "mul ")?;
                src1.disassemble_at(writer, labels, ip + 1)?;
                write!(writer, ", ")?;
                src2.disassemble_at(writer, labels, ip + 2)?;
                write!(writer, ", ")?;
                dst.disassemble_at(writer, labels, ip + 3)?;
            },

            OpCode::Input(dst) => {
                write!(writer, "in ")?;
                dst.disassemble_at(writer, labels, ip + 1)?;
            },

            OpCode::Output(src) => {
                write!(writer, "out ")?;
                src.disassemble_at(writer, labels, ip + 1)?;
            },

            OpCode::JmpTrue(cnd, dst) => {
                write!(writer, "jnz ")?;
                cnd.disassemble_at(writer, labels, ip + 1)?;
                write!(writer, ", ")?;
                disassemble_jmp_dst_at(&dst, writer, labels, ip + 2)?;
            },

            OpCode::JmpFalse(cnd, dst) => {
                write!(writer, "jz ")?;
                cnd.disassemble_at(writer, labels, ip + 1)?;
                write!(writer, ", ")?;
                disassemble_jmp_dst_at(&dst, writer, labels, ip + 2)?;
            },

            OpCode::Less(src1, src2, dst) => {
                write!(writer, "lt ")?;
                src1.disassemble_at(writer, labels, ip + 1)?;
                write!(writer, ", ")?;
                src2.disassemble_at(writer, labels, ip + 2)?;
                write!(writer, ", ")?;
                dst.disassemble_at(writer, labels, ip + 3)?;
            },

            OpCode::Eql(src1, src2, dst) => {
                write!(writer, "eq ")?;
                src1.disassemble_at(writer, labels, ip + 1)?;
                write!(writer, ", ")?;
                src2.disassemble_at(writer, labels, ip + 2)?;
                write!(writer, ", ")?;
                dst.disassemble_at(writer, labels, ip + 3)?;
            },

            OpCode::AdjustRelBase(offset) => {
                write!(writer, "rel ")?;
                offset.disassemble_at(writer, labels, ip + 1)?;
            },

            OpCode::Halt => {
                write!(writer, "hlt")?;
            }
        };

        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
pub struct DisAsmOpts {
    pub line_addrs: bool,
    pub labels: bool,
}

impl Default for DisAsmOpts {
    fn default() -> Self {
        Self {
            line_addrs: false,
            labels: false,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum DisAsmStmt {
    OpCode(OpCode),
    Value(i64),
}

impl DisAsm for DisAsmStmt {
    fn disassemble_at(&self, writer: &mut impl Write,
          labels: &LabelMap, ip: usize) -> Result<(), DisAsmError> {
        match self {
            DisAsmStmt::OpCode(opcode) => {
                opcode.disassemble_at(writer, labels, ip)?;
            },

            DisAsmStmt::Value(n) => {
                if let Some(lbl) = &labels.get(&ip) {
                    write!(writer, "{}: ", lbl)?;
                }
                write!(writer, "${}", n)?;
            },
        }

        Ok(())
    }
}

pub struct MemoryParse {
    pub stmts: Vec<(usize, DisAsmStmt)>,
    pub original_size: usize,
    pub labels: LabelMap,
}

pub fn parse_memory(memory: &[i64], opts: &DisAsmOpts
      ) -> Result<MemoryParse, IntCodeError> {
    let mut state = ProgramState::<ExpandoVec>::from(memory.to_vec());
    let original_size = state.memory.0.len();

    let mut stmts = Vec::new();
    let mut labels = HashMap::new();

    while state.ip < original_size {
        let opcode = OpCode::parse_next(&mut state);
        match opcode {
            Ok(opcode) => {
                if opts.labels {
                    add_labels(&mut labels, &opcode);
                }
                stmts.push((state.ip, DisAsmStmt::OpCode(opcode)));
                state.ip += opcode.width();
            },

            Err(IntCodeError::UnknownOpCode(_, instr)) => {
                stmts.push((state.ip, DisAsmStmt::Value(instr)));
                state.ip += 1;
            },

            Err(e) => return Err(e),
        };
    }

    labels.retain(|ptr, _| *ptr < original_size);

    Ok(MemoryParse { stmts, original_size, labels })
}

pub fn disassemble_memory(writer: &mut impl Write, memory: &[i64],
      opts: &DisAsmOpts) -> Result<(), DisAsmError> {
    let parsed = parse_memory(memory, opts)?;
    for (ip, stmt) in &parsed.stmts {
        if opts.line_addrs {
            write!(writer, "{}\t", ip)?;
        }

        stmt.disassemble_at(writer, &parsed.labels, *ip)?;
        writeln!(writer)?;
    }

    Ok(())
}

fn add_labels(labels: &mut LabelMap, opcode: &OpCode) {
    match opcode {
        OpCode::Add(src1, src2, dst) => {
            add_roperand_label(labels, src1);
            add_roperand_label(labels, src2);
            add_woperand_label(labels, dst);
        },

        OpCode::Mul(src1, src2, dst) => {
            add_roperand_label(labels, src1);
            add_roperand_label(labels, src2);
            add_woperand_label(labels, dst);
        },

        OpCode::Input(dst) => {
            add_woperand_label(labels, dst);
        },

        OpCode::Output(src) => {
            add_roperand_label(labels, src);
        },

        OpCode::JmpTrue(cnd, dst) => {
            add_roperand_label(labels, cnd);
            add_jmp_dst_label(labels, dst);
        },

        OpCode::JmpFalse(cnd, dst) => {
            add_roperand_label(labels, cnd);
            add_jmp_dst_label(labels, dst);
        },

        OpCode::Less(src1, src2, dst) => {
            add_roperand_label(labels, src1);
            add_roperand_label(labels, src2);
            add_woperand_label(labels, dst);
        },

        OpCode::Eql(src1, src2, dst) => {
            add_roperand_label(labels, src1);
            add_roperand_label(labels, src2);
            add_woperand_label(labels, dst);
        },

        OpCode::AdjustRelBase(offset) => {
            add_roperand_label(labels, offset);
        },

        OpCode::Halt => { },
    };
}

#[inline]
fn add_roperand_label(labels: &mut LabelMap, param: &ReadOperand) {
    if let ReadOperand::Memory(WriteOperand::Position(n)) = *param {
        labels.entry(n).or_insert_with(|| format!("lbl{}", n));
    }
}

#[inline]
fn add_woperand_label(labels: &mut LabelMap, param: &WriteOperand) {
    if let WriteOperand::Position(n) = *param {
        labels.entry(n).or_insert_with(|| format!("lbl{}", n));
    }
}

#[inline]
fn add_jmp_dst_label(labels: &mut LabelMap, param: &ReadOperand) {
    if let ReadOperand::Memory(WriteOperand::Position(n)) = *param {
        labels.entry(n).or_insert_with(|| format!("lbl{}", n));
    } else if let ReadOperand::Immediate(n) = *param {
        if let Ok(n) = n.try_into() {
            labels.entry(n).or_insert_with(|| format!("lbl{}", n));
        }
    }
}

fn disassemble_jmp_dst_at(op: &ReadOperand, writer: &mut impl Write,
      labels: &LabelMap, ip: usize) -> Result<(), DisAsmError> {
    match *op {
        ReadOperand::Immediate(n) => {
            if let Some(lbl) = labels.get(&ip) {
                write!(writer, "{}: ", lbl)?;
            }

            if let Ok(n) = n.try_into() {
                if let Some(lbl) = labels.get(&n) {
                    write!(writer, "${}", lbl)?;
                } else {
                    write!(writer, "${}", n)?;
                }
            } else {
                write!(writer, "${}", n)?;
            }

            Ok(())
        },

        ReadOperand::Memory(op) => op.disassemble_at(writer, labels, ip),
    }
}
