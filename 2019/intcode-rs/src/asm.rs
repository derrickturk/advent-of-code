use std::{
    collections::hash_map::HashMap,
    convert::TryInto,
    io::{self, Write},
};

use super::{
    OpCode,
    ReadOperand,
    WriteOperand,
};

pub type LabelMap<'a> = HashMap<&'a str, usize>;

#[derive(Debug)]
pub enum AsmError {
    ImmediateWrite,
    WrongOperandCount(Instruction, usize, u8),
    AddressTooLarge(usize),
    InvalidAddress(i64),
    UnknownLabel(String),
    LabelAlreadyDefined(String),
    IOError(io::Error),
    AscipTooLong(usize),
}

impl From<io::Error> for AsmError {
    fn from(other: io::Error) -> Self {
        AsmError::IOError(other)
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Instruction {
    Add,
    Mul,
    Input,
    Output,
    JmpTrue,
    JmpFalse,
    Less,
    Eql,
    AdjustRelBase,
    Halt,
}

#[derive(Debug, Clone)]
pub struct Labeled<T> {
    pub item: T,
    pub label: Option<(usize, String)>,
}

#[derive(Debug, Clone)]
pub enum Word {
    Label(String),
    Number(i64),
}

#[derive(Debug, Clone)]
pub enum Operand {
    Position(Word),
    Relative(i64),
    Immediate(Word),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Instr(Instruction, Vec<Labeled<Operand>>),
    Immediate(Word),
    Ascii(Vec<u8>),
    Asciz(Vec<u8>),
    Ascip(Vec<u8>),
    Zeroes(usize),
}

#[derive(Debug, Clone)]
pub enum AsmItem {
    OpCode(OpCode),
    Value(i64),
}

impl AsmItem {
    #[inline]
    pub fn emit(&self, memory: &mut Vec<i64>) {
        match self {
            AsmItem::OpCode(opcode) => memory.append(&mut opcode.emit()),
            AsmItem::Value(n) => memory.push(*n),
        };
    }
}

#[inline]
pub fn assemble(program: &[Labeled<Stmt>]) -> Result<Vec<AsmItem>, AsmError> {
    let labels = extract_labels(program)?;
    assemble_with_labels(program, &labels)
}

pub fn assemble_with_labels(program: &[Labeled<Stmt>], labels: &LabelMap
      ) -> Result<Vec<AsmItem>, AsmError> {
    let mut items = Vec::new();
    for stmt in program {
        match &stmt.item {
            Stmt::Instr(instr, operands) => {
                let item = match instr {
                    Instruction::Add => {
                        match &operands[..] {
                            [src1, src2, dst] => {
                                AsmItem::OpCode(OpCode::Add(
                                    assemble_roperand(src1, labels)?,
                                    assemble_roperand(src2, labels)?,
                                    assemble_woperand(dst, labels)?,
                                ))
                            },
                            _ => return Err(AsmError::WrongOperandCount(
                                    *instr, operands.len(), 3)),
                        }
                    },

                    Instruction::Mul => {
                        match &operands[..] {
                            [src1, src2, dst] => {
                                AsmItem::OpCode(OpCode::Mul(
                                    assemble_roperand(src1, labels)?,
                                    assemble_roperand(src2, labels)?,
                                    assemble_woperand(dst, labels)?,
                                ))
                            },
                            _ => return Err(AsmError::WrongOperandCount(
                                    *instr, operands.len(), 3)),
                        }
                    },

                    Instruction::Input => {
                        match &operands[..] {
                            [dst] => {
                                AsmItem::OpCode(OpCode::Input(
                                    assemble_woperand(dst, labels)?,
                                ))
                            },
                            _ => return Err(AsmError::WrongOperandCount(
                                    *instr, operands.len(), 1)),
                        }
                    },

                    Instruction::Output => {
                        match &operands[..] {
                            [dst] => {
                                AsmItem::OpCode(OpCode::Output(
                                    assemble_roperand(dst, labels)?,
                                ))
                            },
                            _ => return Err(AsmError::WrongOperandCount(
                                    *instr, operands.len(), 1)),
                        }
                    },

                    Instruction::JmpTrue => {
                        match &operands[..] {
                            [cnd, dst] => {
                                AsmItem::OpCode(OpCode::JmpTrue(
                                    assemble_roperand(cnd, labels)?,
                                    assemble_roperand(dst, labels)?,
                                ))
                            },
                            _ => return Err(AsmError::WrongOperandCount(
                                    *instr, operands.len(), 2)),
                        }
                    },

                    Instruction::JmpFalse => {
                        match &operands[..] {
                            [cnd, dst] => {
                                AsmItem::OpCode(OpCode::JmpFalse(
                                    assemble_roperand(cnd, labels)?,
                                    assemble_roperand(dst, labels)?,
                                ))
                            },
                            _ => return Err(AsmError::WrongOperandCount(
                                    *instr, operands.len(), 2)),
                        }
                    },

                    Instruction::Less => {
                        match &operands[..] {
                            [src1, src2, dst] => {
                                AsmItem::OpCode(OpCode::Less(
                                    assemble_roperand(src1, labels)?,
                                    assemble_roperand(src2, labels)?,
                                    assemble_woperand(dst, labels)?,
                                ))
                            },
                            _ => return Err(AsmError::WrongOperandCount(
                                    *instr, operands.len(), 3)),
                        }
                    },

                    Instruction::Eql => {
                        match &operands[..] {
                            [src1, src2, dst] => {
                                AsmItem::OpCode(OpCode::Eql(
                                    assemble_roperand(src1, labels)?,
                                    assemble_roperand(src2, labels)?,
                                    assemble_woperand(dst, labels)?,
                                ))
                            },
                            _ => return Err(AsmError::WrongOperandCount(
                                    *instr, operands.len(), 3)),
                        }
                    },

                    Instruction::AdjustRelBase => {
                        match &operands[..] {
                            [offset] => {
                                AsmItem::OpCode(OpCode::AdjustRelBase(
                                    assemble_roperand(offset, labels)?,
                                ))
                            },
                            _ => return Err(AsmError::WrongOperandCount(
                                    *instr, operands.len(), 3)),
                        }
                    },

                    Instruction::Halt => {
                        if operands.is_empty() {
                            AsmItem::OpCode(OpCode::Halt)
                        } else {
                            return Err(AsmError::WrongOperandCount(
                                    *instr, operands.len(), 0));
                        }
                    },
                };
                items.push(item);
            },

            Stmt::Immediate(Word::Number(n)) => {
                items.push(AsmItem::Value(*n));
            },

            Stmt::Immediate(Word::Label(lbl)) => {
                let ptr = *labels.get(lbl.as_str())
                    .ok_or(AsmError::UnknownLabel(lbl.clone()))?;
                let ptr: i64 = ptr.try_into()
                    .map_err(|_| AsmError::AddressTooLarge(ptr))?;
                items.push(AsmItem::Value(ptr));
            },

            Stmt::Ascii(text) => {
                items.extend(text.iter().map(|c| AsmItem::Value(*c as i64)));
            },

            Stmt::Asciz(text) => {
                items.extend(text.iter().map(|c| AsmItem::Value(*c as i64)));
                items.push(AsmItem::Value(0));
            },

            Stmt::Ascip(text) => {
                let size = text.len();
                let size = size.try_into()
                    .map_err(|_| AsmError::AscipTooLong(size))?;
                items.push(AsmItem::Value(size));
                items.extend(text.iter().map(|c| AsmItem::Value(*c as i64)));
            },

            Stmt::Zeroes(count) => {
                items.extend((0..*count).map(|_| AsmItem::Value(0)));
            },
        }
    }
    Ok(items)
}

#[inline]
pub fn program_values(items: &[AsmItem]) -> Vec<i64> {
    let mut memory = Vec::new();
    for item in items {
        item.emit(&mut memory);
    }
    memory
}

#[inline]
pub fn emit_program(writer: &mut impl Write, items: &[AsmItem]
      ) -> io::Result<()> {
    let values = program_values(items);
    let mut sep = "";
    for v in values {
        write!(writer, "{}{}", sep, v)?;
        sep = ",";
    }
    writeln!(writer)
}

pub fn extract_labels(program: &[Labeled<Stmt>]) -> Result<LabelMap, AsmError> {
    let mut labels = HashMap::new();
    for stmt in program {
        if let Some((ptr, lbl)) = &stmt.label {
            if let Some(_) = labels.insert(lbl.as_str(), *ptr) {
                return Err(AsmError::LabelAlreadyDefined(lbl.clone()));
            }
        }

        if let Stmt::Instr(_, operands) = &stmt.item {
            for op in operands {
                if let Some((ptr, lbl)) = &op.label {
                    if let Some(_) = labels.insert(lbl.as_str(), *ptr) {
                        return Err(AsmError::LabelAlreadyDefined(lbl.clone()));
                    }
                }
            }
        }
    }

    Ok(labels)
}

fn assemble_roperand(op: &Labeled<Operand>, labels: &LabelMap
      ) -> Result<ReadOperand, AsmError> {
    match &op.item {
        Operand::Immediate(Word::Number(n)) => Ok(ReadOperand::Immediate(*n)),
        Operand::Immediate(Word::Label(lbl)) => {
            let ptr = *labels.get(lbl.as_str())
                .ok_or(AsmError::UnknownLabel(lbl.clone()))?;
            let ptr: i64 = ptr.try_into()
                .map_err(|_| AsmError::AddressTooLarge(ptr))?;
            Ok(ReadOperand::Immediate(ptr))
        },
        _ => Ok(ReadOperand::Memory(assemble_woperand(op, labels)?)),
    }
}

fn assemble_woperand(op: &Labeled<Operand>, labels: &LabelMap
      ) -> Result<WriteOperand, AsmError> {
    match &op.item {
        Operand::Immediate(_) => Err(AsmError::ImmediateWrite),
        Operand::Position(Word::Number(n)) => {
            let n = *n;
            let ptr = n.try_into()
                .map_err(|_| AsmError::InvalidAddress(n))?;
            Ok(WriteOperand::Position(ptr))
        }
        Operand::Position(Word::Label(lbl)) => {
            let ptr = *labels.get(lbl.as_str())
                .ok_or(AsmError::UnknownLabel(lbl.clone()))?;
            Ok(WriteOperand::Position(ptr))
        },
        Operand::Relative(n) => Ok(WriteOperand::Relative(*n)),
    }
}
