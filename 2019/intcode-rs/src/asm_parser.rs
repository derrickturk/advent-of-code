use regex::{
    Regex,
};

use lazy_static::lazy_static;

use super::asm::{
    Instruction,
    Labeled,
    Word,
    Operand,
    Stmt,
};

#[derive(Debug, Copy, Clone)]
pub enum ParseErrorKind {
    ExpectedInstructionOrWord,
    ExpectedStmt,
    ExpectedOperand,
    ExpectedOperandOrLabel,
}

#[derive(Debug, Copy, Clone)]
pub struct ParseError {
    kind: ParseErrorKind,
    line: usize,
    offset: usize,
}

pub type ParseResult<T> = Result<T, ParseError>;

pub fn parse<'a>(lines: impl Iterator<Item = &'a str>
      ) -> ParseResult<Vec<Labeled<Stmt>>> {
    let mut offset = 0;
    let mut ast = Vec::new();
    for (i, mut line) in lines.enumerate() {
        if COMMENTLINE.is_match(line) || EMPTYLINE.is_match(line) {
            continue;
        }

        if let Some(com_match) = LINEENDCOMMENT.find(line) {
            let (begin, _) = line.split_at(com_match.start());
            line = begin;
        }

        ast.push(stmt(line, i + 1, &mut offset)?);
    }
    Ok(ast)
}

lazy_static! {
    static ref LABEL: Regex = Regex::new(r"^([A-Za-z_][A-Za-z0-9_]*):")
        .unwrap();

    static ref INSTR: Regex = Regex::new(r"^([A-Za-z]+)").unwrap();

    static ref WORDSTMT: Regex = Regex::new(
        r"^\$([A-Za-z_][A-Za-z0-9_]*|-?\d+)$").unwrap();

    static ref SPECIAL: Regex = Regex::new(
        r#"^\.(?i)(asci[ipz])\s*"((?:\\.|[^"])*)""#).unwrap();

    static ref WORDOPERAND: Regex = Regex::new(
        r"^\$?([A-Za-z_][A-Za-z0-9_]*|\(-?\d+\)|-?\d+)$").unwrap();

    static ref DELIM: Regex = Regex::new(r"\s*,\s*").unwrap();

    static ref COMMENTLINE: Regex = Regex::new(r"^\s*#").unwrap();
    static ref LINEENDCOMMENT: Regex = Regex::new(r"#.*$").unwrap();

    static ref EMPTYLINE: Regex = Regex::new(r"^\s*$").unwrap(); 
}

fn stmt(line: &str, line_num: usize, offset: &mut usize
      ) -> ParseResult<Labeled<Stmt>> {
    let mut line = line.trim();
    let mut label = None;

    let mut err = ParseError {
        kind: ParseErrorKind::ExpectedStmt,
        line: line_num,
        offset: *offset,
    };

    if let Some(lbl_match) = LABEL.captures(line).and_then(|caps| caps.get(1)) {
        let (_, rest) = line.split_at(lbl_match.end() + 1);
        label = Some((*offset, lbl_match.as_str().to_string()));
        line = rest.trim_start();
        err.kind = ParseErrorKind::ExpectedInstructionOrWord;
    }

    let instr = INSTR.captures(line)
        .and_then(|caps| caps.get(1))
        .and_then(|imatch| {
            let instr = match imatch.as_str().to_lowercase().as_str() {
                "add" => Some(Instruction::Add),
                "mul" => Some(Instruction::Mul),
                "in" => Some(Instruction::Input),
                "out" => Some(Instruction::Output),
                "jnz" => Some(Instruction::JmpTrue),
                "jz" => Some(Instruction::JmpFalse),
                "lt" => Some(Instruction::Less),
                "eq" => Some(Instruction::Eql),
                "rel" => Some(Instruction::AdjustRelBase),
                "hlt" => Some(Instruction::Halt),
                _ => None,
            };

            let (_, rest) = line.split_at(imatch.end());
            line = rest.trim_start();
            *offset += 1;

            instr
        });

    if let Some(instr) = instr {
        return make_instr(line, instr, label, line_num, offset)
    }

    let word = WORDSTMT.captures(line)
        .and_then(|caps| caps.get(1))
        .and_then(|wmatch| {
            let word = wmatch.as_str();
            let c = word.chars().next().unwrap();
            if c.is_ascii_digit() || c == '-' {
                let word = word.parse::<i64>().ok()?;
                *offset += 1;
                Some(Stmt::Immediate(Word::Number(word)))
            } else {
                *offset += 1;
                Some(Stmt::Immediate(Word::Label(word.to_string())))
            }
        });

    if let Some(word) = word {
        return Ok(Labeled { item: word, label });
    }

    let special = SPECIAL.captures(line)
        .map(|caps| (caps.get(1), caps.get(2)));
    match special {
        Some((Some(cmdmatch), Some(strmatch))) => {
            let cons = match cmdmatch.as_str().to_lowercase().as_str() {
                "ascii" => {
                    Stmt::Ascii
                },
                "asciz" => {
                    *offset += 1;
                    Stmt::Asciz
                },
                "ascip" => {
                    *offset += 1;
                    Stmt::Ascip
                },
                _ => return Err(err),
            };

            let unesc = unescape(strmatch.as_str());
            *offset += unesc.len();
            return Ok(Labeled {
                item: cons(unesc),
                label: label,
            });
        },

        _ => return Err(err),
    };
}

fn make_instr(line: &str, instr: Instruction, label: Option<(usize, String)>,
      line_num: usize, offset: &mut usize) -> ParseResult<Labeled<Stmt>> {
    let mut operands = Vec::new();
    let operand_strs: Vec<&str> = DELIM.split(line).collect();
    for op in operand_strs {
        operands.push(make_operand(op, line_num, *offset)?);
        *offset += 1;
    }
    Ok(Labeled {
        item: Stmt::Instr(instr, operands),
        label: label,
    })
}

fn make_operand(mut operand: &str, line_num: usize, offset: usize
      ) -> ParseResult<Labeled<Operand>> {
    let mut label = None;

    let mut err = ParseError {
        kind: ParseErrorKind::ExpectedOperandOrLabel,
        line: line_num,
        offset: offset,
    };

    if let Some(lbl_match) = LABEL.captures(operand)
          .and_then(|caps| caps.get(1)) {
        let (_, rest) = operand.split_at(lbl_match.end() + 1);
        label = Some((offset, lbl_match.as_str().to_string()));
        operand = rest.trim_start();
        err.kind = ParseErrorKind::ExpectedOperand;
    }

    let mut imm = false;
    if operand.chars().next() == Some('$') {
        imm = true;
    }

    WORDOPERAND.captures(operand)
        .and_then(|caps| caps.get(1))
        .and_then(|wmatch| {
            let word = wmatch.as_str();

            if word.chars().next().unwrap() == '(' {
                if imm {
                    return None;
                }
                operand = operand.trim_start_matches('(')
                    .trim_end_matches(')');
                let word = operand.parse::<i64>().ok()?;
                return Some(Labeled {
                    item: Operand::Relative(word),
                    label: label,
                })
            }

            let c = word.chars().next().unwrap();
            if c.is_ascii_digit() || c == '-' {
                let word = word.parse::<i64>().ok()?;
                Some(Labeled {
                    item: if imm {
                        Operand::Immediate(Word::Number(word))
                    } else {
                        Operand::Position(Word::Number(word))
                    },
                    label: label,
                })
            } else {
                Some(Labeled {
                    item: if imm {
                        Operand::Immediate(Word::Label(word.to_string()))
                    } else {
                        Operand::Position(Word::Label(word.to_string()))
                    },
                    label: label,
                })
            }
        })
        .ok_or(err)
}

fn unescape(lit: &str) -> Vec<u8> {
    let mut buf = Vec::new();
    let mut escape = false;
    for b in lit.bytes() {
        if b == b'\\' {
            escape = true;
            continue;
        }

        let b = if escape {
            match b {
                b'n' => b'\n',
                b't' => b'\t',
                _ => b,
            }
        } else {
            b
        };
        buf.push(b);
        escape = false;
    }
    buf
}
