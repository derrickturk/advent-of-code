use super::{
    IntCodeError,
    ReadOperand,
    WriteOperand,
    OpCode,
    ProgramState,
    ExpandoVec,
};

trait DisAsm {
    fn disassemble(&self) -> String;
}

impl DisAsm for WriteOperand {
    fn disassemble(&self) -> String {
        match *self {
            WriteOperand::Position(ptr) => format!("{}", ptr),
            WriteOperand::Relative(offset) => format!("({})", offset),
        }
    }
}

impl DisAsm for ReadOperand {
    fn disassemble(&self) -> String {
        match *self {
            ReadOperand::Immediate(n) => format!("${}", n),
            ReadOperand::Memory(op) => op.disassemble(),
        }
    }
}

impl DisAsm for OpCode {
    fn disassemble(&self) -> String {
        match self {
            OpCode::Add(src1, src2, dst) => format!("add {}, {}, {}",
                src1.disassemble(), src2.disassemble(), dst.disassemble()),
            OpCode::Mul(src1, src2, dst) => format!("add {}, {}, {}",
                src1.disassemble(), src2.disassemble(), dst.disassemble()),
            OpCode::Input(dst) => format!("in {}", dst.disassemble()),
            OpCode::Output(src) => format!("out {}", src.disassemble()),
            OpCode::JmpTrue(cnd, dst) => format!("jnz {}, {}",
                cnd.disassemble(), dst.disassemble()),
            OpCode::JmpFalse(cnd, dst) => format!("jz {}, {}",
                cnd.disassemble(), dst.disassemble()),
            OpCode::Less(src1, src2, dst) => format!("lt {}, {}, {}",
                src1.disassemble(), src2.disassemble(), dst.disassemble()),
            OpCode::Eql(src1, src2, dst) => format!("eq {}, {}, {}",
                src1.disassemble(), src2.disassemble(), dst.disassemble()),
            OpCode::AdjustRelBase(offset) => format!("rel {}",
                offset.disassemble()),
            OpCode::Halt => format!("hlt"),
        }
    }
}

pub fn disassemble_memory(memory: &[i64]) -> Result<String, IntCodeError> {
    let mut output = String::new();
    let mut state = ProgramState::<ExpandoVec>::from(memory.to_vec());
    let original_len = state.memory.0.len();

    while state.ip < original_len {
        let opcode = OpCode::parse_next(&mut state);
        match opcode {
            Ok(opcode) => {
                state.ip += opcode.width();
                output += &opcode.disassemble();
                output += "\n";
            },

            Err(IntCodeError::UnknownOpCode(n)) => {
                state.ip += 1;
                output += &format!("${}\n", n);
            },

            Err(e) => return Err(e),
        }
    }

    Ok(output)
}
