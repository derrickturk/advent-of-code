use super::{ReadOperand, WriteOperand, OpCode};

trait DisAsm {
    fn disassemble(&self) -> String {
        unimplemented!()
    }
}
