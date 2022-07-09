val exec: Cpu.t -> Io.t -> Instruction.t -> (unit, Error.t) result
val run: Cpu.t -> Io.t -> (unit, Error.t) result
