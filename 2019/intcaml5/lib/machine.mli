open Effect

type _ Effect.t +=
  | Input: (int, Error.t) result t
  | Output: int -> (unit, Error.t) result t

val exec: Cpu.t -> Instruction.t -> (unit, Error.t) result
val run: Cpu.t -> (unit, Error.t) result
