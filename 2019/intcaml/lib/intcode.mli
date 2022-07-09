type cpu = { mutable ip: int; mem: int array }
  [@@deriving show]

type io = { input: in_channel; output: out_channel }

val read: cpu -> Instruction.src -> (int, Error.t) result
val write: cpu -> int -> Instruction.dst -> (unit, Error.t) result
val exec: cpu -> io -> Instruction.t -> (unit, Error.t) result
val run: cpu -> io -> (unit, Error.t) result
