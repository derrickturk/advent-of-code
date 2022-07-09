type t = { mutable ip: int; mem: int array }
  [@@deriving show]

val decode: t -> (Instruction.t, Error.t) result
val read: t -> Instruction.src -> (int, Error.t) result
val write: t -> int -> Instruction.dst -> (unit, Error.t) result
