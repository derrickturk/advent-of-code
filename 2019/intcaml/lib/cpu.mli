type t = { mutable ip: int; mutable rb: int; mem: Memory.t }

val init: Memory.t -> t
val init_from_list: int list -> t
val copy: t -> t

val decode: t -> (Instruction.t, Error.t) result
val read: t -> Instruction.src -> (int, Error.t) result
val write: t -> int -> Instruction.dst -> (unit, Error.t) result

val equal: t -> t -> bool

val of_string_opt: string -> t option
val of_string_exn: string -> t
