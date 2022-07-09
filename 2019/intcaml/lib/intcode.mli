type dst =
  | Mem of int
  [@@deriving show]

type src =
  | Dst of dst
  | Imm of int
  [@@deriving show]

type instr =
  | Add of src * src * dst
  | Mul of src * src * dst
  | Hlt
  [@@deriving show]

type cpu = { mutable ip: int; mem: int array }
  [@@deriving show]

type error =
  | InvalidOpcode of int
  | InvalidAddress of int
  | MalformedInstruction of int (* position of bad/failed word *)
  [@@deriving show]

val read: cpu -> src -> (int, error) result
val write: cpu -> int -> dst -> (unit, error) result
val exec: cpu -> instr -> (unit, error) result
val run: cpu -> (unit, error) result
