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
  | Inp of dst
  | Out of src
  | Hlt
  [@@deriving show]

type cpu = { mutable ip: int; mem: int array }
  [@@deriving show]

type io = { input: in_channel; output: out_channel }

type error =
  | InvalidOpcode of int
  | InvalidAddress of int
  | MalformedInstruction of int (* position of bad/failed word *)
  | IOError of [`Input | `Output]
  [@@deriving show]

val read: cpu -> src -> (int, error) result
val write: cpu -> int -> dst -> (unit, error) result
val exec: cpu -> io -> instr -> (unit, error) result
val run: cpu -> io -> (unit, error) result
