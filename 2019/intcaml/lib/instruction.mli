type dst =
  | Mem of int
  | Rel of int
  [@@deriving show]

type src =
  | Dst of dst
  | Imm of int
  [@@deriving show]

type t =
  | Add of src * src * dst
  | Mul of src * src * dst
  | Inp of dst
  | Out of src
  | Jnz of src * src
  | Jz of src * src
  | Lt of src * src * dst
  | Eq of src * src * dst
  | Arb of src
  | Hlt
  [@@deriving show]
