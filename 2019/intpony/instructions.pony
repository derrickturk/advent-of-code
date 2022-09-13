primitive Mem
primitive Rel
primitive Imm

type Dst is ((Mem | Rel), I64)
type Src is ((Mem | Rel | Imm), I64)

primitive Add
primitive Mul
primitive Inp
primitive Out
primitive Jnz
primitive Jz
primitive Lt
primitive Eq
primitive Arb
primitive Hlt

type Instruction is (
    (Add, Src, Src, Dst)
  | (Mul, Src, Src, Dst)
  | (Inp, Dst)
  | (Out, Src)
  | (Jnz, Src, Src)
  | (Jz, Src, Src)
  | (Lt, Src, Src, Dst)
  | (Eq, Src, Src, Dst)
  | (Arb, Src)
  | Hlt
)
