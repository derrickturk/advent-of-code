pub type dst
  Mem(ptr: int)
  Rel(offset: int)

type src
  Dst(dst: dst)
  Imm(num: int)

type instruction
  Add(src1: src, src2: src, dst: dst)
  Mul(src1: src, src2: src, dst: dst)
  Inp(dst: dst)
  Out(src: src)
  Jnz(cmp: src, tgt: src)
  Jz(cmp: src, tgt: src)
  Lt(src1: src, src2: src, dst: dst)
  Eq(src1: src, src2: src, dst: dst)
  Arb(offset: src)
  Hlt
