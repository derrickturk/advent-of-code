pub value type dst
  Mem(ptr: int)
  Rel(offset: int)

pub fun show(dst: dst): string
  match dst
    Mem(ptr) -> "Mem(" ++ ptr.show ++ ")"
    Rel(offset) -> "Rel(" ++ offset.show ++ ")"

pub value type src
  Dst(dst: dst)
  Imm(num: int)

pub fun show(src: src): string
  match src
    Dst(dst) -> "Dst(" ++ dst.show ++ ")"
    Imm(num) -> "Imm(" ++ num.show ++ ")"

pub type instruction
  Add(src1: src, src2: src, dst: dst)
  Mul(src1: src, src2: src, dst: dst)
  Inp(dst: dst, _: (), _: ())
  Out(src: src, _: (), _: ())
  Jnz(cmp: src, tgt: src, _: ())
  Jz(cmp: src, tgt: src, _: ())
  Less(src1: src, src2: src, dst: dst)
  Eql(src1: src, src2: src, dst: dst)
  Arb(offset: src, _: (), _: ())
  Hlt

pub fun show(instr: instruction): string
  match instr
    Add(src1, src2, src3) ->
      "Add(" ++ src1.show ++ ", " ++ src2.show ++ ", " ++ src3.show ++ ")"
    Mul(src1, src2, src3) ->
      "Mul(" ++ src1.show ++ ", " ++ src2.show ++ ", " ++ src3.show ++ ")"
    Inp(dst, _, _) -> "Inp(" ++ dst.show ++ ")"
    Out(src, _, _) -> "Out(" ++ src.show ++ ")"
    Jnz(cmp, tgt, _) ->
      "Jnz(" ++ cmp.show ++ ", " ++ tgt.show ++ ")"
    Jz(cmp, tgt, _) ->
      "Jz(" ++ cmp.show ++ ", " ++ tgt.show ++ ")"
    Less(src1, src2, src3) ->
      "Less(" ++ src1.show ++ ", " ++ src2.show ++ ", " ++ src3.show ++ ")"
    Eql(src1, src2, src3) ->
      "Eql(" ++ src1.show ++ ", " ++ src2.show ++ ", " ++ src3.show ++ ")"
    Arb(src, _, _) -> "Arb(" ++ src.show ++ ")"
    Hlt -> "Hlt"
