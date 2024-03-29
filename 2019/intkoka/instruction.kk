pub value type dst
  Mem(ptr: int)
  Rel(offset: int)

pub fun dst/show(dst: dst): string
  match dst
    Mem(ptr) -> "Mem(" ++ ptr.show ++ ")"
    Rel(offset) -> "Rel(" ++ offset.show ++ ")"

pub value type src
  Dst(dst: dst)
  Imm(num: int)

pub fun src/show(src: src): string
  match src
    Dst(dst) -> "Dst(" ++ dst.show ++ ")"
    Imm(num) -> "Imm(" ++ num.show ++ ")"

pub type instruction
  Add(src1: src, src2: src, dst: dst)
  Mul(src1: src, src2: src, dst: dst)
  Inp(dst: dst)
  Out(src: src)
  Jnz(cmp: src, tgt: src)
  Jz(cmp: src, tgt: src)
  Less(src1: src, src2: src, dst: dst)
  Eql(src1: src, src2: src, dst: dst)
  Arb(offset: src)
  Hlt

pub fun instr/show(instr: instruction): string
  match instr
    Add(src1, src2, src3) ->
      "Add(" ++ src1.show ++ ", " ++ src2.show ++ ", " ++ src3.show ++ ")"
    Mul(src1, src2, src3) ->
      "Mul(" ++ src1.show ++ ", " ++ src2.show ++ ", " ++ src3.show ++ ")"
    Inp(dst) -> "Inp(" ++ dst.show ++ ")"
    Out(src) -> "Out(" ++ src.show ++ ")"
    Jnz(cmp, tgt) ->
      "Jnz(" ++ cmp.show ++ ", " ++ tgt.show ++ ")"
    Jz(cmp, tgt) ->
      "Jz(" ++ cmp.show ++ ", " ++ tgt.show ++ ")"
    Less(src1, src2, src3) ->
      "Less(" ++ src1.show ++ ", " ++ src2.show ++ ", " ++ src3.show ++ ")"
    Eql(src1, src2, src3) ->
      "Eql(" ++ src1.show ++ ", " ++ src2.show ++ ", " ++ src3.show ++ ")"
    Arb(src) -> "Arb(" ++ src.show ++ ")"
    Hlt -> "Hlt"
