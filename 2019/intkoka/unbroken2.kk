pub type dst
  Mem(ptr: int)
  Rel(offset: int)

pub fun show(dst: dst): string
  match dst
    Mem(ptr) -> "Mem(" ++ ptr.show ++ ")"
    Rel(offset) -> "Rel(" ++ offset.show ++ ")"

pub type src
  Dst(dst: dst)
  Imm(num: int)

pub fun show(src: src): string
  match src
    Dst(dst) -> "Dst(" ++ dst.show ++ ")"
    Imm(num) -> "Imm(" ++ num.show ++ ")"

pub type instruction
  Inp(dst: dst, _: ())
  Out(src: src, _: ())
  Jz(cmp: src, tgt: src)
  Arb(offset: src, _: ())
  Hlt

pub fun show(instr: instruction): string
  match instr
    Inp(dst, _) -> "Inp(" ++ dst.show ++ ")"
    Out(src, _) -> "Out(" ++ src.show ++ ")"
    Jz(cmp, tgt) ->
      "Jz(" ++ cmp.show ++ ", " ++ tgt.show ++ ")"
    Arb(src, _) -> "Arb(" ++ src.show ++ ")"
    Hlt -> "Hlt"

pub val not-busted = Out(Dst(Mem(224)), ())

pub fun main()
  println(not-busted.show)
