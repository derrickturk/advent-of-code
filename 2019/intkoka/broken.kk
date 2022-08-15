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
  Out(src: src)
  Jz(cmp: src, tgt: src)

pub fun show(instr: instruction): string
  match instr
    Out(src) -> "Out(" ++ src.show ++ ")"
    Jz(cmp, tgt) ->
      "Jz(" ++ cmp.show ++ ", " ++ tgt.show ++ ")"

pub val busted = Out(Dst(Mem(224)))

pub fun main()
  println(busted.show)
