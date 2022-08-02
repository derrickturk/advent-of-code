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
  Inp(dst: dst)
  Out(src: src)
  Arb(offset: src)
  Hlt

pub fun show(instr: instruction): string
  match instr
    Inp(dst) -> "Inp(" ++ dst.show ++ ")"
    Out(src) -> "Out(" ++ src.show ++ ")"
    Arb(src) -> "Arb(" ++ src.show ++ ")"
    Hlt -> "Hlt"

pub val fucked = Out(Dst(Mem(224)))

pub fun main()
  println(fucked.show)
