module type S = sig
  type io
  type 'a m

  val exec: Cpu.t -> io -> Instruction.t -> (unit, Error.t) result m
  val run: Cpu.t -> io -> (unit, Error.t) result m
end

module Make (M: Io.S): S with type io = M.t with type 'a m = 'a M.m
