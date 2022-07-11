module type S = sig
  type io
  type 'a m

  val exec: Cpu.t -> io -> Instruction.t -> (unit, Error.t) result m
  val run: Cpu.t -> io -> (unit, Error.t) result m
end

module Make (M: Io.S): S with type io = M.t with type 'a m = 'a M.m = struct
  type io = M.t
  type 'a m = 'a M.m

  open Cpu
  open Instruction
  open Error

  module Io_monad = Io.Monad (M)

  let get io = M.bind (M.get io) begin function
    | Some x -> M.lift (Ok x)
    | None -> M.lift (Error (IOError `Input))
  end

  let put word io = M.bind (M.put word io) begin function
    | Some () -> M.lift (Ok ())
    | None -> M.lift (Error (IOError `Output))
  end

  let exec ({ ip; _ } as cpu) io =
    let open Result_monad in function
    | Add (src1, src2, dst) -> M.lift begin
        let* x = read cpu src1 in
        let* y = read cpu src2 in
        let* () = write cpu (x + y) dst in
        return (cpu.ip <- ip + 4)
      end
    | Mul (src1, src2, dst) -> M.lift begin
        let* x = read cpu src1 in
        let* y = read cpu src2 in
        let* () = write cpu (x * y) dst in
        return (cpu.ip <- ip + 4)
      end
    | Inp dst ->
        let open Io_monad in
        let* word = get io in
        let* () = M.lift (write cpu word dst) in
        return (cpu.ip <- ip + 2)
    | Out src ->
        let open Io_monad in
        let* word = M.lift (read cpu src) in
        let* () = put word io in
        return (cpu.ip <- ip + 2)
    | Jnz (cond, tgt) -> M.lift begin
        let* cond = read cpu cond in
        let* tgt = read cpu tgt in
        return begin if cond <> 0
          then cpu.ip <- tgt
          else cpu.ip <- ip + 3
        end
      end
    | Jz (cond, tgt) -> M.lift begin
        let* cond = read cpu cond in
        let* tgt = read cpu tgt in
        return begin if cond = 0
          then cpu.ip <- tgt
          else cpu.ip <- ip + 3
        end
      end
    | Lt (src1, src2, dst) -> M.lift begin
        let* x = read cpu src1 in
        let* y = read cpu src2 in
        let* () = write cpu (if x < y then 1 else 0) dst in
        return (cpu.ip <- ip + 4)
      end
    | Eq (src1, src2, dst) -> M.lift begin
        let* x = read cpu src1 in
        let* y = read cpu src2 in
        let* () = write cpu (if x = y then 1 else 0) dst in
        return (cpu.ip <- ip + 4)
      end
    | Arb src -> M.lift begin
        let* offset = read cpu src in
        cpu.rb <- cpu.rb + offset;
        return (cpu.ip <- cpu.ip + 2)
      end
    | Hlt -> Io_monad.return ()

  let rec run cpu io =
    let open Io_monad in
    let* instr = M.lift (decode cpu) in
    match instr with
      | Hlt -> return ()
      | _ ->
          let* _ = exec cpu io instr in
          run cpu io
end
