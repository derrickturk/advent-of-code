open Cpu
open Instruction

open Effect

type _ Effect.t +=
  | Input: (int, Error.t) result t
  | Output: int -> (unit, Error.t) result t

let exec ({ ip; _ } as cpu) =
  let open Result_monad in function
  | Add (src1, src2, dst) -> begin
      let* x = read cpu src1 in
      let* y = read cpu src2 in
      let* () = write cpu (x + y) dst in
      return (cpu.ip <- ip + 4)
    end
  | Mul (src1, src2, dst) -> begin
      let* x = read cpu src1 in
      let* y = read cpu src2 in
      let* () = write cpu (x * y) dst in
      return (cpu.ip <- ip + 4)
    end
  | Inp dst ->
      let* word = perform Input in
      let* () = write cpu word dst in
      return (cpu.ip <- ip + 2)
  | Out src ->
      let* word = read cpu src in
      let* () = perform (Output word) in
      return (cpu.ip <- ip + 2)
  | Jnz (cond, tgt) -> begin
      let* cond = read cpu cond in
      let* tgt = read cpu tgt in
      return begin if cond <> 0
        then cpu.ip <- tgt
        else cpu.ip <- ip + 3
      end
    end
  | Jz (cond, tgt) -> begin
      let* cond = read cpu cond in
      let* tgt = read cpu tgt in
      return begin if cond = 0
        then cpu.ip <- tgt
        else cpu.ip <- ip + 3
      end
    end
  | Lt (src1, src2, dst) -> begin
      let* x = read cpu src1 in
      let* y = read cpu src2 in
      let* () = write cpu (if x < y then 1 else 0) dst in
      return (cpu.ip <- ip + 4)
    end
  | Eq (src1, src2, dst) -> begin
      let* x = read cpu src1 in
      let* y = read cpu src2 in
      let* () = write cpu (if x = y then 1 else 0) dst in
      return (cpu.ip <- ip + 4)
    end
  | Arb src -> begin
      let* offset = read cpu src in
      cpu.rb <- cpu.rb + offset;
      return (cpu.ip <- cpu.ip + 2)
    end
  | Hlt -> return ()

let rec run cpu =
  let open Result_monad in
  let* instr = decode cpu in
  match instr with
    | Hlt -> return ()
    | _ ->
        let* _ = exec cpu instr in
        run cpu
