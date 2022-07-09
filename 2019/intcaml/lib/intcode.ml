open Cpu
open Instruction
open Error

let exec ({ ip; _ } as cpu) io =
  let open Result_monad in function
  | Add (src1, src2, dst) ->
      let* x = read cpu src1 in
      let* y = read cpu src2 in
      let* () = write cpu (x + y) dst in
      return (cpu.ip <- ip + 4)
  | Mul (src1, src2, dst) ->
      let* x = read cpu src1 in
      let* y = read cpu src2 in
      let* () = write cpu (x * y) dst in
      return (cpu.ip <- ip + 4)
  | Inp dst ->
      let* word = Option.to_result ~none:(IOError `Input) (Io.get io) in
      let* () = write cpu word dst in
      return (cpu.ip <- ip + 2)
  | Out src ->
      let* word = read cpu src in
      let* () = Option.to_result ~none:(IOError `Output) (Io.put word io) in
      return (cpu.ip <- ip + 2)
  | Jnz (cond, tgt) ->
      let* cond = read cpu cond in
      let* tgt = read cpu tgt in
      return begin if cond <> 0
        then cpu.ip <- tgt
        else cpu.ip <- ip + 3
      end
  | Jz (cond, tgt) ->
      let* cond = read cpu cond in
      let* tgt = read cpu tgt in
      return begin if cond = 0
        then cpu.ip <- tgt
        else cpu.ip <- ip + 3
      end
  | Lt (src1, src2, dst) ->
      let* x = read cpu src1 in
      let* y = read cpu src2 in
      let* () = write cpu (if x < y then 1 else 0) dst in
      return (cpu.ip <- ip + 4)
  | Eq (src1, src2, dst) ->
      let* x = read cpu src1 in
      let* y = read cpu src2 in
      let* () = write cpu (if x = y then 1 else 0) dst in
      return (cpu.ip <- ip + 4)
  | Hlt -> return ()

let rec run cpu io =
  let open Result_monad in
  let* instr = decode cpu in
  match instr with
    | Hlt -> return ()
    | _ ->
        let* _ = exec cpu io instr in
        run cpu io
