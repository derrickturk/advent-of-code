open Error
open Instruction
open Memory

type t = { mutable ip: int; mutable rb: int; mem: Memory.t }

let init mem = { ip = 0; rb = 0; mem }

let init_from_list image = init (Memory.init image)

let copy { ip; rb; mem } = { ip; rb; mem = Memory.copy mem }

let read { rb; mem; _ } = function
  | Imm word -> Ok word
  | Dst dst -> let ptr = match dst with
      | Mem ptr -> ptr
      | Rel offset -> rb + offset
    in try
      Ok (mem.%(ptr))
    with
      _ -> Error (InvalidAddress ptr)

let write { rb; mem; _ } word dst =
  let ptr = match dst with
    | Mem ptr -> ptr
    | Rel offset -> rb + offset
  in try
    Ok (mem.%(ptr) <- word)
  with
    _ -> Error (InvalidAddress ptr)

type mode = Pos | Imm | Rel

let decode { ip; mem; _ } =
  let open Result_monad in

  let decode_mode = function
    | 0 -> Ok Pos
    | 1 -> Ok Imm
    | 2 -> Ok Rel
    | _ -> Error (MalformedInstruction ip)
  in

  let decode_dst pos mode = try
    let word = mem.%(pos) in
    match mode with
      | Pos -> Ok (Mem word)
      | Rel -> Ok (Rel word)
      | _ -> Error (MalformedInstruction ip)
  with
    _ -> Error (InvalidAddress pos)
  in

  let decode_src pos mode = try
    let word = mem.%(pos) in
    match mode with
      | Pos -> Ok (Dst (Mem word))
      | Rel -> Ok (Dst (Rel word))
      | Imm -> Ok (Imm word)
  with
    _ -> Error (InvalidAddress pos)
  in

  let* (op, mode1, mode2, mode3) =
    try
      let word = mem.%(ip) in
      let op = word mod 100 in
      let* mode1 = decode_mode ((word / 100) mod 10) in
      let* mode2 = decode_mode ((word / 1000) mod 10) in
      let* mode3 = decode_mode ((word / 10000) mod 10) in
      Ok (op, mode1, mode2, mode3)
    with
      _ -> Error (InvalidAddress ip)
  in

  match op with
    | 1 ->
        let* src1 = decode_src (ip + 1) mode1 in
        let* src2 = decode_src (ip + 2) mode2 in
        let* dst = decode_dst (ip + 3) mode3 in
        return (Add (src1, src2, dst))
    | 2 ->
        let* src1 = decode_src (ip + 1) mode1 in
        let* src2 = decode_src (ip + 2) mode2 in
        let* dst = decode_dst (ip + 3) mode3 in
        return (Mul (src1, src2, dst))
    | 3 ->
        let* dst = decode_dst (ip + 1) mode1 in
        return (Inp dst)
    | 4 ->
        let* src = decode_src (ip + 1) mode1 in
        return (Out src)
    | 5 ->
        let* src1 = decode_src (ip + 1) mode1 in
        let* src2 = decode_src (ip + 2) mode2 in
        return (Jnz (src1, src2))
    | 6 ->
        let* src1 = decode_src (ip + 1) mode1 in
        let* src2 = decode_src (ip + 2) mode2 in
        return (Jz (src1, src2))
    | 7 ->
        let* src1 = decode_src (ip + 1) mode1 in
        let* src2 = decode_src (ip + 2) mode2 in
        let* dst = decode_dst (ip + 3) mode3 in
        return (Lt (src1, src2, dst))
    | 8 ->
        let* src1 = decode_src (ip + 1) mode1 in
        let* src2 = decode_src (ip + 2) mode2 in
        let* dst = decode_dst (ip + 3) mode3 in
        return (Eq (src1, src2, dst))
    | 9 ->
        let* src = decode_src (ip + 1) mode1 in
        return (Arb src)
    | 99 -> return Hlt
    | word -> Error (InvalidOpcode word)
    | exception _ -> Error (InvalidAddress ip)

let equal t1 t2 = t1.ip = t2.ip && Memory.equal t1.mem t2.mem

let of_string_exn words = init (Memory.of_string_exn words)

let of_string_opt words = try
  Some (of_string_exn words)
with
  _ -> None
