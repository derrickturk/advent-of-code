open Instruction
open Error

type t = { mutable ip: int; mem: int array }
  [@@deriving show]

let read { mem; _ } = function
  | Dst (Mem ptr) -> begin try
      Ok mem.(ptr)
    with
      _ -> Error (InvalidAddress ptr)
    end
  | Imm word -> Ok word

let write { mem; _ } word = function
  | Mem ptr -> try
      Ok (mem.(ptr) <- word)
    with
      _ -> Error (InvalidAddress ptr)

type mode = Pos | Imm

let decode { ip; mem } =
  let open Result_monad in

  let decode_mode = function
    | 0 -> Ok Pos
    | 1 -> Ok Imm
    | _ -> Error (MalformedInstruction ip)
  in

  let decode_dst pos mode = try
    let word = mem.(pos) in
    match mode with
      | Pos -> Ok (Mem word)
      | _ -> Error (MalformedInstruction ip)
  with
    _ -> Error (InvalidAddress pos)
  in

  let decode_src pos mode = try
    let word = mem.(pos) in
    match mode with
      | Pos -> Ok (Dst (Mem word))
      | Imm -> Ok (Imm word)
  with
    _ -> Error (InvalidAddress pos)
  in

  let* (op, mode1, mode2, mode3) =
    try
      let word = mem.(ip) in
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
    | 99 -> return Hlt
    | word -> Error (InvalidOpcode word)
    | exception _ -> Error (InvalidAddress ip)
