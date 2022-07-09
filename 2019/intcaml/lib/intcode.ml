type dst =
  | Mem of int
  [@@deriving show]

type src =
  | Dst of dst
  | Imm of int
  [@@deriving show]

type instr =
  | Add of src * src * dst
  | Mul of src * src * dst
  | Hlt
  [@@deriving show]

type cpu = { mutable ip: int; mem: int array }
  [@@deriving show]

type error =
  | InvalidOpcode of int
  | InvalidAddress of int
  | MalformedInstruction of int (* position of bad/failed word *)
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

module Result_monad = struct
  let (let*) m f = Result.bind m f
  (*
  let (let+) m f = Result.map m f
  let (and+) a b = match a, b with
    | Ok a', Ok b' -> Ok (a', b')
    | (Error _ as e), _ -> e
    | _, (Error _ as e) -> e
  *)
  let return x = Ok x
end

let decode { ip; mem } =
  let open Result_monad in

  let decode_dst mem pos = try
    let word = mem.(pos) in
    Ok (Mem word)
  with
    _ -> Error (MalformedInstruction pos)
  in

  let decode_src mem pos = try
    let word = mem.(pos) in
    Ok (Dst (Mem word))
  with
    _ -> Error (MalformedInstruction pos)
  in

  match mem.(ip) with
    | 1 ->
        let* src1 = decode_src mem (ip + 1) in
        let* src2 = decode_src mem (ip + 2) in
        let* dst = decode_dst mem (ip + 3) in
        return (Add (src1, src2, dst))
    | 2 ->
        let* src1 = decode_src mem (ip + 1) in
        let* src2 = decode_src mem (ip + 2) in
        let* dst = decode_dst mem (ip + 3) in
        return (Mul (src1, src2, dst))
    | 99 -> return Hlt
    | word -> Error (InvalidOpcode word)
    | exception _ -> Error (InvalidAddress ip)

let exec ({ ip; _ } as cpu) =
  let open Result_monad in function
  | Add (src1, src2, dst) ->
      let* x = read cpu src1 in
      let* y = read cpu src2 in
      let* _ = write cpu (x + y) dst in
      return (cpu.ip <- ip + 4)
  | Mul (src1, src2, dst) ->
      let* x = read cpu src1 in
      let* y = read cpu src2 in
      let* _ = write cpu (x * y) dst in
      return (cpu.ip <- ip + 4)
  | Hlt -> return ()

let rec run cpu =
  let open Result_monad in
  let* i = decode cpu in
  match i with
    | Hlt -> return ()
    | _ ->
        let* _ = exec cpu i in
        run cpu
