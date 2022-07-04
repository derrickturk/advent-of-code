type reg =
  | A
  | B

type instr =
  | Hlf of reg
  | Tpl of reg
  | Inc of reg
  | Jmp of int
  | Jie of reg * int
  | Jio of reg * int

type compy =
  { a: int
  ; b: int
  ; ip: int
  ; code: instr array
  }

let init instrs =
  { a = 0
  ; b = 0
  ; ip = 0
  ; code = Array.of_list instrs
  }

let step ({ ip; code; _ } as cpu) =
  let read { a; b; _ } = function
    | A -> a
    | B -> b
  in
  let write cpu v = function
    | A -> { cpu with a = v }
    | B -> { cpu with b = v }
  in
  let cpu = { cpu with ip = ip + 1 } in
  try
    let cpu = match code.(ip) with
      | Hlf r -> write cpu (read cpu r / 2) r
      | Tpl r -> write cpu (read cpu r * 3) r
      | Inc r -> write cpu (read cpu r + 1) r
      | Jmp o -> { cpu with ip = ip + o }
      | Jie (r, o) -> if read cpu r mod 2 = 0
          then { cpu with ip = ip + o }
          else cpu
      | Jio (r, o) -> if read cpu r = 1
          then { cpu with ip = ip + o }
          else cpu
    in Some cpu
  with _ -> None

let rec run cpu =
  match step cpu with
    | None -> cpu
    | Some cpu' -> run cpu'

module Parser = struct
  open Kessel

  let reg = (A <$ l "a") <|> (B <$ l "b")

  let offset =
    let+ sign = (1 <$ l "+") <|> (-1 <$ l "-")
    and+ off = unsigned_int
    in sign * off

  let instr =
    let hlf = let+ _ = lexeme (l "hlf") and+ r = reg in Hlf r in
    let tpl = let+ _ = lexeme (l "tpl") and+ r = reg in Tpl r in
    let inc = let+ _ = lexeme (l "inc") and+ r = reg in Inc r in
    let jmp = let+ _ = lexeme (l "jmp") and+ o = offset in Jmp o in
    let jie =
      let+ _ = lexeme (l "jie")
      and+ r = lexeme reg
      and+ _ = lexeme (l ",")
      and+ o = offset
      in Jie (r, o)
    in
    let jio =
      let+ _ = lexeme (l "jio")
      and+ r = lexeme reg
      and+ _ = lexeme (l ",")
      and+ o = offset
      in Jio (r, o)
    in
    hlf <|> tpl <|> inc <|> jmp <|> jie <|> jio

  let program = many (lexeme instr)
end

let () =
  let input = In_channel.(input_all stdin) in
  match Kessel.parse_all Parser.program input with
    | Ok instrs ->
        let cpu = run (init instrs) in
        let cpu2 = run { (init instrs) with a = 1 } in
        print_endline (string_of_int cpu.b);
        print_endline (string_of_int cpu2.b)
    | Error e -> print_endline
        ("error: expected " ^ e.expected ^ ", found " ^ e.found)
