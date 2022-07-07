module Cpu = struct
  type t = { mutable ip: int; instrs: int array }

  let make instrs = { ip = 0; instrs }

  let step1 ({ ip; instrs } as cpu) =
    try
      cpu.ip <- ip + instrs.(ip);
      instrs.(ip) <- instrs.(ip) + 1;
      true
    with
      Invalid_argument _ -> false

  let step2 ({ ip; instrs } as cpu) =
    try
      let jmp = instrs.(ip) in
      cpu.ip <- ip + jmp;
      instrs.(ip) <- jmp + if jmp >= 3 then -1 else 1;
      true
    with
      Invalid_argument _ -> false
end

let iter_to_completion f cpu =
  let rec go n = if f cpu
    then go (n + 1)
    else n
  in go 0

let rec line_seq chan =
  let open Seq in
  fun () -> match In_channel.input_line chan with
    | None -> Nil
    | Some line -> Cons (line, line_seq chan)

let () =
  let instrs = In_channel.stdin
    |> line_seq
    |> Seq.map int_of_string
    |> Array.of_seq
  in
  let steps1 = instrs
    |> Array.copy
    |> Cpu.make
    |> iter_to_completion Cpu.step1
  in
  let steps2 = instrs
    |> Array.copy
    |> Cpu.make
    |> iter_to_completion Cpu.step2
  in
  print_endline (string_of_int steps1);
  print_endline (string_of_int steps2)
