open Intcaml5
open Intcaml5.Cpu
open Intcaml5.Machine
open Intcaml5.Io

let part1 input =
  let cpu = Cpu.of_string_exn input in
  let open Memory in
  cpu.mem.%(1) <- 12;
  cpu.mem.%(2) <- 2;
  match with_null_io run cpu with
    | Ok () -> print_endline (string_of_int (cpu.mem.%(0)))
    | Error e -> print_endline (Error.show e)

exception Done of int * int

let part2 input =
  let mem = Memory.of_string_exn input in
  try
    for x = 0 to 100 do
      for y = 0 to 100 do
        let open Memory in
        let cpu = Cpu.init (copy mem) in
        cpu.mem.%(1) <- x;
        cpu.mem.%(2) <- y;
        match with_null_io run cpu with
          | Ok () -> if cpu.mem.%(0) = 19690720
            then raise (Done (x, y))
          | _ -> ()
      done
    done;
    print_endline "couldn't find it!"
  with
    Done (x, y) -> print_endline (string_of_int (100 * x + y))

let () =
  let input = In_channel.(input_all stdin) in
  part1 input;
  part2 input
