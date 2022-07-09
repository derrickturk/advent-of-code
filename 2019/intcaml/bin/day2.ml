open Intcaml.Intcode
open Intcaml.Intcode_loader

(* eventually we can use a dummy-io module and just use () here *)
let dummy_io = { input = In_channel.stdin; output = Out_channel.stdout }

let part1 input =
  let cpu = cpu_of_string_exn input in
  cpu.mem.(1) <- 12;
  cpu.mem.(2) <- 2;
  match run cpu dummy_io with
    | Ok () -> print_endline (string_of_int (cpu.mem.(0)))
    | Error e -> print_endline (show_error e)

exception Done of int * int

let part2 input =
  let mem = mem_of_string_exn input in
  try
    for x = 0 to 100 do
      for y = 0 to 100 do
        let cpu = { ip = 0; mem = Array.copy mem } in
        cpu.mem.(1) <- x;
        cpu.mem.(2) <- y;
        match run cpu dummy_io with
          | Ok () -> if cpu.mem.(0) = 19690720
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
