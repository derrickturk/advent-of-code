open Intcaml.Intcode
open Intcaml.Intcode_loader

let () =
  match cpu_of_string_opt In_channel.(input_all stdin) with
    | Some cpu -> begin match run cpu with
        | Ok () -> print_endline (show_cpu cpu)
        | Error e -> print_endline (show_error e)
      end
    | None -> print_endline "that's not even a program"
