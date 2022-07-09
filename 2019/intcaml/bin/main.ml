open Intcaml.Intcode
open Intcaml.Intcode_loader

let () =
  let io = { input = In_channel.stdin; output = Out_channel.stdout } in
  let prog = In_channel.input_line stdin in
  match Option.bind prog cpu_of_string_opt with
    | Some cpu -> begin match run cpu io with
        | Ok () -> ()
        | Error e -> print_endline (show_error e)
      end
    | None -> print_endline "that's not even a program"
