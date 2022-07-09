open Intcaml
open Intcaml.Intcode_loader

module M = Machine.Make (Io.Channels)

let () =
  let io: Io.Channels.t = { input = In_channel.stdin; output = Out_channel.stdout } in
  let prog = In_channel.input_line stdin in
  match Option.bind prog cpu_of_string_opt with
    | Some cpu -> begin match M.run cpu io with
        | Ok () -> ()
        | Error e -> print_endline (Error.show e)
      end
    | None -> print_endline "that's not even a program"
