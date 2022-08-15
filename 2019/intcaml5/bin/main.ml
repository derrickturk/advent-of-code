open Intcaml5

type io_mode =
  | IntLines
  | IntCommaSep
  | Ascii

type config = { program_path: string option; io_mode: io_mode }

let parse_config () =
  let usage_msg = Sys.argv.(0) ^ " program [-m lines|commasep|ascii]" in
  let program_path = ref None in
  let io_mode = ref IntLines in
  let anon_fun path = match !program_path with 
    | None -> program_path := Some path
    | Some _ -> raise (Arg.Bad "Extra argument provided.")
  in
  let mode_fun = function
    | "lines" -> io_mode := IntLines
    | "commasep" -> io_mode := IntCommaSep
    | "ascii" -> io_mode := Ascii
    | _ -> raise (Arg.Bad "Invalid I/O mode.")
  in
  let spec =
    [ ("-m", Arg.Symbol (["lines"; "commasep"; "ascii"], mode_fun), " I/O mode")
    ]
  in
  Arg.parse spec anon_fun usage_msg;
  { program_path = !program_path; io_mode = !io_mode }

let main runner prog =
  let io: Io.channels =
    { input = In_channel.stdin; output = Out_channel.stdout }
  in
  match Cpu.of_string_opt prog with
    | Some cpu -> begin match runner io Machine.run cpu with
        | Ok () -> ()
        | Error e -> print_endline (Error.show e)
      end
    | None -> print_endline "that's not even a program"

let () =
  let { program_path; io_mode } = parse_config () in
  let prog = match program_path with
    | None -> Option.value ~default:"" (In_channel.input_line stdin)
    | Some path -> In_channel.(with_open_text path input_all)
  in
  match io_mode with
    | IntLines -> main Io.with_line_io prog
    | IntCommaSep -> main Io.with_commasep_io prog
    | Ascii -> main Io.with_ascii_io prog
