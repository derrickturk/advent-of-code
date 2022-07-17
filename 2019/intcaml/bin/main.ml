open Intcaml

module type SyncIo = sig
  include Io.S with type 'a m = 'a
end

type io_mode =
  | IntLines
  | IntCommaSep
  | Ascii

type config = { program_path: string option; io_mode: io_mode }

module CommaSep = struct
  type t = { input: in_channel
           ; output: out_channel
           ; mutable buf: int list
           ; mutable first: bool
           }

  type 'a m = 'a

  let bind m f = f m
  let return x = x
  let lift r = r

  let make input output = { input; output; buf = []; first = true }

  let get ({ input; buf; _ } as io) =
    match buf with
      | [] -> begin match In_channel.input_line input with
          | None -> None
          | Some line -> try
              let words = String.split_on_char ',' line in
              let vals = List.map
                (fun s -> s |> String.trim |> int_of_string) words
              in
              begin match vals with
                | [] -> None
                | hd::tl ->
                    io.buf <- tl;
                    Some hd
              end
            with
              _ -> None
        end
      | hd::tl ->
          io.buf <- tl;
          Some hd

  let put word ({ output; first; _ } as io) =
    let text = string_of_int word in
    try
      if first
        then Some (Out_channel.output_string output text)
        else begin
          io.first <- false;
          Out_channel.output_char output ',';
          Some (Out_channel.output_string output text)
        end
    with
      _ -> None
end

module Ascii = struct
  type t = { input: in_channel ; output: out_channel }

  type 'a m = 'a

  let bind m f = f m
  let return x = x
  let lift r = r

  let make input output = { input; output }

  let get { input; _ } =
    Option.map int_of_char (In_channel.input_char input)

  let put word { output; _ } =
    try
      let c = char_of_int word in
      Out_channel.output_char output c;
      if c = '\n' then Out_channel.flush output;
      Some ()
    with
      _ -> None
end

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

let main (type a) (module I: SyncIo with type t = a) (io: a) (prog: string) =
  let module M = Machine.Make (I) in
  match Cpu.of_string_opt prog with
    | Some cpu -> begin match M.run cpu io with
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
    | IntLines ->
        let io = let open Io.Channels in
          { input = In_channel.stdin; output = Out_channel.stdout }
        in
        main (module Io.Channels) io prog
    | IntCommaSep ->
        let io = CommaSep.make In_channel.stdin Out_channel.stdout in
        main (module CommaSep) io prog
    | Ascii ->
        let io = Ascii.make In_channel.stdin Out_channel.stdout in
        main (module Ascii) io prog
