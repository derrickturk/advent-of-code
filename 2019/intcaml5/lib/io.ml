open Effect
open Effect.Deep

type channels = { input: in_channel; output: out_channel }

type handler =
  { read: in_channel -> int option
  ; write: int -> out_channel -> unit option
  }

let with_null_io f init = try_with f init
  { effc = fun (type a) (eff: a t) -> match eff with
      | Machine.Input -> Some (
          fun (k: (a, _) continuation) ->
            continue k (Error (Error.IOError `Input))
        )
      | Machine.Output _ -> Some (
          fun (k: (a, _) continuation) ->
            continue k (Error (Error.IOError `Output))
        )
      | _ -> None
  }

let with_channel_io { input; output } f init { read; write } = try_with f init
  { effc = fun (type a) (eff: a t) -> match eff with
      | Machine.Input -> Some (
          fun (k: (a, _) continuation) ->
            match read input with
              | None -> continue k (Error (Error.IOError `Input))
              | Some word -> continue k (Ok word)
        )
      | Machine.Output word -> Some (
          fun (k: (a, _) continuation) ->
            match write word output with
              | None -> continue k (Error (Error.IOError `Output))
              | Some () -> continue k (Ok ())
        )
      | _ -> None
  }

let with_line_io io f init = with_channel_io io f init
  { read = begin fun input ->
      Option.bind (In_channel.input_line input) int_of_string_opt
    end
  ; write = begin fun word output ->
      Out_channel.output_string output (string_of_int word);
      Out_channel.output_char output '\n';
      Some ()
    end
  }

let with_commasep_io io f init =
  let buf = ref [] in
  let first = ref true in
  with_channel_io io f init
  { read = begin fun input ->
      match !buf with
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
                      buf := tl;
                      Some hd
                end
              with
                _ -> None
          end
        | hd::tl ->
            buf := tl;
            Some hd
    end
  ; write = begin fun word output ->
      if !first
        then first := false
        else Out_channel.output_char output ','
      ;
      Out_channel.output_string output (string_of_int word);
      Out_channel.output_char output '\n';
      Some ()
    end
  }

let with_ascii_io io f init = with_channel_io io f init
  { read = begin fun input ->
      Option.map int_of_char (In_channel.input_char input)
    end
  ; write = begin fun word output ->
      try
        let c = char_of_int word in
        Out_channel.output_char output c;
        if c = '\n' then Out_channel.flush output;
        Some ()
      with
        _ -> None
    end
  }
