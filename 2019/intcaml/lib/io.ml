type t = { input: in_channel; output: out_channel }
type 'a m = 'a

let get { input; _ } =
  match In_channel.input_line input with
    | Some line -> int_of_string_opt line
    | _ -> None
    | exception _ -> None

let put word { output; _ } =
  try
    word |> string_of_int |> Out_channel.output_string output;
    Some (Out_channel.output_char output '\n')
  with
    _ -> None
