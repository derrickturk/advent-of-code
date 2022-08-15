open Effect
open Effect.Deep

type channels = { input: in_channel; output: out_channel }

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

let with_channel_io _ _ _ = failwith "TODO"

(*
module Channels = struct
  type t = { input: in_channel; output: out_channel }
  type 'a m = 'a
  let bind m f = f m
  let return x = x
  let lift x = x

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
end
*)
