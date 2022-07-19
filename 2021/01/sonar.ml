let rec read_ints chan =
  match In_channel.input_line chan with
    | None -> []
    | Some line -> int_of_string line :: read_ints chan

let rec threes = function
  | a::(b::c::tl as rest) -> fun () ->
      Seq.Cons ((a, b, c), fun () -> threes rest ())
  | _ -> fun () -> Seq.Nil

let () =
  let input = read_ints In_channel.stdin in
  let increasing =
    let input' = List.to_seq input in
    let f n (prev, cur) = if cur > prev then n + 1 else n in
    Seq.(fold_left f 0 (zip input' (drop 1 input')))
  in
  let increasing3 =
    let input' = threes input in
    let f n ((a, b, c), (d, e, f)) =
      if d + e + f > a + b + c then n + 1 else n
    in
    Seq.(fold_left f 0 (zip input' (drop 1 input')))
  in
  Printf.printf "%d\n%d\n" increasing increasing3
