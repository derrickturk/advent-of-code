let stdin_lines = Seq.of_dispenser (fun () -> In_channel.(input_line stdin))

let chunk_by d s =
  let rec this_chunk this () =
  let open Seq in
    match s () with
      | Cons (x, s') ->
          if x = d
            then Cons (List.rev this, this_chunk [])
            else this_chunk (x::this) ()
      | Nil -> match this with
          | [] -> Nil
          | _ -> Cons (List.rev this, fun () -> Nil)
  in this_chunk []

let rec traverse_opt = function
  | [] -> Some []
  | (None::_) -> None
  | (Some hd::tl) -> match traverse_opt tl with
      | Some tl' -> Some (hd::tl')
      | None -> None

let () =
  let elf_records = stdin_lines
    |> chunk_by ""
    |> Seq.map (fun l -> l |> List.map int_of_string_opt |> traverse_opt)
    |> List.of_seq
    |> traverse_opt
    |> Option.map (List.map (List.fold_left (+) 0))
    |> Option.map (List.sort (fun a b -> compare b a))
  in
  match elf_records with
    | Some (e1::e2::e3::_) ->
        print_endline (string_of_int e1);
        print_endline (string_of_int (e1 + e2 + e3))
    | Some _ ->
        Out_channel.(output_string stderr "Not enough elves!\n");
        Stdlib.exit 1
    | None ->
        Out_channel.(output_string stderr "Bad input!\n");
        Stdlib.exit 1
