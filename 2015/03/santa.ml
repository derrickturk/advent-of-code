module Pos = struct
  type t = int * int
  let compare (x0, y0) (x1, y1) =
    match compare x0 x1 with
      | 0 -> compare y0 y1
      | c -> c
end

module PosSet = Set.Make (Pos)

exception Bad_move

let move (x, y) = function
  | '>' -> (x + 1, y)
  | '<' -> (x - 1, y)
  | '^' -> (x, y + 1)
  | 'v' -> (x, y - 1)
  | _ -> raise Bad_move

let rec evens seq () =
  match seq () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons(x, xs) -> Seq.Cons(x, odds xs)
and odds seq () =
  match seq () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons(x, xs) -> evens xs ()

let () =
  let input = String.trim In_channel.(input_all stdin) in
  let step (pos, seen) c =
    let pos' = move pos c in
    (pos', PosSet.add pos' seen)
  in
  let seen seq = snd
    (Seq.fold_left step ((0, 0), PosSet.singleton (0, 0)) seq) in
  let input = String.to_seq input in
  let part1_seen = seen input in
  let santa_seen = seen (evens input) in
  let robot_seen = seen (odds input) in
  print_endline (part1_seen |> PosSet.cardinal |> string_of_int);
  print_endline
    (PosSet.union santa_seen robot_seen |> PosSet.cardinal |> string_of_int)
