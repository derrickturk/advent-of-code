exception Bad_char of char
exception Found_basement of int

let () =
  let input = In_channel.(input_all stdin) in
  let parse = function
    | '(' -> 1
    | ')' -> -1
    | '\n' -> 0
    | c -> raise (Bad_char c)
  in
  let floor = String.fold_left (fun n c -> n + parse c) 0 input in
  let basement_ix = Seq.(
    length (take_while (fun x -> x >= 0)
      (scan (fun n c -> n + parse c) 0 (String.to_seq input)))
  ) in
  print_endline (string_of_int floor);
  print_endline (string_of_int basement_ix)
