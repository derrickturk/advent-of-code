type instr =
  { reg: string
  ; cmd: int -> int
  ; cnd_reg: string
  ; cnd: int -> bool
  }

let (let*) o f = Option.bind o f
let (let+) o f = Option.map f o
let (and+) o1 o2 = match o1, o2 with
  | Some x, Some y -> Some (x, y)
  | _ -> None

let parse line = match String.split_on_char ' ' line with
  | [ reg; cmd_kind; cmd_val; "if"; cnd_reg; cnd_op; cnd_val ] ->
      let+ cmd =
        let* cmd_val = int_of_string_opt cmd_val in
        begin match cmd_kind with
          | "inc" -> Some (fun x -> x + cmd_val)
          | "dec" -> Some (fun x -> x - cmd_val)
          | _ -> None
        end
      and+ cnd =
        let* cnd_val = int_of_string_opt cnd_val in
        begin match cnd_op with
          | "==" -> Some (fun x -> x == cnd_val)
          | "!=" -> Some (fun x -> x <> cnd_val)
          | "<" -> Some (fun x -> x < cnd_val)
          | ">" -> Some (fun x -> x > cnd_val)
          | "<=" -> Some (fun x -> x <= cnd_val)
          | ">=" -> Some (fun x -> x >= cnd_val)
          | _ -> None
        end
      in
      { reg; cmd; cnd_reg; cnd }
  | _ -> None

let parse_all lines =
  let rec go seen = function
    | [] -> Some (List.rev seen)
    | line::rest -> match parse line with
        | Some instr -> go (instr::seen) rest
        | _ -> None
  in go [] lines

module Mem = Map.Make (String)
type mem = int Mem.t

let exec mem { reg; cmd; cnd_reg; cnd } =
  let cnd_val = try Mem.find cnd_reg mem with _ -> 0 in
  let apply = function
    | None -> Some (cmd 0)
    | Some x -> Some (cmd x)
  in
  if cnd cnd_val
    then Mem.update reg apply mem
    else mem

let rec line_seq chan =
  let open Seq in
  fun () -> match In_channel.input_line chan with
    | None -> Nil
    | Some line -> Cons (line, line_seq chan)

let max_value mem = Mem.fold (fun _ x m -> max x m) mem 0

let () =
  match In_channel.stdin |> line_seq |> List.of_seq |> parse_all with
    | Some instrs ->
        let (champ, final) = List.fold_left
          (fun (m, mem) i ->
            let mem' = exec mem i in (max m (max_value mem'), mem'))
          (0, Mem.empty)
          instrs
        in
        print_endline (string_of_int (max_value final));
        print_endline (string_of_int champ)
    | _ -> print_endline "failed to parse"
