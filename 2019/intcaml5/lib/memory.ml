type t = { mutable base: int array; extended: (int, int) Hashtbl.t }

let extended_size_hint = 1024
let base_expand_factor = 2.

let init image =
  { base = Array.of_list image
  ; extended = Hashtbl.create ~random:true extended_size_hint
  }

let copy { base; extended } =
  { base = Array.copy base; extended = Hashtbl.copy extended }

let (.%()) mem ix =
  if ix < Array.length mem.base (* includes exception if < 0 *)
    then mem.base.(ix)
    else try Hashtbl.find mem.extended ix with _ -> 0

let (.%()<-) mem ix word =
  let current_length = Array.length (mem.base) in
  if ix < current_length (* includes < 0 exception case *)
    then mem.base.(ix) <- word
    else begin
      let expand_length = int_of_float
        (float_of_int current_length *. base_expand_factor)
      in
      if ix < expand_length
        then
          let padding = Array.make (expand_length - current_length) 0 in
          mem.base <- Array.append mem.base padding;
          mem.base.(ix) <- word
        else
          Hashtbl.replace mem.extended ix word
    end

let to_seq_nonzero { base; extended } =
  let nonzero (_, v) = v <> 0 in
  let ixcmp (i1, _) (i2, _) = Int.compare i1 i2 in
  let ext_nonzero = Seq.filter nonzero (Hashtbl.to_seq extended) in
  let ext_nonzero = Array.of_seq ext_nonzero in
  Array.sort ixcmp ext_nonzero;
  Seq.append
    (Seq.filter nonzero (Array.to_seqi base))
    (Array.to_seq ext_nonzero)

let equal t1 t2 = Seq.equal (=) (to_seq_nonzero t1) (to_seq_nonzero t2)

let of_string_exn words = words
  |> String.split_on_char ','
  |> List.map (fun s -> s |> String.trim |> int_of_string)
  |> init

let of_string_opt words = try
  Some (of_string_exn words)
with
  _ -> None
