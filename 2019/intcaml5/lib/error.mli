type t =
  | InvalidOpcode of int
  | InvalidAddress of int
  | MalformedInstruction of int (* position of bad/failed word *)
  | IOError of [`Input | `Output]
  [@@deriving show]
