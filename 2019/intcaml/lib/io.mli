(* to-do: abstract these, and add e.g. monad lift hooks to make it work *)
type t = { input: in_channel; output: out_channel }
type 'a m = 'a

val get: t -> int option m
val put: int -> t -> unit option m
