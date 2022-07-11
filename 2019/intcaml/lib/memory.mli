type t

val init: int list -> t
val copy: t -> t

val (.%()): t -> int -> int
val (.%()<-): t -> int -> int -> unit

val equal: t -> t -> bool

val to_seq_nonzero: t -> (int * int) Seq.t

val of_string_opt: string -> t option
val of_string_exn: string -> t
