module NameMap: Map.S with type key = string

type t = { size: int; children: t NameMap.t }

val empty: t
val insertChild: string -> t -> t -> t
val lookupChild: string -> t -> t option
val deleteChild: string -> t -> t
val total_size: t -> int
val rooted_with_total_size: t -> (int * t) list
