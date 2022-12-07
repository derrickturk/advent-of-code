type t

val empty: t
val root: t -> t
val parent: t -> t
val child: string -> t -> t
val modify_size: (int -> int) -> t -> t
val to_directory: t -> Directory.t
val apply: t -> Command.t -> t
