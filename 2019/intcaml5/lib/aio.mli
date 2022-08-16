type xport
type xports = { input: xport; output: xport }

val create_xport: ?contents:int -> unit -> xport
val read_xport: xport -> int option
val read: unit -> (int, Error.t) result
val write: int -> (unit, Error.t) result

val with_xport_io: (xports * (unit -> unit)) list -> unit
