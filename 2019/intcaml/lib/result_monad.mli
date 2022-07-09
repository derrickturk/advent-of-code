type 'a t = ('a, Error.t) result
val (let*): 'a t -> ('a -> 'b t) -> 'b t
val return: 'a -> 'a t
