module type S = sig
  type t
  type 'a m

  val bind: 'a m -> ('a -> 'b m) -> 'b m
  val return: 'a -> 'a m
  val lift: ('a, Error.t) result -> ('a, Error.t) result m

  val get: t -> int option m
  val put: int -> t -> unit option m
end

module type Monad_intf = sig
  type 'a t
  val (let*): 'a t -> ('a -> 'b t) -> 'b t
  val return: 'a -> 'a t
end

module Monad (M: S): Monad_intf
    with type 'a t = ('a, Error.t) result M.m

module Null: sig
  type t = unit
  type 'a m = 'a
  include S with type t := t with type 'a m := 'a m
end

module Channels: sig
  type t = { input: in_channel; output: out_channel }
  type 'a m = 'a
  include S with type t := t with type 'a m := 'a m
end
