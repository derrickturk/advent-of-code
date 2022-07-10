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

module Monad (M: S) = struct
  type 'a t = ('a, Error.t) result M.m

  let (let*) m f = M.bind m begin function
    | Ok x -> f x
    | Error _ as e -> M.return e
  end

  let return x = M.return (Ok x)
end

module Null = struct
  type t = unit
  type 'a m = 'a

  let bind m f = f m
  let return x = x
  let lift x = x

  let get () = None
  let put _ () = None
end

module Channels = struct
  type t = { input: in_channel; output: out_channel }
  type 'a m = 'a

  let bind m f = f m
  let return x = x
  let lift x = x

  let get { input; _ } =
    match In_channel.input_line input with
      | Some line -> int_of_string_opt line
      | _ -> None
      | exception _ -> None

  let put word { output; _ } =
    try
      word |> string_of_int |> Out_channel.output_string output;
      Some (Out_channel.output_char output '\n')
    with
      _ -> None
end

module Lwt_mvar = struct
  open Lwt.Infix

  type t = { input: int Lwt_mvar.t; output: int Lwt_mvar.t }
  type 'a m = 'a Lwt.t

  let bind = Lwt.bind
  let return = Lwt.return
  let lift = Lwt_result.lift

  let get { input; _ } = Lwt_mvar.take input >|= Option.some
  let put word { output; _ } = Lwt_mvar.put output word >|= Option.some
end
