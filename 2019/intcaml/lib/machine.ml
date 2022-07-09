module type Machine = sig
  type t
  type io
  type 'a m
end
