type newdir =
  | Root
  | Parent
  | Child of string

type ls_result =
  | FileEntry of int * string
  | DirectoryEntry of string

type t =
  | Cd of newdir
  | Ls of ls_result list

val read: In_channel.t -> t list option
