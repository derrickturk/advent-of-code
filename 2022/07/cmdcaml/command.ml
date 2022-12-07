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

let rec lines chan = match In_channel.input_line chan with
  | None -> []
  | Some line -> line::lines chan

exception Parse_error

let read chan =
  let rec go_ls = function
    | [] -> ([], [])
    | (line::rest) as all ->
        match String.split_on_char ' ' line with
        | "$"::_ -> ([], all)
        | ["dir"; name] ->
            let (these, rest') = go_ls rest in
            (DirectoryEntry name::these, rest')
        | [nums; name] ->
            let (these, rest') = go_ls rest in
            (FileEntry (int_of_string nums, name)::these, rest')
        | _ -> raise Parse_error
  in
  let rec go_cmd = function
    | [] -> []
    | (line::rest) -> match String.split_on_char ' ' line with
        | ["$"; "cd"; "/"] -> Cd Root::go_cmd rest
        | ["$"; "cd"; ".."] -> Cd Parent::go_cmd rest
        | ["$"; "cd"; child] -> Cd (Child child)::go_cmd rest
        | ["$"; "ls"] ->
            let (results, rest) = go_ls rest in
            Ls results::go_cmd rest
        | _ -> raise Parse_error
  in
  try Some (go_cmd (lines chan)) with
    _ -> None
