type path =
  | Root
  | Child of (string * t)
and t = Z of (path * Directory.t)

let empty = Z (Root, Directory.empty)

let parent = function
  | Z (Root, _) as z -> z
  | Z (Child (name, Z (pp, pd)), d) ->
      Z (pp, Directory.insertChild name d pd)

let rec root = function
  | Z (Root, _) as z -> z
  | c -> root (parent c)

let child name (Z (pp, pd)) =
  let open Directory in
  match lookupChild name pd with
    | Some cd -> Z (Child (name, Z (pp, deleteChild name pd)), cd)
    | None -> Z (Child (name, Z (pp, pd)), empty)

let modify_size f (Z (p, d)) = Z (p, { d with size = f d.size })

let to_directory (Z (_, d)) = d

let apply_ls z = function
  | Command.FileEntry (sz, _) -> modify_size (fun s -> s + sz) z
  | DirectoryEntry _ -> z

let apply z = function
  | Command.Cd Root -> root z
  | Cd Parent -> parent z
  | Cd (Child name) -> child name z
  | Ls results -> List.fold_left apply_ls z results
