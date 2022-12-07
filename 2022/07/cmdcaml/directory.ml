module NameMap = Map.Make(String)

type t = { size: int; children: t NameMap.t }

let empty = { size = 0; children = NameMap.empty }

let insertChild name child d =
  { d with children = NameMap.add name child d.children }

let lookupChild name d = NameMap.find_opt name d.children

let deleteChild name d =
  { d with children = NameMap.remove name d.children }

let rec total_size d =
  NameMap.fold (fun _ c sz -> sz + total_size c) d.children d.size

let rec rooted_with_total_size d = (total_size d, d) ::
  List.concat_map (fun (_, c) -> rooted_with_total_size c)
    (NameMap.bindings d.children)
