let () =
  match Command.read In_channel.stdin with
    | Some cmds ->
        let root =
          let open Zipper in
          List.fold_left apply empty cmds |> root |> to_directory
        in
        let sizes = Directory.rooted_with_total_size root
          |> List.map (fun (sz, _) -> sz)
        in
        let root_size = List.hd sizes in
        let sizes_inorder = sizes |> List.sort compare in
        let total_small = sizes_inorder
          |> List.filter (fun sz -> sz <= 100000)
          |> List.fold_left (+) 0
        in
        let gap = 30000000 - (70000000 - root_size) in
        let first_delete = List.find (fun s -> s >= gap) sizes_inorder in
        print_endline (string_of_int total_small);
        print_endline (string_of_int first_delete)
    | None ->
        prerr_endline "failed to parse";
        exit 1
