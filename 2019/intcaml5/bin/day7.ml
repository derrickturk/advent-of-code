open Intcaml5

exception Intcode_error of Error.t

let rec leaveOuts = function
  | [] -> []
  | hd::tl -> (hd, tl)::(List.map (fun (x, xs) -> (x, hd::xs)) (leaveOuts tl))

let rec permute = function
  | [] -> []
  | [x] -> [[x]]
  | xs -> List.concat (List.map permute1 (leaveOuts xs))
and permute1 (hd, xs) = List.map (fun xs -> hd::xs) (permute xs)

let thrust_part1 (p0, p1, p2, p3, p4) mem =
  let open Aio in
  let m0_in = create_xport ~contents:p0 () in
  let m1_in = create_xport ~contents:p1 () in
  let m2_in = create_xport ~contents:p2 () in
  let m3_in = create_xport ~contents:p3 () in
  let m4_in = create_xport ~contents:p4 () in
  let m4_out = create_xport () in
  let m0_io = { input = m0_in; output = m1_in } in
  let m1_io = { input = m1_in; output = m2_in } in
  let m2_io = { input = m2_in; output = m3_in } in
  let m3_io = { input = m3_in; output = m4_in } in
  let m4_io = { input = m4_in; output = m4_out } in
  let manual = { input = m0_in; output = m0_in } in
  let run io = (io, fun () ->
    match Machine.run (Cpu.init (Memory.copy mem)) with
      | Ok () -> ()
      | Error e -> raise (Intcode_error e)
  ) in
  let m_all = [ run m0_io
              ; run m1_io
              ; run m2_io
              ; run m3_io
              ; run m4_io
              ; (manual, fun () ->
                  match write 0 with
                    | Ok () -> ()
                    | Error e -> raise (Intcode_error e)
                )
              ]
  in
  with_xport_io m_all;
  Option.get (read_xport m4_out)

let thrust_part2 (p0, p1, p2, p3, p4) mem =
  let open Aio in
  let loop = create_xport ~contents:p0 () in
  let m1_in = create_xport ~contents:p1 () in
  let m2_in = create_xport ~contents:p2 () in
  let m3_in = create_xport ~contents:p3 () in
  let m4_in = create_xport ~contents:p4 () in
  let m0_io = { input = loop; output = m1_in } in
  let m1_io = { input = m1_in; output = m2_in } in
  let m2_io = { input = m2_in; output = m3_in } in
  let m3_io = { input = m3_in; output = m4_in } in
  let m4_io = { input = m4_in; output = loop } in
  let run io = (io, fun () ->
    match Machine.run (Cpu.init (Memory.copy mem)) with
      | Ok () -> ()
      | Error e -> raise (Intcode_error e)
  ) in
  let m_all = [ run m0_io
              ; run m1_io
              ; run m2_io
              ; run m3_io
              ; run m4_io
              ; (m4_io, fun () ->
                  match write 0 with
                    | Ok () -> ()
                    | Error e -> raise (Intcode_error e)
                )
              ]
  in
  with_xport_io m_all;
  Option.get (read_xport loop)

let max_thrust f settings mem =
  let visit t = function
    | [p0; p1; p2; p3; p4] ->
        let t' = f (p0, p1, p2, p3, p4) mem in
        max t t'
    | _ -> raise (Invalid_argument "programmer error")
  in
  List.fold_left visit 0 (permute settings)

let () =
  let input = In_channel.(input_all stdin) in
  let mem = Memory.of_string_exn input in
  let t1 = max_thrust thrust_part1 [0; 1; 2; 3; 4] mem in
  let t2 = max_thrust thrust_part2 [5; 6; 7; 8; 9] mem in
  print_endline (string_of_int t1);
  print_endline (string_of_int t2)
