(*
open Intcaml5

module M = Machine.Make (Io.Lwt_mvar)

exception Intcode_error of Intcaml.Error.t

let rec leaveOuts = function
  | [] -> []
  | hd::tl -> (hd, tl)::(List.map (fun (x, xs) -> (x, hd::xs)) (leaveOuts tl))

let rec permute = function
  | [] -> []
  | [x] -> [[x]]
  | xs -> List.concat (List.map permute1 (leaveOuts xs))
and permute1 (hd, xs) = List.map (fun xs -> hd::xs) (permute xs)

let thrust_part1 (p0, p1, p2, p3, p4) mem =
  let m0_in = Lwt_mvar.create p0 in
  let m1_in = Lwt_mvar.create p1 in
  let m2_in = Lwt_mvar.create p2 in
  let m3_in = Lwt_mvar.create p3 in
  let m4_in = Lwt_mvar.create p4 in
  let m4_out = Lwt_mvar.create_empty () in
  let open Io.Lwt_mvar in
  let m0_io = { input = m0_in; output = m1_in } in
  let m1_io = { input = m1_in; output = m2_in } in
  let m2_io = { input = m2_in; output = m3_in } in
  let m3_io = { input = m3_in; output = m4_in } in
  let m4_io = { input = m4_in; output = m4_out } in
  let run io = let open Lwt_result in
    get_exn (map_error (fun e -> Intcode_error e)
      (M.run (Cpu.init (Memory.copy mem)) io))
  in
  let m_all = [ run m0_io
              ; run m1_io
              ; run m2_io
              ; run m3_io
              ; run m4_io
              ; Lwt_mvar.put m0_in 0
              ]
  in Lwt.bind (Lwt.join m_all) (fun _ -> Lwt_mvar.take m4_out)

let thrust_part2 (p0, p1, p2, p3, p4) mem =
  let loop = Lwt_mvar.create p0 in
  let m1_in = Lwt_mvar.create p1 in
  let m2_in = Lwt_mvar.create p2 in
  let m3_in = Lwt_mvar.create p3 in
  let m4_in = Lwt_mvar.create p4 in
  let open Io.Lwt_mvar in
  let m0_io = { input = loop; output = m1_in } in
  let m1_io = { input = m1_in; output = m2_in } in
  let m2_io = { input = m2_in; output = m3_in } in
  let m3_io = { input = m3_in; output = m4_in } in
  let m4_io = { input = m4_in; output = loop } in
  let run io = let open Lwt_result in
    get_exn (map_error (fun e -> Intcode_error e)
      (M.run (Cpu.init (Memory.copy mem)) io))
  in
  let m_all = [ run m0_io
              ; run m1_io
              ; run m2_io
              ; run m3_io
              ; run m4_io
              ; Lwt_mvar.put loop 0
              ]
  in Lwt.bind (Lwt.join m_all) (fun _ -> Lwt_mvar.take loop)

let max_thrust f settings mem =
  let max_thrust t = function
    | [p0; p1; p2; p3; p4] ->
        Lwt.map (fun t' -> max t t') (f (p0, p1, p2, p3, p4) mem)
    | _ -> raise (Invalid_argument "programmer error")
  in
  Lwt_list.fold_left_s max_thrust 0 (permute settings)

let () =
  let input = In_channel.(input_all stdin) in
  let mem = Memory.of_string_exn input in
  let (t1, t2) = Lwt_main.run @@
    Lwt.both 
      (max_thrust thrust_part1 [0; 1; 2; 3; 4] mem)
      (max_thrust thrust_part2 [5; 6; 7; 8; 9] mem)
  in
  print_endline (string_of_int t1);
  print_endline (string_of_int t2)
*)

let () = print_endline "not yet implemented"
