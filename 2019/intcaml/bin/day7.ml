open Intcaml

module M = Machine.Make (Io.Lwt_mvar)

exception Intcode_error of Intcaml.Error.t

let thrust (p0, p1, p2, p3, p4) mem =
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
      (M.run { ip = 0; mem = Array.copy mem } io))
  in
  let m_all = [ run m0_io
              ; run m1_io
              ; run m2_io
              ; run m3_io
              ; run m4_io
              ; Lwt_mvar.put m0_in 0
              ]
  in Lwt.bind (Lwt.join m_all) (fun _ -> Lwt_mvar.take m4_out)

let () =
  let init = (4, 3, 2, 1, 0) in
  let mem = [|3;15;3;16;1002;16;10;16;1;16;15;15;4;15;99;0;0|] in
  let t = Lwt_main.run (thrust init mem) in
  print_endline (string_of_int t)
