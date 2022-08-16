open Intcaml5

let thrust (p0, p1, p2, p3, p4) mem =
  let m0_in = Aio.create_xport ~contents:p0 () in
  let m1_in = Aio.create_xport ~contents:p1 () in
  let m2_in = Aio.create_xport ~contents:p2 () in
  let m3_in = Aio.create_xport ~contents:p3 () in
  let m4_in = Aio.create_xport ~contents:p4 () in
  let m4_out = Aio.create_xport () in
  let m0_io: Aio.xports = { input = m0_in; output = m1_in } in
  let m1_io: Aio.xports = { input = m1_in; output = m2_in } in
  let m2_io: Aio.xports = { input = m2_in; output = m3_in } in
  let m3_io: Aio.xports = { input = m3_in; output = m4_in } in
  let m4_io: Aio.xports = { input = m4_in; output = m4_out } in
  let manual: Aio.xports = { input = m0_in; output = m0_in } in
  let run io = (io, fun () ->
    match Machine.run (Cpu.init (Memory.copy mem)) with
      | Ok () -> ()
      | Error _ -> failwith "error"
  ) in
  let m_all = [ run m0_io
              ; run m1_io
              ; run m2_io
              ; run m3_io
              ; run m4_io
              ; (manual, fun () ->
                  match Aio.write 0 with
                    | Ok () -> ()
                    | Error _ -> failwith "error"
                )
              ]
  in
  Aio.with_xport_io m_all;
  Aio.read_xport m4_out

let test_cases =
  [ ( (4, 3, 2, 1, 0)
    , "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
    , 43210
    )
  ; ( (0, 1, 2, 3, 4)
    , "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
    , 54321
    )
  ; ( (1, 0, 4, 3, 2)
    , "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
    , 65210
    )
  ]

let do_test (init, mem, result) =
  let mem = Memory.of_string_exn mem in
  thrust init mem = Some result

let () = assert (List.for_all do_test test_cases)
