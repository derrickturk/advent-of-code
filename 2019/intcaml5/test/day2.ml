open Intcaml5
open Intcaml5.Cpu

module M = Machine.Make (Io.Null)

let () =
  let str = "1,9,10,3,2,3,11,0,99,30,40,50" in
  let mem = Memory.init [1;9;10;3;2;3;11;0;99;30;40;50] in
  assert Memory.(equal (of_string_exn str) mem);
  assert Cpu.(equal (of_string_exn str) (init mem))

let should_run_to s0 s1 =
  let cpu = Cpu.of_string_exn s0 in
  let final_mem = Memory.of_string_exn s1 in
  assert (M.run cpu () = Ok ());
  assert (Memory.equal cpu.mem final_mem)

let () =
  should_run_to
    "1,9,10,3,2,3,11,0,99,30,40,50"
    "3500,9,10,70,2,3,11,0,99,30,40,50";
  should_run_to "1,0,0,0,99" "2,0,0,0,99";
  should_run_to "2,3,0,3,99" "2,3,0,6,99";
  should_run_to "2,4,4,5,99,0" "2,4,4,5,99,9801";
  should_run_to "1,1,1,4,99,5,6,0,99" "30,1,1,4,2,5,6,0,99"
