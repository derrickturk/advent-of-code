open Effect
open Effect.Deep

type xport = int option ref
type xports = { input: xport; output: xport }

let create_xport ?contents () = ref contents
let read_xport xp = !xp
let read () = perform Machine.Input
let write word = perform (Machine.Output word)

let run_on task_queue { input; output } f init =
  let defer task = Queue.add task task_queue in
  let next () = try Queue.take task_queue () with Queue.Empty -> () in
  let rec try_read k = match !input with
    | Some word ->
        input := None;
        continue k (Ok word)
    | None ->
        if Queue.is_empty task_queue
          then continue k (Error (Error.IOError `Input))
          else defer (fun () -> try_read k)
        ;
        next ()
  in
  let rec try_write word k = match !output with
    | Some _ ->
        if Queue.is_empty task_queue
          then continue k (Error (Error.IOError `Output))
          else begin
            defer (fun () -> try_write word k);
            next ()
          end
    | None ->
        output := Some word;
        continue k (Ok ())
  in
  match_with f init
    { retc = next
    ; exnc = raise
    ; effc = begin fun (type a) (eff: a t) -> match eff with
        | Machine.Input -> Some (
            fun (k: (a, _) continuation) -> try_read k
          )
        | Machine.Output word -> Some (
            fun (k: (a, _) continuation) -> try_write word k
          )
        | _ -> None
      end
    }

let with_xport_io tasks =
  let pending = Queue.create () in
  List.iter
    (fun (ports, task) ->
      Queue.add (fun () -> run_on pending ports task ()) pending)
    tasks;
  try Queue.take pending () with _ -> ()
