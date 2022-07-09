open Intcode

let mem_of_string_exn words = words
  |> String.split_on_char ','
  |> List.map (fun s -> s |> String.trim |> int_of_string)
  |> Array.of_list

let mem_of_string_opt words = try
  Some (mem_of_string_exn words)
with
  _ -> None

let cpu_of_string_exn words = { ip = 0; mem = mem_of_string_exn words }

let cpu_of_string_opt words = try
  Some (cpu_of_string_exn words)
with
  _ -> None
