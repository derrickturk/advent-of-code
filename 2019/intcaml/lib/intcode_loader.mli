val mem_of_string_opt: string -> int array option
val mem_of_string_exn: string -> int array
(* should we save these names for a true "dump memory" operation? *)
val cpu_of_string_opt: string -> Cpu.t option
val cpu_of_string_exn: string -> Cpu.t
