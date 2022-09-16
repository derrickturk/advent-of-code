use "files"

use "intpony"

actor Main
  new create(env: Env) =>
    let path = try
      env.args(1)?
    else
      env.exitcode(1)
      let name = try env.args(0)? else "intpony02" end
      env.err.print("Usage: " + name + " code-file")
      return
    end

    let file = match OpenFile(FilePath(FileAuth(env.root), path))
    | let f: File => FileLines(f)
    else
      env.exitcode(1)
      env.err.print("Unable to read \"" + path + "\"")
      return
    end

    let code: Array[I64] trn = Array[I64]
    for line in file do
      for word in line.split(",").values() do
        try
          code.push(word.i64()?)
        else
          env.exitcode(1)
          env.err.print("Unable to parse \"" + word + "\"")
          return
        end
      end
    end
    let code': Array[I64] val = consume code

    problem1(code', env)
    problem2(code', env)

  fun problem1(code: Array[I64] val, env: Env) =>
    // TODO: permutations (LOL)
    var cpus = Array[Cpu]
    for (i, phase) in [as U8: 0; 1; 2; 3; 4].pairs() do
      let mem: Memory iso = Memory(code)
      let cpu = Cpu(consume mem)

      if i > 0 then
        try
          cpus(cpus.size() - 1)?.subscribe(cpu)
        end
      end

      cpu.on_completion(object iso
        fun ref halted() =>
          None

        fun ref illegal_instruction() =>
          env.exitcode(1)
          env.err.print("cpu " + i.string() + " crashed")
      end)

      cpu.send(phase.i64())

      cpus.push(cpu)

      cpu.run()
    end

    try
      cpus(cpus.size() - 1)?.subscribe(object
        be send(word: I64) =>
          _print(word)

        fun _print(word: I64) =>
          env.out.print(word.string())
      end)
    end

    try
      cpus(0)?.send(0)
    end

  fun problem2(code: Array[I64] val, env: Env) =>
    // TODO: permutations (LOL)
    var cpus = Array[Cpu]
    for (i, phase) in [as U8: 5; 6; 7; 8; 9].pairs() do
      let mem: Memory iso = Memory(code)
      let cpu = Cpu(consume mem)

      if i > 0 then
        try
          cpus(cpus.size() - 1)?.subscribe(cpu)
        end
      end

      cpu.on_completion(object iso
        fun ref halted() =>
          None

        fun ref illegal_instruction() =>
          env.exitcode(1)
          env.err.print("cpu " + i.string() + " crashed")
      end)

      cpu.send(phase.i64())

      cpus.push(cpu)

      cpu.run()
    end

    /* you'd fuckin' think
    var outputs = Array[I64]
    try
      cpus(cpus.size() - 1)?.subscribe(cpus(0)?)
      cpus(cpus.size() - 1)?.subscribe(object
        be send(word: I64) =>
          _push(word)

        fun _push(word: I64) =>
          outputs.push(word)
      end)

      cpus(cpus.size() - 1)?.on_completion(object iso
        fun ref halted() =>
          try
            env.out.print(outputs(outputs.size() - 1).string())
          end

        fun ref illegal_instruction() =>
          env.exitcode(1)
          env.err.print("cpu " + (cpus.size() - 1).string() + " crashed")
      end)
    end
    */

    try
      cpus(cpus.size() - 1)?.subscribe(cpus(0)?)
      cpus(cpus.size() - 1)?.subscribe(object
        be send(word: I64) =>
          _print(word)

        fun _print(word: I64) =>
          env.out.print(word.string())
      end)
    end

    try
      cpus(0)?.send(0)
    end
