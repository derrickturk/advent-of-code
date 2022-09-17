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

    let code = recover Array[I64] end
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
    let mem = recover val Memory(consume code) end

    problem1(mem, env)
    problem2(mem, env)

  fun problem1(mem: Memory val, env: Env) =>
    // TODO: permutations (LOL)
    var cpus = Array[Cpu]
    for (i, phase) in [as U8: 0; 1; 2; 3; 4].pairs() do
      let mem' = mem.clone()
      let cpu = Cpu(consume mem')

      if i > 0 then
        try
          cpus(cpus.size() - 1)?.subscribe(cpu)
        end
      end

      cpu.subscribe_crash({() =>
        env.exitcode(1)
        env.err.print("cpu " + i.string() + " crashed")
      })

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

  fun problem2(mem: Memory val, env: Env) =>
    // TODO: permutations (LOL)
    var cpus = Array[Cpu]
    for (i, phase) in [as U8: 5; 6; 7; 8; 9].pairs() do
      let mem' = mem.clone()
      let cpu = Cpu(consume mem')

      if i > 0 then
        try
          cpus(cpus.size() - 1)?.subscribe(cpu)
        end
      end

      cpu.subscribe_crash({() =>
        env.exitcode(1)
        env.err.print("cpu " + i.string() + " crashed")
      })

      cpu.send(phase.i64())

      cpus.push(cpu)

      cpu.run()
    end

    let lk = LastKeeper
    try
      cpus(cpus.size() - 1)?.subscribe(cpus(0)?)
      cpus(cpus.size() - 1)?.subscribe(lk)
      cpus(cpus.size() - 1)?.subscribe_halt({()(env) =>
        lk.query({(word: (I64 | None)) =>
          match word
          | let word': I64 => env.out.print("part 2: " + word'.string())
          else
            None
          end
        })
      })
    end

    try
      cpus(0)?.send(0)
    end

actor LastKeeper
  var _last: (I64 | None) = None

  be send(word: I64) =>
    _last = word

  be query(fn: {((I64 | None))} val) =>
    fn(_last)
