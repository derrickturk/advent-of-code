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

    let mem1 = mem.clone()
    let cpu1 = Cpu(consume mem1)

    cpu1.subscribe(object
      let env: Env = env

      be send(word: I64) =>
        _print(word)

      fun _print(word: I64) =>
        env.out.print(word.string())
    end)

    cpu1.send(1)

    cpu1.subscribe_crash({() =>
      env.exitcode(1)
      env.err.print("illegal instruction")
    })

    cpu1.run()

    let mem2 = mem.clone()
    let cpu2 = Cpu(consume mem2)

    cpu2.subscribe(object
      let env: Env = env

      be send(word: I64) =>
        _print(word)

      fun _print(word: I64) =>
        env.out.print(word.string())
    end)

    cpu2.send(5)

    cpu2.subscribe_crash({() =>
      env.exitcode(2)
      env.err.print("illegal instruction")
    })

    cpu2.run()
