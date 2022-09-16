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

    /* so much FUCKING BULLSHIT to fill a fucking array
     *   and send a const* to a constructor
     */
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

    let mem1: Memory iso = Memory(code')
    let cpu1 = Cpu(consume mem1)

    cpu1.subscribe(object
      let env: Env = env

      be send(word: I64) =>
        _print(word)

      fun _print(word: I64) =>
        env.out.print(word.string())
    end)

    cpu1.send(1)

    cpu1.on_completion(object iso
      let env: Env = env

      fun ref halted() =>
        None

      fun ref illegal_instruction() =>
        env.exitcode(1)
        env.err.print("illegal instruction")
    end)

    cpu1.run()

    let mem2: Memory iso = Memory(code')
    let cpu2 = Cpu(consume mem2)

    cpu2.subscribe(object
      let env: Env = env

      be send(word: I64) =>
        _print(word)

      fun _print(word: I64) =>
        env.out.print(word.string())
    end)

    cpu2.send(5)

    cpu2.on_completion(object iso
      let env: Env = env

      fun ref halted() =>
        None

      fun ref illegal_instruction() =>
        env.exitcode(2)
        env.err.print("illegal instruction")
    end)

    cpu2.run()
