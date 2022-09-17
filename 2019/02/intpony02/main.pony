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
    mem1(1) = 12
    mem1(2) = 2
    let cpu1 = Cpu(consume mem1)
    cpu1.on_completion(object iso
      let env: Env = env

      fun ref halted() =>
        cpu1.query_memory(0, {(word: I64) =>
          env.out.print(word.string())
        })

      fun ref illegal_instruction() =>
        env.exitcode(1)
        env.err.print("illegal instruction")
    end)
    cpu1.run()

    var i: I64 = 0
    while i < 100 do
      var j: I64 = 0
      while j < 100 do
        let mem2 = mem.clone()
        mem2(1) = i
        mem2(2) = j
        let cpu2 = Cpu(consume mem2)
        cpu2.on_completion(object iso
          let env: Env = env
          let i: I64 = i
          let j: I64 = j

          fun ref halted() =>
            cpu2.query_memory(0, {(word: I64) =>
              if word == 19690720 then
                env.out.print("found " + ((100 * i) + j).string())
              end
            })

          fun ref illegal_instruction() => None
        end)
        cpu2.run()

        j = j + 1
      end
      i = i + 1
    end
