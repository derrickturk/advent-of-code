use "collections"
use "files"
use "promises"

use "time"
use "debug"

use "intpony"

actor Main
  new create(env: Env) =>
    let path = try
      env.args(1)?
    else
      env.exitcode(1)
      let name = try env.args(0)? else "intpony07" end
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

    let p1 = problem1(mem, env)
    let p2 = problem2(mem, env)
    p1.next[None]({(max: I64)(env) =>
      env.out.print("Problem 1: " + max.string())
      p2.next[None]({(max: I64)(env) =>
        env.out.print("Problem 2: " + max.string())
      } iso)
    } iso)

  fun problem1(mem: Memory val, env: Env): Promise[I64] =>
    let ps = Array[Promise[I64]]

    Permuter[U8]([0; 1; 2; 3; 4]).for_each({(phases: Array[U8])(env) =>
      let p = Promise[I64]

      var cpus = Array[Cpu]
      for (i, phase) in phases.pairs() do
        let mem' = mem.clone()
        let cpu = Cpu(consume mem')

        if i > 0 then
          try
            cpus(cpus.size() - 1)?.subscribe(cpu)
          end
        end

        cpu.subscribe_crash({() =>
          p.reject()
          env.exitcode(1)
          env.err.print("cpu " + i.string() + " crashed")
        })

        cpu(phase.i64())

        cpus.push(cpu)
      end

      try
        let last_cpu = cpus(cpus.size() - 1)?
        last_cpu.subscribe(p)
        cpus(0)?(0)
      end

      for cpu in cpus.values() do
        cpu.run()
      end

      ps.push(p)
    })

    Promises[I64].join(ps.values()).next[I64]({(vals: Array[I64] val) =>
      var max: I64 = I64.min_value()
      for v in vals.values() do
        if v > max then
          max = v
        end
      end
      max
    })

  fun problem2(mem: Memory val, env: Env): Promise[I64] =>
    let ps = Array[Promise[I64]]

    Permuter[U8]([5; 6; 7; 8; 9]).for_each({(phases: Array[U8])(env) =>
      let p = Promise[I64]

      var cpus = Array[Cpu]
      for (i, phase) in phases.pairs() do
        let mem' = mem.clone()
        let cpu = Cpu(consume mem')

        if i > 0 then
          try
            cpus(cpus.size() - 1)?.subscribe(cpu)
          end
        end

        cpu.subscribe_crash({() =>
          p.reject()
          env.exitcode(1)
          env.err.print("cpu " + i.string() + " crashed")
        })

        cpu(phase.i64())

        cpus.push(cpu)
      end

      try
        let last_cpu = cpus(cpus.size() - 1)?
        Debug("run I even do?")
        last_cpu.subscribe(cpus(0)?)

        let last_tracker = LastTracker
        last_cpu.subscribe(last_tracker)
        last_cpu.subscribe_halt({()(last_tracker, p) =>
          last_tracker.query({(word: I64) =>
            Debug("do I even run")
            p(word)
          })
        })
      end

      try
        cpus(0)?(0)
      end

      for cpu in cpus.values() do
        cpu.run()
      end

      ps.push(p)
    })

    Promises[I64].join(ps.values()).next[I64]({(vals: Array[I64] val) =>
      var max: I64 = I64.min_value()
      for v in vals.values() do
        if v > max then
          max = v
        end
      end
      max
    })

actor LastTracker
  var _last: I64 = I64.min_value()

  be apply(word: I64) =>
    _last = word

  be query(fn: {(I64)} val) =>
    fn(_last)

class Permuter[A]
  var _vals: Array[A]

  new create(vals: Array[A] iso) =>
    _vals = consume vals

  fun ref for_each(fn: {ref (Array[A])}) =>
    if _vals.size() > 0 then
      _for_each(fn, _vals.size())
    end

  fun ref _for_each(fn: {ref (Array[A])}, k: USize) =>
    if k == 1 then
      fn(_vals)
    else
      _for_each(fn, k - 1)

      for i in Range(0, k - 1) do
        if (k % 2) == 0 then
          try
            _vals.swap_elements(i, k - 1)?
          end
        else
          try
            _vals.swap_elements(0, k - 1)?
          end
        end
        _for_each(fn, k - 1)
      end
    end
