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
    let mk = MaxKeeper
    let p = Promise[I64]
    let waiter = CpuWaiter({()(p) =>
      mk.query({(max: (I64 | None)) =>
        match max
        | let m: I64 => p(m)
        else
          p.reject()
        end
      })
    })

    Permuter[U8]([0; 1; 2; 3; 4]).for_each({(phases: Array[U8])(env) =>
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
          env.exitcode(1)
          env.err.print("cpu " + i.string() + " crashed")
        })

        cpu.send(phase.i64())

        cpus.push(cpu)
      end

      try
        cpus(cpus.size() - 1)?.subscribe(mk)
        waiter.wait(cpus(cpus.size() - 1)?)
      end

      try
        cpus(0)?.send(0)
      end

      for cpu in cpus.values() do
        Debug("RUN")
        cpu.run()
      end
    })
    waiter.start_waiting()
    p

  fun problem2(mem: Memory val, env: Env): Promise[I64] =>
    let mk = MaxKeeper
    let p = Promise[I64]
    let waiter = CpuWaiter({()(p) =>
      mk.query({(max: (I64 | None)) =>
        match max
        | let m: I64 => p(m)
        else
          p.reject()
        end
      })
    })

    Permuter[U8]([5; 6; 7; 8; 9]).for_each({(phases: Array[U8])(env, mk) =>
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
          env.exitcode(1)
          env.err.print("cpu " + i.string() + " crashed")
        })

        cpu.send(phase.i64())

        cpus.push(cpu)
      end

      let lk = LastKeeper
      try
        cpus(cpus.size() - 1)?.subscribe(cpus(0)?)
        cpus(cpus.size() - 1)?.subscribe(lk)
        cpus(cpus.size() - 1)?.subscribe_halt({()(mk) =>
          lk.query({(word: (I64 | None)) =>
            match word
            | let word': I64 => mk.send(word')
            end
          })
        })
        waiter.wait(cpus(cpus.size() - 1)?)
      end

      try
        cpus(0)?.send(0)
      end

      for cpu in cpus.values() do
        Debug("RUN")
        cpu.run()
      end
    })
    waiter.start_waiting()
    p

actor LastKeeper
  var _last: (I64 | None) = None

  be send(word: I64) =>
    _last = word

  be query(fn: {((I64 | None))} val) =>
    fn(_last)

actor MaxKeeper
  var _max: (I64 | None) = None

  be send(word: I64) =>
    _max = match _max
    | let m: I64 => m.max(word)
    else
      word
    end

  be query(fn: {((I64 | None))} val) =>
    fn(_max)

actor CpuWaiter
  var _waiting: SetIs[Cpu tag] = SetIs[Cpu tag]
  var _wait: Bool = false
  let _when_done: {()} val

  new create(when_done: {()} val) =>
    _when_done = when_done

  be wait(cpu: Cpu tag) =>
    _waiting.set(cpu)
    cpu.subscribe_halt({()(self = recover tag this end) =>
      self._done(cpu)
    })
    cpu.subscribe_crash({()(self = recover tag this end) =>
      self._done(cpu)
    })

  be start_waiting() =>
    _wait = true
    if _waiting.size() == 0 then
      Debug("already done, triggering")
      _when_done()
    end

  be _done(cpu: Cpu tag) =>
    _waiting.unset(cpu)
    if _wait and (_waiting.size() == 0) then
      Debug("done, triggering")
      _when_done()
    end

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
