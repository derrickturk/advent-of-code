use "buffered"
use "collections"

actor ElfWatcher
  var _thiself: U64 = 0
  var _elves: Array[U64] = Array[U64]

  be tally(calories: U64) =>
    _thiself = _thiself + calories

  be finish_tally() =>
    _elves.push(_thiself)
    _thiself = 0

  be query_top_n(n: USize, fn: {((Array[U64] box | None))} val) =>
    _elves = Sort[Array[U64], U64](_elves)
    if n > _elves.size() then
      fn(None)
    else
      fn(_elves.slice(_elves.size() - n))
    end

actor Main
  new create(env: Env) =>
    let ew = ElfWatcher

    env.input(
      object iso is InputNotify
        let _env: Env = env
        let _rdr: Reader iso = Reader
        var _fail: Bool = false

        fun ref apply(data: Array[U8] iso) =>
          _rdr.append(consume data)
          try
            while true do
              let line = _rdr.line()?
              if line == "" then
                ew.finish_tally()
              else
                try
                  ew.tally(line.u64()?)
                else
                  _fail = true
                  _env.input.dispose()
                end
              end
            end
          end

        fun ref dispose() =>
          if _fail then
            _env.err.print("parsing failed")
            _env.exitcode(1)
            return
          end
          ew.finish_tally() // get the last guy
          ew.query_top_n(3, {(result: (Array[U64] box | None)) =>
            match result
            | None =>
                _env.err.print("not enough data")
                _env.exitcode(1)
            | let top: Array[U64] box =>
                try
                  _env.out.print(top(2)?.string())
                  _env.out.print((top(0)? + top(1)? + top(2)?).string())
                end
            end
          })
      end)
