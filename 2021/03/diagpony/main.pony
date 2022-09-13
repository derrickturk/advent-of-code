use "buffered"
use "itertools"

primitive FromBinary
  fun apply(arr: Array[Bool] val): U32 =>
    var ret: U32 = 0
    for b in arr.values() do
      ret = ret << 1
      if b then
        ret = ret or 1
      end
    end
    ret

actor BitTally
  var _count_ones: Array[U32] = Array[U32]
  var _lines: U32 = 0

  be see(line: String val) =>
    while line.size() > _count_ones.size() do
      _count_ones.push(0)
    end

    for (i, c) in Iter[U8](line.values()).enum() do
      if c == '1' then
        try // can't fail
          _count_ones(i)? = _count_ones(i)? + 1
        end
      end
    end

    _lines = _lines + 1

  be query(fn: {(U32, U32)} val) =>
    fn(most_popular(), least_popular())

  fun most_popular(): U32 =>
    let ret: Array[Bool] iso = Array[Bool].init(false, _count_ones.size())
    for (i, count) in _count_ones.pairs() do
      if count >= (_lines / 2) then
        try
          ret(i)? = true
        end
      end
    end
    FromBinary(consume ret)

  fun least_popular(): U32 =>
    let ret: Array[Bool] iso = Array[Bool].init(false, _count_ones.size())
    for (i, count) in _count_ones.pairs() do
      if count < (_lines / 2) then
        try
          ret(i)? = true
        end
      end
    end
    FromBinary(consume ret)

actor Main
  new create(env: Env) =>
    let tally = BitTally
    env.input(
      object iso is InputNotify
        let _rdr: Reader iso = Reader

        fun ref apply(data: Array[U8] iso) =>
          _rdr.append(consume data)
          try
            while true do
              let line: String val = _rdr.line()?
              tally.see(line)
            end
          end

        fun ref dispose() =>
          let o = env.out
          tally.query({(most: U32, least: U32) =>
            o.print((most * least).string())
            /*
            gc.query({(x: I64, y: I64, aim: I64) =>
              env.out.print((x * y).string())
            })
            */
          })
      end)
