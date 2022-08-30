use "files"

primitive Day1
  fun part1(env: Env, values: Array[U64]) =>
    var count: U64 = 0
    var last: (U64 | None) = None
    for value in values.values() do
      match last
        | let last': U64 =>
            if value > last' then
              count = count + 1
            end
      end
      last = value
    end
    env.out.print(count.string())

  fun part2(env: Env, values: Array[U64]) =>
    var count: U64 = 0
    var prev3: (U64 | None) = None
    var prev2: (U64 | None) = None
    var prev1: (U64 | None) = None
    for value in values.values() do
      match (prev3, prev2, prev1)
        | (let prev3': U64, let prev2': U64, let prev1': U64) =>
            if (value + prev1' + prev2') > (prev1' + prev2' + prev3') then
              count = count + 1
            end
      end
      prev3 = prev2
      prev2 = prev1
      prev1 = value
    end
    env.out.print(count.string())

actor Part1
  var _count: U64 = 0
  var _last: (U64 | None) = None

  be see(value: U64) =>
    match _last
      | let last: U64 =>
          if value > last then
            _count = _count + 1
          end
    end
    _last = value

  be query(fn: {(U64)} val) =>
    fn(_count)

actor Part2
  var _count: U64 = 0
  var _prev3: (U64 | None) = None
  var _prev2: (U64 | None) = None
  var _prev1: (U64 | None) = None

  be see(value: U64) =>
    match (_prev3, _prev2, _prev1)
      | (let prev3: U64, let prev2: U64, let prev1: U64) =>
          if (value + prev1 + prev2) > (prev1 + prev2 + prev3) then
            _count = _count + 1
          end
    end
    _prev3 = _prev2
    _prev2 = _prev1
    _prev1 = value

  be query(fn: {(U64)} val) =>
    fn(_count)

actor Main
  new create(env: Env) =>
    let path = try
      env.args(1)?
    else
      env.exitcode(1)
      env.err.print("Usage: sony input-file")
      return
    end

    let file = match OpenFile(FilePath(FileAuth(env.root), path))
    | let f: File => FileLines(f)
    else
      env.exitcode(1)
      env.err.print("Unable to read \"" + path + "\"")
      return
    end

    let part1 = Part1
    let part2 = Part2
    for line in file do
      let reading = try
        line.u64()?
      else
        env.exitcode(1)
        env.err.print("Bad reading: \"" + consume line + "\"")
        return
      end
      part1.see(reading)
      part2.see(reading)
    end

    // amusingly, sometimes Part 2 finishes before Part 1
    part1.query({(total: U64) => env.out.print("Part 1: " + total.string())})
    part2.query({(total: U64) => env.out.print("Part 2: " + total.string())})
