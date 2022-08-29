use "files"
use "itertools"

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

actor Main
  new create(env: Env) =>
    try
      let input = FilePath(FileAuth(env.root), env.args(1)?)
      let nums = match OpenFile(input)
        | let file: File =>
            Iter[String](FileLines(file))
              // TODO: this isn't quite the error handling we want
              .map[U64]({(l): U64? => l.u64()?})
              .collect(Array[U64])
        else
          error
        end
      Day1.part1(env, nums)
      Day1.part2(env, nums)
    else
      env.err.print("well, that sucked")
    end
