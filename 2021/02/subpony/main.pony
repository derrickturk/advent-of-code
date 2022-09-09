use "buffered"
// use "debug"

primitive Up
primitive Down
primitive Forward

type Dir is (Up | Down | Forward)
type Cmd is (Dir, U8)

primitive Parser
  fun parse(line: String val): Cmd? =>
    let split: Array[String] = line.split(" ")
    if split.size() != 2 then error end
    let dir = match split(0)?
    | "up" => Up
    | "down" => Down
    | "forward" => Forward
    else
      error
    end
    let units = split(1)?.u8()?
    (dir, units)

actor BadComputer
  var _x: I64 = 0
  var _y: I64 = 0

  be step(cmd: Cmd) =>
    (let dir, let n) = cmd
    let n' = n.i64()
    match dir
    | Up => _y = _y - n'
    | Down => _y = _y + n'
    | Forward => _x = _x + n'
    end

  be query(fn: {(I64, I64)} val) =>
    fn(_x, _y)

actor GoodComputer
  var _x: I64 = 0
  var _y: I64 = 0
  var _aim: I64 = 0

  be step(cmd: Cmd) =>
    (let dir, let n) = cmd
    let n' = n.i64()
    match dir
    | Up => _aim = _aim - n'
    | Down => _aim = _aim + n'
    | Forward =>
        _x = _x + n'
        _y = _y + (_aim * n')
    end

  be query(fn: {(I64, I64, I64)} val) =>
    fn(_x, _y, _aim)

actor Main
  new create(env: Env) =>
    let bc = BadComputer
    let gc = GoodComputer
    env.input(
      object iso is InputNotify
        let _rdr: Reader iso = Reader
        var _fail: Bool = false

        fun ref apply(data: Array[U8] iso) =>
          _rdr.append(consume data)
          try
            while true do
              let line: String val = _rdr.line()?
              try
                let cmd: Cmd = Parser.parse(line)?
                bc.step(cmd)
                gc.step(cmd)
              else
                _fail = true
                env.exitcode(1)
                env.err.print("Unable to parse \"" + line + "\"")
                env.input.dispose()
              end
            end
          end

        fun ref dispose() =>
          if _fail then
            return
          end
          bc.query({(x: I64, y: I64) =>
            env.out.print((x * y).string())
            gc.query({(x: I64, y: I64, aim: I64) =>
              env.out.print((x * y).string())
            })
          })
      end)
