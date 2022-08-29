use "files"

actor Main
  new create(env: Env) =>
    try
      let input = FilePath(FileAuth(env.root), env.args(1)?)
      match OpenFile(input)
        | let file: File =>
            let lines = FileLines(file)
            var count: U64 = 0
            var last: (U64 | None) = None
            for l in lines do
              let value = l.u64()?
              match last
                | let last': U64 =>
                    if value > last' then
                      count = count + 1
                    end
              end
              last = value
            end
            env.out.print(count.string())
        else
          error
        end
    else
      env.err.print("well, that sucked")
    end
