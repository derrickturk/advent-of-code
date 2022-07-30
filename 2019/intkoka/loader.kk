import std/os/path
import std/os/file

import error
import machine

pub fun load(path: path): <exn, fsys, alloc<h>> machine<h>
  val source = read-text-file(path)
  val words = source.lines.filter(fn (l) -> l != "").flatmap fn (l)
    l.split(",").map fn (w)
      match parse-int(w)
        Just(n) -> n
        Nothing -> throw("Invalid program", InvalidProgram(path.show))
  init(words)
