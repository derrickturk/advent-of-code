import std/os/env
import std/os/readline
import std/os/path

import error
import loader
import machine

fun with-consoleio(action)
  with handler
    ctl input()
      match readline().parse-int
        Just(n) -> resume(n)
        Nothing -> throw("Invalid input", IOError)
    ctl output(word)
      resume(println(word))
  action()

pub fun main(): _ ()
  match get-args()
    [file] ->
      val cpu = load(file.path)
      with-consoleio { cpu.run(); () }
      ()
    _ ->
      println("Usage: consoleio program.ic")
