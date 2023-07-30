import std/os/env
import std/os/readline
import std/os/path

import error
import loader
import machine

/*
 * me, a simpleton: why can't this signature be
 * fun with-consoleio(action: () -> <intcode-io | e> a): <console, exn | e> a
 * you, an intellectual: https://github.com/koka-lang/koka/issues/164#issuecomment-863123552
 */
fun with-consoleio(action: () -> <intcode-io, console, exn | e> a
    ): <console, exn | e> a
  with handler
    fun input()
      match readline().parse-int
        Just(n) -> n
        Nothing -> throw("Invalid input", IOError)
    fun output(word)
      println(word)
  action()

pub fun main(): _ ()
  match get-args()
    /*
    [file] ->
      val cpu = load(file.path)
      with-consoleio { cpu.run(); () }
      ()
    */
    _ ->
      println("Usage: consoleio program.ic")
