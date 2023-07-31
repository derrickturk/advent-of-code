import std/os/env
import std/os/readline
import std/os/path

import error
import loader
import machine
import queue

// alias iotask<a, e> = a -> <intcode-io | e> ()
alias mailbox<h> = ref<h, maybe<int>>

fun run-on(
    task-queue: ref<h, queue<() -> <intcode-io, st<h>, div | e> ()>>,
    inbox: mailbox<h>,
    outbox: mailbox<h>,
    task: () -> <intcode-io, st<h>, div | e> ()): <st<h>, div | e> ()

  fun next(): <intcode-io | _> ()
    match (!task-queue).pop
      Just((t, q)) ->
        task-queue := q
        t()
      Nothing ->
        ()

  fun defer(t: () -> <intcode-io | _> ()): _ ()
    task-queue := (!task-queue).push(t)

  fun try-read(k: int -> <intcode-io | _> ()): <intcode-io | _> ()
    match !inbox
      Just(n) ->
        inbox := Nothing
        k(n)
      Nothing ->
        defer({ try-read(k) })
        next()

  fun try-write(word: int, k: () -> <intcode-io | _> ()): <intcode-io | _> ()
    match !outbox
      Just(_) ->
        defer({ try-write(word, k) })
        next()
      Nothing ->
        outbox := Just(word)
        k()

  with handler
    return(_)
      next()
    ctl input()
      try-read(resume)
    ctl output(word)
      try-write(word, { resume(()) })

  task()

pub fun main(): _ ()
  match get-args()
    [file] ->
      val task-queue = ref(queue())
      val cpu = load(file.path)
      val inbox = ref(Nothing)
      val outbox = ref(Nothing)
      run-on(task-queue, inbox, outbox) { cpu.run(); () }
      ()
    _ ->
      println("Usage: asyncio program.ic")
