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
    task-queue: ref<_, queue<() -> <intcode-io | _> ()>>,
    inbox: mailbox<_>,
    outbox: mailbox<_>,
    task: () -> <intcode-io | _> ())

  fun next(): _ ()
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

  fun try-write(word, k: () -> <intcode-io | _> ()): <intcode-io | _> ()
    match !outbox
      Just(_) ->
        defer({ try-write(word, k) })
        next()
      Nothing ->
        outbox := Just(word)
        k()

  /*
  with handler
    return(_)
      next()
    ctl input()
      try-read(resume)
    ctl output(word)
      try-write(word, resume)
  */

  next()

/*
fun with-mailboxes(inbox: mailbox<h>, outbox: mailbox<h>,
    action: task<h, e>): <st<h>, div | e> ()
  match !action
    Just(a) ->
      with handler
        return(_)
          action := Nothing;
          ()
        ctl input()
          match !inbox
            Just(n) ->
              inbox := Nothing
              resume(n)
            Nothing ->
              // TODO
              action := Nothing
              ()
        ctl output(word)
          match !outbox
            Just(_) ->
              ()
            Nothing ->
              outbox := Just(word)
              resume(())
      a()
    Nothing -> ()
*/

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
