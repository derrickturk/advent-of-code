import std/num/int64
import std/num/float64

abstract type memory<h>
  Memory(vec: ref<h, vector<int>>)

pub fun init(mem: vector<int>): alloc<h> memory<h>
  Memory(ref(mem))

pub fun @index(^mem: memory<h>, ^index: int): <exn, read<h>> int
  if index < (!mem.vec).length
    then (!mem.vec)[index]
    else 0

pub fun assign/@index(^mem: memory<h>, ^index: int, word: int): <exn, read<h>, write<h>> ()
  with v <- modify(mem.vec)
  if index >= v.length then
    val new_len = (int64(index).float64 * 1.5).int
    v := copy_with_len(v, new_len)
  v[index] := word

fun copy_with_len(^v: vector<int>, ^new_len: int): vector<int>
  vector-init(v.length, fn(i) { try({v[i]}, fn(_) {0}) })
