abstract type memory<h>
  Memory(vec: ref<h, vector<int>>)

pub fun init(mem: vector<int>): alloc<h> memory<h>
  Memory(ref(mem))

pub fun [](^mem: memory<h>, ^index: int): <exn, read<h>> int
  if index < (!mem.vec).length
    then (!mem.vec)[index]
    else 0

pub fun [](^mem: memory<h>, ^index: int, word: int): <exn, read<h>, write<h>> ()
  with v <- modify(mem.vec)
  if index >= v.length then
    val new_len = (index.float64 * 1.5).int
    v := copy_with_len(v, new_len)
  v[index] := word

fun copy_with_len(^v: vector<int>, ^new_len: int): vector<int>
  vector-init(v.length, fn(i) { try({v[i]}, fn(_) {0}) })
