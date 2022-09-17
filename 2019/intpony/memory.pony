use "itertools"

type _Mode is (Mem | Rel | Imm)

class Memory
  var _memory: Array[I64]

  new create(image: Array[I64]) =>
    _memory = image

  /* because Pony doesn't do any sort of escape analysis, this ctor is pretty
   *   useless. why?
   *   - we have to pass a ref because of the definition of Iterator
   *   - but Pony doesn't know that we don't hang onto that ref anywhere
   *   - we COULD pass in a iso by consuming it, then recover the result to
   *       iso or val, but
   *   - to use it with FileLines, we need to get a FileLines iso, the FileLines
   *       ctor returns FileLines ref^ and takes File ref, which means that we
   *       can't recover a FileLines iso from that
   */
  new from_lines(lines: Iterator[String iso^] ref)? =>
    _memory = Array[I64]
    for line in lines do
      for word in line.split(",").values() do
        try
          _memory.push(word.i64()?)
        else
          error
        end
      end
    end

  /* I had to print a cheat-sheet to write this,
   *   and Array.clone's limitations make it worse than useless for this:
   *   it's an attractive nuisance.
   */
  fun box clone(): Memory iso^ =>
    let sz = _memory.size()
    let memory' = recover Array[I64](sz) end
    for m in _memory.values() do
      memory'.push(m)
    end
    recover Memory(consume memory') end

  fun box apply(i: USize val): I64 =>
    try
      _memory(i)?
    else
      0
    end

  fun ref update(i: USize val, value: I64) =>
    if i >= _memory.size() then
      _memory.concat(Iter[I64].repeat_value(0) where len = i - _memory.size())
    end
    try
      // unTODOable: panic, etc
      _memory(i)? = value
    end

  fun box decode(ip: USize val): (Instruction, USize)? =>
    let word = this(ip)
    let op = word % 100
    let mode1 = _DecodeMode((word / 100) % 10)?
    let mode2 = _DecodeMode((word / 1000) % 10)?
    let mode3 = _DecodeMode((word / 10000) % 10)?
    match op
    | 1 => ((Add,
        _decode_src(ip + 1, mode1),
        _decode_src(ip + 2, mode2),
        _decode_dst(ip + 3, mode3)?
      ), ip + 4)
    | 2 => ((Mul,
        _decode_src(ip + 1, mode1),
        _decode_src(ip + 2, mode2),
        _decode_dst(ip + 3, mode3)?
      ), ip + 4)
    | 3 => ((Inp,
        _decode_dst(ip + 1, mode1)?
      ), ip + 2)
    | 4 => ((Out,
        _decode_src(ip + 1, mode1)
      ), ip + 2)
    | 5 => ((Jnz,
        _decode_src(ip + 1, mode1),
        _decode_src(ip + 2, mode2)
      ), ip + 3)
    | 6 => ((Jz,
        _decode_src(ip + 1, mode1),
        _decode_src(ip + 2, mode2)
      ), ip + 3)
    | 7 => ((Lt,
        _decode_src(ip + 1, mode1),
        _decode_src(ip + 2, mode2),
        _decode_dst(ip + 3, mode3)?
      ), ip + 4)
    | 8 => ((Eq,
        _decode_src(ip + 1, mode1),
        _decode_src(ip + 2, mode2),
        _decode_dst(ip + 3, mode3)?
      ), ip + 4)
    | 9 => ((Arb,
        _decode_src(ip + 1, mode1)
      ), ip + 2)
    | 99 => (Hlt, ip + 1)
    else
      error
    end

  fun box _decode_src(ptr: USize val, mode: _Mode): Src =>
    match mode
    | Mem => (Mem, this(ptr))
    | Rel => (Rel, this(ptr))
    | Imm => (Imm, this(ptr))
    end

  fun box _decode_dst(ptr: USize val, mode: _Mode): Dst? =>
    match mode
    | Mem => (Mem, this(ptr))
    | Rel => (Rel, this(ptr))
    else
      error
    end

primitive  _DecodeMode
  fun apply(digit: I64): _Mode? =>
    match digit
    | 0 => Mem
    | 1 => Imm
    | 2 => Rel
    else
      error
    end
