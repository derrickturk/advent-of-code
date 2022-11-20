use "collections"

interface Sendable
  fun tag apply(word: I64)

primitive Running
primitive Blocked
primitive Halted
primitive IllegalInstruction

type Status is (Running | Blocked | Halted | IllegalInstruction)

actor Cpu is Sendable
  var _memory: Memory
  var _ip: USize = 0
  var _rb: I64 = 0
  var _input: Array[I64 val] = Array[I64 val]
  var _status: Status = Running
  var _subscribers: SetIs[Sendable tag] = SetIs[Sendable tag]
  var _on_halt: SetIs[{ref ()} iso] = SetIs[{ref ()} iso]
  var _on_crash: SetIs[{ref ()} iso] = SetIs[{ref ()} iso]
  var _running: Bool = false

  new create(memory: Memory iso) =>
    _memory = consume memory

  be subscribe(rcvr: Sendable tag) =>
    _subscribers.set(rcvr)

  be unsubscribe(rcvr: Sendable tag) =>
    _subscribers.unset(rcvr)

  be subscribe_halt(notify: {ref ()} iso) =>
    _on_halt.set(consume notify)

  be unsubscribe_halt(notify: {ref ()} tag) =>
    _on_halt.unset(notify)

  be subscribe_crash(notify: {ref ()} iso) =>
    _on_crash.set(consume notify)

  be unsubscribe_crash(notify: {ref ()} tag) =>
    _on_crash.unset(notify)

  be step() =>
    if not (_status is Running) then
      return
    end

    try
      (let instr, let ip') = _memory.decode(_ip)?
      match instr
      | (Add, let lhs: Src, let rhs: Src, let dst: Dst) =>
          _write(dst, _read(lhs) + _read(rhs))
          _ip = ip'
      | (Mul, let lhs: Src, let rhs: Src, let dst: Dst) =>
          _write(dst, _read(lhs) * _read(rhs))
          _ip = ip'
      | (Inp, let dst: Dst) =>
          try
            _write(dst, _input.shift()?)
            _ip = ip'
          else
            _status = Blocked
          end
      | (Out, let src: Src) =>
          let word = _read(src)
          for rcvr in _subscribers.values() do
            rcvr(word)
          end
          _ip = ip'
      | (Jnz, let cnd: Src, let tgt: Src) =>
          _ip = if _read(cnd) != 0 then
            let tgt' = _read(tgt)
            if tgt' < 0 then error else tgt'.usize() end
          else
            ip'
          end
      | (Jz, let cnd: Src, let tgt: Src) =>
          _ip = if _read(cnd) == 0 then
            let tgt' = _read(tgt)
            if tgt' < 0 then error else tgt'.usize() end
          else
            ip'
          end
      | (Lt, let lhs: Src, let rhs: Src, let dst: Dst) =>
          _write(dst, if _read(lhs) < _read(rhs) then 1 else 0 end)
          _ip = ip'
      | (Eq, let lhs: Src, let rhs: Src, let dst: Dst) =>
          _write(dst, if _read(lhs) == _read(rhs) then 1 else 0 end)
          _ip = ip'
      | (Arb, let src: Src) =>
          _rb = _rb + _read(src)
          _ip = ip'
      | Hlt =>
          _status = Halted
          _ip = ip'
          for notify in _on_halt.values() do
            notify()
          end
      end
    else
      _status = IllegalInstruction
      for notify in _on_crash.values() do
        notify()
      end
    end

    if _running and (_status is Running) then
      step()
    end

  be run() =>
    if _status is Running then
      _running = true
      step()
    end

  be apply(word: I64) =>
    // TODO: apply backpressure if we keep receiving while Running?
    _input.push(word)
    if _status is Blocked then
      _status = Running
      if _running then
        step()
      end
    end

  be query_status(fn: {(Status)} val) =>
    fn(_status)

  be query_memory(ptr: USize, fn: {(I64)} val) =>
    fn(_memory(ptr))

  fun box _read(src: Src): I64 =>
    (let mode, let value) = src
    match mode
    | Imm => value
    | Mem => _memory(value.usize())
    | Rel => _memory((_rb + value).usize())
    end

  fun ref _write(dst: Dst, word: I64) =>
    (let mode, let value) = dst
    match mode
    | Mem => _memory(value.usize()) = word
    | Rel => _memory((_rb + value).usize()) = word
    end
