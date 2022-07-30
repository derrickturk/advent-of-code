import memory
import instruction

pub struct machine<h>
  ip: int
  rb: int
  mem: memory<h>

effect intcode-io
  ctl input(): int
  ctl output(word: int): ()

extend type exception-info
  InvalidOpcode(op: int)
  MalformedInstruction(ip: int)

type mode
  MPos
  MImm
  MRel

pub fun init(mem: list<int>): alloc<h> machine<h>
  Machine(ip = 0, rb = 0, mem = init(vector(mem)))

pub fun exec(cpu: machine<h>, instr: instruction)
    : <exn, intcode-io, read<h>, write<h>> machine<h>
  match instr
    Add(src1, src2, dst) ->
      cpu.write(dst, cpu.read(src1) + cpu.read(src2))
      cpu(ip = cpu.ip + 4)
    Mul(src1, src2, dst) ->
      cpu.write(dst, cpu.read(src1) * cpu.read(src2))
      cpu(ip = cpu.ip + 4)
    Inp(dst) ->
      cpu.write(dst, input())
      cpu(ip = cpu.ip + 2)
    Out(src) ->
      output(cpu.read(src))
      cpu(ip = cpu.ip + 2)
    Jnz(cmp, tgt) ->
      if cpu.read(cmp) != 0
        then cpu(ip = cpu.read(tgt))
        else cpu(ip = cpu.ip + 3)
    Jz(cmp, tgt) ->
      if cpu.read(cmp) == 0
        then cpu(ip = cpu.read(tgt))
        else cpu(ip = cpu.ip + 3)
    Less(src1, src2, dst) ->
      cpu.write(dst, if cpu.read(src1) < cpu.read(src2) then 1 else 0)
      cpu(ip = cpu.ip + 4)
    Eql(src1, src2, dst) ->
      cpu.write(dst, if cpu.read(src1) == cpu.read(src2) then 1 else 0)
      cpu(ip = cpu.ip + 4)
    Hlt -> cpu

pub fun run(cpu: machine<h>)
    : <div, exn, intcode-io, read<h>, write<h>> machine<h>
  match cpu.decode()
    Hlt -> cpu
    instr ->
      cpu.exec(instr).run

pub fun decode(cpu: machine<h>): <read<h>, write<h>, exn> instruction
  val word = cpu.mem[cpu.ip]
  val op = word % 100
  val mode1 = decode-mode((word / 100) % 10, cpu.ip)
  val mode2 = decode-mode((word / 1000) % 10, cpu.ip)
  val mode3 = decode-mode((word / 10000) % 10, cpu.ip)
  match op
    1 ->
      val src1 = decode-src(cpu.mem[cpu.ip + 1], mode1, cpu.ip)
      val src2 = decode-src(cpu.mem[cpu.ip + 2], mode2, cpu.ip)
      val dst = decode-dst(cpu.mem[cpu.ip + 3], mode3, cpu.ip)
      Add(src1, src2, dst)
    2 ->
      val src1 = decode-src(cpu.mem[cpu.ip + 1], mode1, cpu.ip)
      val src2 = decode-src(cpu.mem[cpu.ip + 2], mode2, cpu.ip)
      val dst = decode-dst(cpu.mem[cpu.ip + 3], mode3, cpu.ip)
      Mul(src1, src2, dst)
    3 ->
      val dst = decode-dst(cpu.mem[cpu.ip + 1], mode1, cpu.ip)
      Inp(dst)
    4 ->
      val src = decode-src(cpu.mem[cpu.ip + 1], mode1, cpu.ip)
      Out(src)
    5 ->
      val src1 = decode-src(cpu.mem[cpu.ip + 1], mode1, cpu.ip)
      val src2 = decode-src(cpu.mem[cpu.ip + 2], mode2, cpu.ip)
      Jnz(src1, src2)
    6 ->
      val src1 = decode-src(cpu.mem[cpu.ip + 1], mode1, cpu.ip)
      val src2 = decode-src(cpu.mem[cpu.ip + 2], mode2, cpu.ip)
      Jz(src1, src2)
    7 ->
      val src1 = decode-src(cpu.mem[cpu.ip + 1], mode1, cpu.ip)
      val src2 = decode-src(cpu.mem[cpu.ip + 2], mode2, cpu.ip)
      val dst = decode-dst(cpu.mem[cpu.ip + 3], mode3, cpu.ip)
      Less(src1, src2, dst)
    8 ->
      val src1 = decode-src(cpu.mem[cpu.ip + 1], mode1, cpu.ip)
      val src2 = decode-src(cpu.mem[cpu.ip + 2], mode2, cpu.ip)
      val dst = decode-dst(cpu.mem[cpu.ip + 3], mode3, cpu.ip)
      Eql(src1, src2, dst)
    9 -> Hlt
    _ -> throw("Unknown opcode", InvalidOpcode(op))

pub fun read(cpu: machine<h>, src: src): <exn, read<h>> int
  match src
    Imm(word) -> word
    Dst(dst) ->
      val ptr = match dst
        Mem(ptr) -> ptr
        Rel(offset) -> cpu.rb + offset
      cpu.mem[ptr]

pub fun write(cpu: machine<h>, dst: dst, word: int): <exn, read<h>, write<h>> ()
  val ptr = match dst
    Mem(ptr) -> ptr
    Rel(offset) -> cpu.rb + offset
  cpu.mem[ptr] := word

fun decode-mode(digit: int, ip: int): exn mode
  match digit
    0 -> MPos
    1 -> MImm
    2 -> MRel
    _ -> throw("Bad mode digit", MalformedInstruction(ip))

fun decode-dst(word: int, mode: mode, ip: int): exn dst
  match mode
    MPos -> Mem(word)
    MRel -> Rel(word)
    _ -> throw("Invalid mode", MalformedInstruction(ip))

fun decode-src(word: int, mode: mode, ip: int): src
  match mode
    MPos -> Dst(Mem(word))
    MRel -> Dst(Rel(word))
    MImm -> Imm(word)
