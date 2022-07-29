import memory
import instruction

pub struct machine<h>
  ip: int
  rb: int
  mem: memory<h>

extend type exception-info
  InvalidOpcode(op: int)
  MalformedInstruction(ip: int)

type mode
  MPos
  MImm
  MRel

pub fun init(mem: list<int>): alloc<h> machine<h>
  Machine(ip = 0, rb = 0, mem = init(vector(mem)))

fun decode(cpu: machine<h>): <read<h>, write<h>, exn> instruction
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
      Lt(src1, src2, dst)
    8 ->
      val src1 = decode-src(cpu.mem[cpu.ip + 1], mode1, cpu.ip)
      val src2 = decode-src(cpu.mem[cpu.ip + 2], mode2, cpu.ip)
      val dst = decode-dst(cpu.mem[cpu.ip + 3], mode3, cpu.ip)
      Eq(src1, src2, dst)
    9 -> Hlt
    _ -> throw("Unknown opcode", InvalidOpcode(op))

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
