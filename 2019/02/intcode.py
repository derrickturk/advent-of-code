import sys

from typing import NamedTuple, List
from typing_extensions import Protocol

Program = List[int]

class HaltException(Exception):
    pass

class OpCode(Protocol):
    def eval(self, program: Program):
        ...

    @property
    def ip_step(self) -> int:
        ...

class Add(NamedTuple):
    src1: int
    src2: int
    dst: int

    def eval(self, program: Program):
        program[self.dst] = program[self.src1] + program[self.src2]

    @property
    def ip_step(self) -> int:
        return 4

class Mul(NamedTuple):
    src1: int
    src2: int
    dst: int

    def eval(self, program: Program):
        program[self.dst] = program[self.src1] * program[self.src2]

    @property
    def ip_step(self) -> int:
        return 4

class Halt(NamedTuple):
    def eval(self, program: Program):
        raise HaltException

    @property
    def ip_step(self) -> int:
        return 1

def parse(program: Program, ip: int) -> OpCode:
    op = program[ip]
    if op == 1:
        return Add(*program[ip + 1:ip + 4])
    if op == 2:
        return Mul(*program[ip + 1:ip + 4])
    if op == 99:
        return Halt()
    raise ValueError(f'Unknown opcode {op}')

def execute(program: Program) -> int:
    ip = 0
    while True:
        try:
            opcode = parse(program, ip)
            opcode.eval(program)
            ip += opcode.ip_step
        except HaltException:
            return program[0]

def problem1(program: Program):
    program[1] = 12
    program[2] = 2
    res = execute(program)
    print(f'final state: {program}')
    print(res)

def problem2(program: Program):
    for noun in range(100):
        for verb in range(100):
            this_prog = list(program)
            this_prog[1] = noun
            this_prog[2] = verb
            try:
                if execute(this_prog) == 19690720:
                    print(100 * noun + verb)
                    return
            except:
                pass
    print('No solution found', file=sys.stderr)

def main(args: List[str]) -> int:
    if len(args) != 2:
        print(f'Usage: {args[0]} 1|2 <input.txt', file=sys.stderr)
        return 0

    program = [int(x) for x in sys.stdin.readline().rstrip().split(',')]

    if args[1] == '1':
        problem1(program)
    elif args[1] == '2':
        problem2(program)
    else:
        print(f'Usage: {args[0]} 1|2 <input.txt', file=sys.stderr)

    return 0

if __name__ == '__main__':
    sys.exit(main(sys.argv))
