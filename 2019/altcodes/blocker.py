# blocker - minimalist intcode with explicit "blocking" I/O

import sys
from collections import deque
from enum import Enum

import argparse

class State(Enum):
    HALT = 0
    WAIT_INPUT = 1

class Mem:
    __slots__ = ('_mem', '_bp')

    def __init__(self, mem):
        self._mem = mem
        self._bp = 0

    def __getitem__(self, idx):
        if idx >= len(self._mem):
            self._mem.extend([0] * (idx - len(self._mem) + 1))
        return self._mem[idx]

    def __setitem__(self, idx, val):
        if idx >= len(self._mem):
            self._mem.extend([0] * (idx - len(self._mem) + 1))
        self._mem[idx] = val

    def r(self, ptr, mode):
        if mode == 0:
            return self[self[ptr]]
        elif mode == 1:
            return self[ptr]
        elif mode == 2:
            return self[self._bp + self[ptr]]
        else:
            raise ValueError(f'unknown read mode {mode}')

    def w(self, ptr, mode, val):
        if mode == 0:
            self[self[ptr]] = val
        elif mode == 2:
            self[self._bp + self[ptr]] = val
        else:
            raise ValueError(f'unknown write mode {mode}')

    def adjust_bp(self, offset):
        self._bp += offset

def load(opcode):
    instr = opcode % 100
    modes = opcode // 100
    return instr, modes % 10, modes // 10 % 10, modes // 100

def exec(mem, ip, in_deque, out_deque):
    while True:
        instr, m1, m2, m3 = load(mem[ip])
        if instr == 1:
            mem.w(ip + 3, m3, mem.r(ip + 1, m1) + mem.r(ip + 2, m2))
            ip += 4
        elif instr == 2:
            mem.w(ip + 3, m3, mem.r(ip + 1, m1) * mem.r(ip + 2, m2))
            ip += 4
        elif instr == 3:
            if len(in_deque) == 0:
                return State.WAIT_INPUT, ip
            mem.w(ip + 1, m1, in_deque.popleft())
            ip += 2
        elif instr == 4:
            out_deque.append(mem.r(ip + 1, m1))
            ip += 2
        elif instr == 5:
            if mem.r(ip + 1, m1) != 0:
                ip = mem.r(ip + 2, m2)
            else:
                ip += 3
        elif instr == 6:
            if mem.r(ip + 1, m1) == 0:
                ip = mem.r(ip + 2, m2)
            else:
                ip += 3
        elif instr == 7:
            mem.w(ip + 3, m3, int(mem.r(ip + 1, m1) < mem.r(ip + 2, m2)))
            ip += 4
        elif instr == 8:
            mem.w(ip + 3, m3, int(mem.r(ip + 1, m1) == mem.r(ip + 2, m2)))
            ip += 4
        elif instr == 9:
            mem.adjust_bp(mem.r(ip + 1, m1))
            ip += 2
        elif instr == 99:
            return State.HALT, ip
        else:
            raise ValueError(f'Invalid opcode: {mem[ip]} / instruction {instr}')

def arg_parser():
    parser = argparse.ArgumentParser()
    parser.add_argument('file', metavar='IMAGE-FILE', type=open)
    parser.add_argument('--text', '-t', action='store_true')
    parser.add_argument('--start-input-file', '-f', nargs='+', type=open)
    return parser

def main(argv):
    args = arg_parser().parse_args(argv[1:])

    image = [int(x) for x in args.file.readline().rstrip().split(',')]
    args.file.close()

    inputs = deque()
    outputs = deque()

    if args.start_input_file:
        for f in args.start_input_file:
            if args.text:
                inputs.extend(ord(c) for c in f.read() if c != '\r')
            else:
                inputs.extend(int(line.rstrip()) for line in f)

    ip = 0
    mem = Mem(image)
    while True:
        state, ip = exec(mem, ip, inputs, outputs)
        if state == State.HALT:
            break
        elif state == State.WAIT_INPUT:
            # first, emit queued output
            if args.text:
                for o in outputs:
                    sys.stdout.write(chr(o))
            else:
                for o in outputs:
                    print(o)
            outputs.clear()
            line = sys.stdin.readline()
            if args.text:
                inputs.extend(ord(c) for c in line if c != '\r')
            else:
                inputs.append(int(line.rstrip()))

    # emit remaining output after halt
    if args.text:
        for o in outputs:
            sys.stdout.write(chr(o))
    else:
        for o in outputs:
            print(o)

if __name__ == '__main__':
    main(sys.argv)
