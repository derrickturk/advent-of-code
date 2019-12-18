import sys

class Mem:
    __slots__ = ('_mem',)

    def __init__(self, mem):
        self._mem = mem

    def __getitem__(self, idx):
        if idx >= len(self._mem):
            self._mem.extend([0] * (idx - len(self._mem) + 1))
        return self._mem[idx]

    def __setitem__(self, idx, val):
        if idx >= len(self._mem):
            self._mem.extend([0] * (idx - len(self._mem) + 1))
        self._mem[idx] = val

def load(opcode):
    instr = opcode % 100
    modes = opcode // 100
    return instr, modes % 10, modes // 10 % 10, modes // 100

def _r(mem, ptr, mode):
    if mode == 0:
        return mem[mem[ptr]]
    elif mode == 1:
        return mem[ptr]
    else:
        raise ValueError(f'unknown read mode {mode}')

def _w(mem, ptr, mode, val):
    if mode == 0:
        mem[mem[ptr]] = val
    else:
        raise ValueError(f'unknown write mode {mode}')

def exec(mem):
    ip = 0

    while True:
        instr, m1, m2, m3 = load(mem[ip])
        if instr == 1:
            _w(mem, ip + 3, m3, _r(mem, ip + 1, m1) + _r(mem, ip + 2, m2))
            ip += 4
        elif instr == 2:
            _w(mem, ip + 3, m3, _r(mem, ip + 1, m1) * _r(mem, ip + 2, m2))
            ip += 4
        elif instr == 3:
            _w(mem, ip + 1, m1, int(sys.stdin.readline().rstrip()))
            ip += 2
        elif instr == 4:
            print(_r(mem, ip + 1, m1))
            ip += 2
        elif instr == 5:
            if _r(mem, ip + 1, m1) != 0:
                ip = _r(mem, ip + 2, m2)
            else:
                ip += 3
        elif instr == 6:
            if _r(mem, ip + 1, m1) == 0:
                ip = _r(mem, ip + 2, m2)
            else:
                ip += 3
        elif instr == 7:
            _w(mem, ip + 3, m3, int(_r(mem, ip + 1, m1) < _r(mem, ip + 2, m2)))
            ip += 4
        elif instr == 8:
            _w(mem, ip + 3, m3, int(_r(mem, ip + 1, m1) == _r(mem, ip + 2, m2)))
            ip += 4
        elif instr == 99:
            break
        else:
            raise ValueError(f'Invalid opcode: {mem[ip]} / instruction {instr}')

def main(argv):
    if len(argv) == 2:
        with open(argv[1], 'r') as f:
            image = [int(x) for x in f.readline().rstrip().split(',')]
    else:
        image = [int(x) for x in sys.stdin.readline().rstrip().split(',')]

    exec(Mem(image))

if __name__ == '__main__':
    main(sys.argv)
