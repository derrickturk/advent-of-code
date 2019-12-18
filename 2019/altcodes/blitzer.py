# blitzer - minimalist intcode with async I/O

import sys
import asyncio
import threading # !!!

BUF_SIZE = 512

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

async def exec(mem, in_q, out_q):
    ip = 0
    while True:
        instr, m1, m2, m3 = load(mem[ip])
        # print(f'ip = {ip}, bp = {mem._bp}, {instr}/{m1}{m2}{m3}')
        if instr == 1:
            mem.w(ip + 3, m3, mem.r(ip + 1, m1) + mem.r(ip + 2, m2))
            ip += 4
        elif instr == 2:
            mem.w(ip + 3, m3, mem.r(ip + 1, m1) * mem.r(ip + 2, m2))
            ip += 4
        elif instr == 3:
            mem.w(ip + 1, m1, await in_q.get())
            in_q.task_done()
            ip += 2
        elif instr == 4:
            await out_q.put(mem.r(ip + 1, m1))
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
            await out_q.join()
            return
        else:
            raise ValueError(f'Invalid opcode: {mem[ip]} / instruction {instr}')

async def areadline(loop):
    fut = loop.create_future()
    def _reader():
        line = sys.stdin.readline()
        loop.call_soon_threadsafe(fut.set_result, line)
    threading.Thread(target=_reader, daemon=True).start()
    return await fut

async def pump_input(loop, q):
    while True:
        line = await areadline(loop)
        await q.put(int(line))

async def pump_output(loop, q):
    while True:
        val = await q.get()
        await loop.run_in_executor(None, sys.stdout.write, f'{val}\n')
        q.task_done()

async def main(argv):
    if len(argv) == 2:
        with open(argv[1], 'r') as f:
            image = [int(x) for x in f.readline().rstrip().split(',')]
    else:
        image = [int(x) for x in sys.stdin.readline().rstrip().split(',')]

    in_q = asyncio.Queue(BUF_SIZE)
    out_q = asyncio.Queue(BUF_SIZE)

    vm = asyncio.create_task(exec(Mem(image), in_q, out_q))
    loop = asyncio.get_event_loop()

    do_in = asyncio.create_task(pump_input(loop, in_q))
    do_out = asyncio.create_task(pump_output(loop, out_q))

    done = []
    pending = [vm, do_in, do_out]
    while vm in pending:
        done, pending = await asyncio.wait(
                pending, return_when=asyncio.FIRST_COMPLETED)
    for p in pending:
        p.cancel()

if __name__ == '__main__':
    asyncio.run(main(sys.argv))
