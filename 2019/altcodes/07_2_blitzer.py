# use the "blitzer" VM to solve problem 7

import sys
import asyncio

from itertools import permutations

from blitzer import Mem, exec

BUF_SZ = 512

PHASES = [5, 6, 7, 8, 9]

async def run_with_phases(image, phases):
    # ha, I can't see a way to use a single "loopback queue" here
    pipe_in = asyncio.Queue(512)
    pipe_out = asyncio.Queue(512)

    between = [asyncio.Queue(512) for _ in range(len(phases) - 1)]

    inputs = [pipe_in, *between]
    outputs = [*between, pipe_out]

    # send each machine's phase
    for i, p in zip(inputs, phases):
        await i.put(p)

    # send 0 to kick off the pipe
    await pipe_in.put(0)

    tasks = [
        asyncio.create_task(exec(Mem(image.copy()), i, o))
        for i, o in zip(inputs, outputs)
    ]

    last_output = None
    async def _pump_output():
        nonlocal last_output
        while True:
            last_output = await pipe_out.get()
            pipe_out.task_done()
            await pipe_in.put(last_output)

    pump_output = asyncio.create_task(_pump_output())
    tasks.append(pump_output)

    pending = tasks
    done = list()
    while pending and not (len(pending) == 1 and pump_output in pending):
        done, pending = await asyncio.wait(
                pending, return_when=asyncio.FIRST_COMPLETED)
    pump_output.cancel()

    return last_output

async def main(argv):
    if len(argv) == 2:
        with open(argv[1], 'r') as f:
            image = [int(x) for x in f.readline().rstrip().split(',')]
    else:
        image = [int(x) for x in sys.stdin.readline().rstrip().split(',')]

    perms = list(permutations(PHASES))

    outs = await asyncio.gather(*(run_with_phases(image, p) for p in perms))

    max_out = None
    max_phases = None
    for phases, out in zip(perms, outs):
        if max_out is None or out > max_out:
            max_out = out
            max_phases = phases
    print(f'highest = {max_out} @ {max_phases}')

if __name__ == '__main__':
    asyncio.run(main(sys.argv))
