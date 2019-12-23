# use the "blinder" VM to solve problem 7 pt 1

import sys
import asyncio
from collections import deque

from itertools import permutations

from blinder import Mem, exec

PHASES = [0, 1, 2, 3, 4]

def run_with_phases(image, phases):
    for i, p in enumerate(phases):
        inputs = deque((p,))
        if i == 0:
            inputs.append(0)
        else:
            inputs.extend(outputs)
        outputs = deque()
        exec(Mem(image.copy()), inputs, outputs)
    return outputs.popleft()

def main(argv):
    if len(argv) == 2:
        with open(argv[1], 'r') as f:
            image = [int(x) for x in f.readline().rstrip().split(',')]
    else:
        image = [int(x) for x in sys.stdin.readline().rstrip().split(',')]

    perms = list(permutations(PHASES))

    max_out = None
    max_phases = None
    for phases in perms:
        out = run_with_phases(image, phases)
        if max_out is None or out > max_out:
            max_out = out
            max_phases = phases
    print(f'highest = {max_out} @ {max_phases}')

if __name__ == '__main__':
    main(sys.argv)
