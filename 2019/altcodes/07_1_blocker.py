# use the "blocker" VM to solve problem 7

import sys

from itertools import permutations

from blocker import State, Mem, exec

PHASES = [0, 1, 2, 3, 4]

def run_with_phases(image, phases):
    mems = [Mem(image.copy()) for _ in phases]
    ips = [0 for _ in phases]

    pipe_in = list()
    pipe_out = list()

    between = [list() for _ in range(len(phases) - 1)]

    inputs = [pipe_in, *between]
    outputs = [*between, pipe_out]

    last_out = None

    # send each machine's phase
    for i, p in zip(inputs, phases):
        i.append(p)

    # send 0 to kick off the pipe
    pipe_in.append(0)

    while True:
        states = [
            exec(m, ip, i, o)
            for m, ip, i, o in zip(mems, ips, inputs, outputs)
        ]
        ips = [ip for _, ip in states]
        states = [state for state, _ in states]

        # clear the output pipe
        if pipe_out:
            last_out = pipe_out[-1]
            pipe_out.clear()

        # the front of the pipe needs input
        if states[0] == State.WAIT_INPUT:
            inputs.append(int(sys.stdin.readline().rstrip()))

        if all(s == State.HALT for s in states):
            break

    return last_out

def main(argv):
    if len(argv) == 2:
        with open(argv[1], 'r') as f:
            image = [int(x) for x in f.readline().rstrip().split(',')]
    else:
        image = [int(x) for x in sys.stdin.readline().rstrip().split(',')]

    max_out = None
    max_phases = None
    for phases in permutations(PHASES):
        out = run_with_phases(image, phases)
        if max_out is None or out > max_out:
            max_out = out
            max_phases = phases
    print(f'highest = {max_out} @ {max_phases}')

if __name__ == '__main__':
    main(sys.argv)
