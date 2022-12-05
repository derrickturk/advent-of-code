import sys
from copy import deepcopy
from enum import Enum, auto

class ParseState:
    Initial = auto()
    ReadStacks = auto()
    Skip = auto()
    ReadMoves = auto()

def update_stacks(line: str, stacks: list[list[str]]) -> None:
    for s in stacks:
        if line[1].isalpha():
            s.insert(0, line[1])
        line = line[4:]

def main() -> int:
    state = ParseState.Initial
    n_fields: int = -1
    stacks: list[list[str]] = []
    stacks2: list[list[str]] = []

    for line in sys.stdin:
        line = line.rstrip('\r\n')
        match state:
            case ParseState.Initial:
                n_fields = (len(line) - 3) // 4 + 1
                stacks = [[] for _ in range(n_fields)]
                state = ParseState.ReadStacks
                update_stacks(line, stacks)
            case ParseState.ReadStacks:
                if '[' not in line:
                    state = ParseState.Skip
                    continue
                update_stacks(line, stacks)
            case ParseState.Skip:
                state = ParseState.ReadMoves
                stacks2 = deepcopy(stacks)
                continue
            case ParseState.ReadMoves:
                _, count, _, src, _, dst = line.split(' ')
                dst0 = int(dst) - 1
                src0 = int(src) - 1
                i_count = int(count)
                for _ in range(i_count):
                    stacks[dst0].append(stacks[src0].pop())
                stacks2[dst0] += stacks2[src0][-i_count:]
                stacks2[src0] = stacks2[src0][:-i_count]

    print(''.join(s[-1] for s in stacks))
    print(''.join(s[-1] for s in stacks2))

    return 0

if __name__ == '__main__':
    sys.exit(main())
