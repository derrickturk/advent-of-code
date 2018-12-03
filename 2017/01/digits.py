from typing import Tuple, List, Iterable, Optional
from itertools import cycle, islice
from sys import exit, stdin

def pairs(xs: List[int]) -> Iterable[Tuple[int, int]]:
    steps = len(xs) // 2
    return zip(xs, islice(cycle(xs), steps, None))

def checksum(xs: List[int]) -> int:
    return sum(x for (x, x_next) in pairs(xs) if x == x_next)

def parse(line: str) -> List[int]:
    return [int(c) for c in line]

def main():
    try:
        print(checksum(parse(stdin.readline().rstrip())))
        return 0
    except ValueError:
        print('invalid input')
        return 1

if __name__ == '__main__':
    exit(main())
