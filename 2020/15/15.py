import sys

from typing import Dict, List, Iterator
from itertools import islice

def numbers(start: List[int]) -> Iterator[int]:
    i = 0
    last_age = 0
    last_turn: Dict[int, int] = dict()
    new = False

    for val in start:
        if val in last_turn:
            last_age = i - last_turn[val]
            new = False
        else:
            new = True
        last_turn[val] = i
        yield val
        i += 1

    while True:
        val = 0 if new else last_age
        if val in last_turn:
            last_age = i - last_turn[val]
            new = False
        else:
            new = True
        last_turn[val] = i
        last = val
        yield val
        i += 1

def main() -> int:
    start = [int(x) for x in sys.stdin.readline().strip().split(',')]

    print(next(islice(numbers(start), 2019, None)))

    return 0

if __name__ == '__main__':
    sys.exit(main())
