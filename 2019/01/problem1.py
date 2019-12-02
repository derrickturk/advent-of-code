# two approaches, in Python

import sys

from itertools import islice, takewhile

from typing import Callable, List, Iterator, TypeVar

_T = TypeVar('_T')

def iterate(fn: Callable[[_T], _T], init: _T) -> Iterator[_T]:
    while True:
        yield init
        init = fn(init)

def fuel(mass: int) -> int:
    return mass // 3 - 2

def total_fuel_proc(mass: int) -> int:
    total = 0
    inc_fuel = fuel(mass)
    while inc_fuel > 0:
        total += inc_fuel 
        inc_fuel = fuel(inc_fuel)
    return total

def total_fuel_func(mass: int) -> int:
    return sum(takewhile(lambda f: f > 0, islice(iterate(fuel, mass), 1, None)))

def main(args: List[str]) -> int:
    if len(args) != 2:
        print(f'Usage: {args[0]} 1|2 <input.txt', file=sys.stderr)
        return 0

    masses = (int(l.rstrip()) for l in sys.stdin.readlines())

    if args[1] == '1':
        print(sum(fuel(m) for m in masses))
    elif args[1] == '2p':
        print(sum(total_fuel_proc(m) for m in masses))
    elif args[1] == '2f':
        print(sum(total_fuel_func(m) for m in masses))
    else:
        print(f'Usage: {args[0]} 1|2p|2f <input.txt', file=sys.stderr)

    return 0

if __name__ == '__main__':
    sys.exit(main(sys.argv))
