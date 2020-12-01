import sys

from itertools import islice

from typing import List, Optional, Set, Tuple

def find2sum(xs: List[int], target: int) -> Optional[Tuple[int, int]]:
    seen: Set[int] = set()
    for (i, x) in enumerate(xs):
        if target - x in seen:
            return x, target - x
        seen.add(x)
    return None

def find3sum(xs: List[int], target: int) -> Optional[Tuple[int, int, int]]:
    seen: Set[int] = set()
    for (i, x) in enumerate(xs):
        for (j, y) in islice(enumerate(xs), i + 1, None):
            z = target - x - y
            if z in seen:
                return x, y, z
            seen.add(x)
    return None

def main() -> int:
    inp = [int(l.strip()) for l in sys.stdin]

    res2 = find2sum(inp, 2020)
    if res2 is None:
        print('no 2-element solution')
    else:
        x, y = res2
        print(x * y)

    res3 = find3sum(inp, 2020)
    if res3 is None:
        print('no 3-element solution')
    else:
        x, y, z = res3
        print(x * y * z)

    return 0

if __name__ == '__main__':
    sys.exit(main())

