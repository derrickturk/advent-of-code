import sys

from typing import List, Optional, Tuple

def find2sum(xs: List[int], target: int) -> Optional[Tuple[int, int]]:
    for (i, x) in enumerate(xs):
        for (j, y) in enumerate(xs):
            if i != j and x + y == target:
                return x, y
    return None

def find3sum(xs: List[int], target: int) -> Optional[Tuple[int, int, int]]:
    for (i, x) in enumerate(xs):
        for (j, y) in enumerate(xs):
            for (k, z) in enumerate(xs):
                if i != j != k and x + y + z == 2020:
                    return x, y, z
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
