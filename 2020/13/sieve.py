import sys

from typing import List, Tuple

# assumes all bus numbers are prime, thus lcm(x, y) = x * y
def solve(constraints: List[Tuple[int, int]]) -> int:
    if len(constraints) == 0:
        raise ValueError('no constraints')
    result = 0
    step = 1
    for (val, mod) in constraints:
        while result % mod != val: 
            result += step
        step *= mod
    return result

if __name__ == '__main__':
    _, buses = sys.stdin
    constraints = [((int(n) - i) % int(n), int(n))
      for i, n in enumerate(buses.strip().split(','))
      if n != 'x']
    constraints.sort(key=lambda t: -t[1])
    print(solve(constraints))
