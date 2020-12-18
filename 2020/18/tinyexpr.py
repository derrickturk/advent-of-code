import sys

from typing import Optional, Tuple

# we'll represent a parser as a function(input: str) -> Optional[Tuple[str, int]]
# we're going to ignore whitespace entirely by stripping it before parsing

def nat(inp: str) -> Optional[Tuple[str, int]]:
    digits = []
    while inp and inp[0].isdigit():
        digits.append(inp[0])
        inp = inp[1:]
    if not digits:
        return None
    return inp, int(''.join(digits))

def term(inp: str) -> Optional[Tuple[str, int]]:
    res = nat(inp)
    if res is not None:
        return res
    if not inp or inp[0] != '(':
        return None
    res = expr(inp[1:])
    if res is None:
        return None
    inp, ex = res
    if not inp or inp[0] != ')':
        return None
    return inp[1:], ex

def add_term(inp: str) -> Optional[Tuple[str, int]]:
    if not inp or inp[0] != '+':
        return None
    return term(inp[1:])

def mul_term(inp: str) -> Optional[Tuple[str, int]]:
    if not inp or inp[0] != '*':
        return None
    return term(inp[1:])

def expr(inp: str) -> Optional[Tuple[str, int]]:
    res = term(inp)
    if res is None:
        return None
    rest, t = res
    while True:
        res = add_term(rest)
        if res is not None:
            rest, add_t = res
            t += add_t
        else:
            res = mul_term(rest)
            if res is not None:
                rest, mul_t = res
                t *= mul_t
            else:
                break
    return rest, t

def main() -> int:
    sum_1 = 0
    for line in sys.stdin:
        stripped = ''.join(c for c in line if not c.isspace())
        res = expr(stripped)
        if res is None:
            print(f'bad expression: "{line.rstrip()}"', file=sys.stderr)
            return 1
        rest, val = res
        if rest:
            print(f'extra input: "{rest}"', file=sys.stderr)
        sum_1 += val
    print(sum_1)
    return 0

if __name__ == '__main__':
    sys.exit(main())
