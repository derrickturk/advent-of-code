import sys

from typing import List, NamedTuple, Optional, Tuple
from typing_extensions import Protocol

class Expr(Protocol):
    def eval(self) -> int:
        ...

class Lit(NamedTuple):
    val: int

    def eval(self) -> int:
        return self.val

class Add(NamedTuple):
    lhs: Expr
    rhs: Expr

    def eval(self) -> int:
        return self.lhs.eval() + self.rhs.eval()

class Mul(NamedTuple):
    lhs: Expr
    rhs: Expr

    def eval(self) -> int:
        return self.lhs.eval() * self.rhs.eval()

# a parser[T] will just be a function (str) -> Optional[(str, T)]
# all "lexeme" parser will consume trailing whitespace before returning

def natural(inp: str) -> Optional[Tuple[str, int]]:
    num = []
    while inp and inp[0].isdigit():
        num.append(inp[0])
        inp = inp[1:]
    if num:
        return inp.lstrip(), int(''.join(num))
    else:
        return None

# parsers for part 1

def term1(inp: str) -> Optional[Tuple[str, Expr]]:
    # nat
    res1 = natural(inp)
    if res1 is not None:
        rest, val = res1
        return rest, Lit(val)

    # else, try ( expr )
    if not inp or inp[0] != '(':
        return None
    res2 = expr1(inp[1:].lstrip())
    if res2 is not None:
        rest, ex = res2
        if not rest or rest[0] != ')':
            return None
        return rest[1:].lstrip(), ex
    return None

# parse a single "+ term" in a series (returning the term)
def add_term1(inp: str) -> Optional[Tuple[str, Expr]]:
    if not inp or inp[0] != '+':
        return None
    return term1(inp[1:].lstrip())

# parse a single "* term" in a series (returning the term)
def mul_term1(inp: str) -> Optional[Tuple[str, Expr]]:
    if not inp or inp[0] != '*':
        return None
    return term1(inp[1:].lstrip())

def expr1(inp: str) -> Optional[Tuple[str, Expr]]:
    res = term1(inp)
    if res is None:
        return None
    t: Expr
    rest, t = res
    while True:
        res = add_term1(rest)
        if res is not None:
            rest, u = res
            t = Add(t, u)
        else:
            res = mul_term1(rest)
            if res is not None:
                rest, u = res
                t = Mul(t, u)
            else:
                break
    return rest, t

def prog1(inp: str) -> Optional[List[Expr]]:
    prog: List[Expr] = []
    while True:
        res = expr1(inp)
        if res is None:
            if inp:
                return None
            return prog
        inp, ex = res
        prog.append(ex)

# parsers for part 2

def term2(inp: str) -> Optional[Tuple[str, Expr]]:
    # nat
    res1 = natural(inp)
    if res1 is not None:
        rest, val = res1
        return rest, Lit(val)

    # else, try ( expr )
    if not inp or inp[0] != '(':
        return None
    res2 = expr2(inp[1:].lstrip())
    if res2 is not None:
        rest, ex = res2
        if not rest or rest[0] != ')':
            return None
        return rest[1:].lstrip(), ex
    return None

def add_term2(inp: str) -> Optional[Tuple[str, Expr]]:
    if not inp or inp[0] != '+':
        return None
    return term2(inp[1:].lstrip())

def factor2(inp: str) -> Optional[Tuple[str, Expr]]:
    res = term1(inp)
    if res is None:
        return None
    t: Expr
    rest, t = res
    while True:
        res = add_term2(rest)
        if res is not None:
            rest, u = res
            t = Add(t, u)
        else:
            break
    return rest, t

def mul_factor2(inp: str) -> Optional[Tuple[str, Expr]]:
    if not inp or inp[0] != '*':
        return None
    return factor2(inp[1:].lstrip())

def expr2(inp: str) -> Optional[Tuple[str, Expr]]:
    res = term1(inp)
    if res is None:
        return None
    t: Expr
    rest, t = res
    while True:
        res = mul_factor2(rest)
        if res is not None:
            rest, u = res
            t = Add(t, u)
        else:
            break
    return rest, t

def prog2(inp: str) -> Optional[List[Expr]]:
    prog: List[Expr] = []
    while True:
        res = expr2(inp)
        if res is None:
            if inp:
                return None
            return prog
        inp, ex = res
        prog.append(ex)

def main() -> int:
    inp = sys.stdin.read()

    p1 = prog1(inp)
    if p1 is None:
        print('failed to parse as prog1', file=sys.stderr)
    else:
        print(sum(e.eval() for e in p1))

    p2 = prog1(inp)
    if p2 is None:
        print('failed to parse as prog2', file=sys.stderr)
    else:
        print(sum(e.eval() for e in p2))

    return 0

if __name__ == '__main__':
    sys.exit(main())
