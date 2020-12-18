import sys

from typing import Dict, Iterable, List, NamedTuple, Optional
from enum import Enum, auto

class TokenType(Enum):
    LPAREN = auto()
    RPAREN = auto()
    PLUS = auto()
    MUL = auto()
    NUM = auto()

class Token(NamedTuple):
    ty: TokenType
    val: Optional[int] = None # only for NUM

    def __str__(self) -> str:
        if self.ty == TokenType.LPAREN:
            return '('
        if self.ty == TokenType.RPAREN:
            return ')'
        if self.ty == TokenType.PLUS:
            return '+'
        if self.ty == TokenType.MUL:
            return '*'
        if self.ty == TokenType.NUM:
            return str(self.val)
        assert False

PREC1 = { TokenType.PLUS: 0, TokenType.MUL: 0 }
PREC2 = { TokenType.PLUS: 1, TokenType.MUL: 0 }

def tokenize(inp: str) -> Iterable[Token]:
    while True:
        inp = inp.lstrip()
        if not inp:
            break
        if inp[0] == '(':
            yield Token(TokenType.LPAREN)
            inp = inp[1:]
        elif inp[0] == ')':
            yield Token(TokenType.RPAREN)
            inp = inp[1:]
        elif inp[0] == '+':
            yield Token(TokenType.PLUS)
            inp = inp[1:]
        elif inp[0] == '*':
            yield Token(TokenType.MUL)
            inp = inp[1:]
        elif inp[0].isdigit():
            num = []
            i = 0
            while i < len(inp) and inp[i].isdigit():
                num.append(inp[i])
                i += 1
            inp = inp[i:]
            yield Token(TokenType.NUM, int(''.join(num)))
        else:
            raise ValueError(f"unexpected character: '{inp[0]}'")

def eval(inp: str, prec: Dict[TokenType, int]) -> int:
    num_stack: List[int] = []
    op_stack: List[Token] = []

    for t in tokenize(inp):
        if t.ty == TokenType.NUM:
            assert t.val is not None
            num_stack.append(t.val)
        elif t.ty in (TokenType.PLUS, TokenType.MUL):
            while (op_stack and op_stack[-1].ty != TokenType.LPAREN
                    and prec[op_stack[-1].ty] >= prec[t.ty]):
                op = op_stack.pop()
                x = num_stack.pop()
                y = num_stack.pop()
                if op.ty == TokenType.PLUS:
                    num_stack.append(x + y)
                elif op.ty == TokenType.MUL:
                    num_stack.append(x * y)
                else:
                    assert False
            op_stack.append(t)
        elif t.ty == TokenType.LPAREN:
            op_stack.append(t)
        elif t.ty == TokenType.RPAREN:
            while op_stack and op_stack[-1].ty != TokenType.LPAREN:
                op = op_stack.pop()
                x = num_stack.pop()
                y = num_stack.pop()
                if op.ty == TokenType.PLUS:
                    num_stack.append(x + y)
                elif op.ty == TokenType.MUL:
                    num_stack.append(x * y)
                else:
                    assert False
            if not op_stack:
                raise ValueError('mismatched parens')
            assert op_stack[-1].ty == TokenType.LPAREN
            op_stack.pop()
        else:
            assert False

    while op_stack:
        op = op_stack.pop()
        if op.ty not in (TokenType.PLUS, TokenType.MUL):
            raise ValueError('mismatched parens')
        x = num_stack.pop()
        y = num_stack.pop()
        if op.ty == TokenType.PLUS:
            num_stack.append(x + y)
        else:
            num_stack.append(x * y)

    if len(num_stack) != 1:
        raise ValueError('mismatched... something')

    return num_stack[-1]

def main() -> int:
    sum1 = 0
    sum2 = 0
    for line in sys.stdin:
        sum1 += eval(line, PREC1)
        sum2 += eval(line, PREC2)
    print(sum1)
    print(sum2)
    return 0

if __name__ == '__main__':
    sys.exit(main())
