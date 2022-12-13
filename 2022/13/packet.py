import sys
from enum import Enum, auto
from functools import cmp_to_key
from typing import Iterator, TextIO, Tuple, TypeAlias

class Symbol(Enum):
    LBRACE = auto()
    RBRACE = auto()
    COMMA = auto()

Token: TypeAlias = Symbol | int

BUF_SZ = 1024

def lex(stream: TextIO) -> Iterator[Token]:
    buf: str | None = None
    num_buf: str | None = None
    while True:
        if buf is None:
            buf = stream.read(BUF_SZ)
            if buf == '':
                return

        for c in buf:
            if c.isdecimal():
                if num_buf is None:
                    num_buf = c
                else:
                    num_buf += c
                continue

            if num_buf is not None:
                yield int(num_buf)
                num_buf = None

            if c.isspace():
                continue

            match c:
                case '[': yield Symbol.LBRACE
                case ']': yield Symbol.RBRACE
                case ',': yield Symbol.COMMA
                case _: raise ValueError(f'invalid character: {c}')

        buf = None

class PeekableLexer:
    def __init__(self, toks: Iterator[Token]):
        self._toks: Iterator[Token] = toks
        self._buf: Token | None = None

    def __iter__(self) -> Iterator[Token]:
        return self

    def __next__(self) -> Token:
        if self._buf is not None:
            tok = self._buf
            self._buf = None
            return tok
        return next(self._toks)

    def peek(self) -> Token | None:
        if self._buf is None:
            try:
                self._buf = next(self._toks)
            except StopIteration:
                pass
        return self._buf

Packet: TypeAlias = int | list['Packet']

def parse_packet(tokens: PeekableLexer) -> Packet:
    match next(tokens):
        case int(n):
            return n
        case Symbol.LBRACE:
            if tokens.peek() == Symbol.RBRACE:
                next(tokens)
                return []
            # start with first packet..
            packet = [parse_packet(tokens)]
            while True:
                match next(tokens):
                    case Symbol.RBRACE:
                        return packet
                    case Symbol.COMMA:
                        pass
                    case t:
                        raise ValueError(f'bad token in list: {t}')
                packet.append(parse_packet(tokens))
        case t:
            raise ValueError(f'bad token: {t}')

def parse_packets(tokens: PeekableLexer) -> list[Packet]:
    packets = []
    while tokens.peek() is not None:
        packets.append(parse_packet(tokens))
    return packets

def compare(a: Packet, b: Packet) -> int:
    match a, b:
        case (int(m), int(n)):
            return m - n
        case (int(m), b):
            return compare([m], b)
        case (a, int(n)):
            return compare(a, [n])
        case ([], []):
            return 0
        case ([_, *_], []):
            return 1
        case ([], [_, *_]):
            return -1
        case ([fa, *ra], [fb, *rb]):
            match compare(fa, fb):
                case 0:
                    return compare(ra, rb)
                case c:
                    return c
        case _:
            raise ValueError('bad packet')

def chunks2(packets: list[Packet]) -> Iterator[Tuple[Packet, Packet]]:
    if len(packets) % 2 != 0:
        raise ValueError('odd number of packets')
    i = 0
    while i < len(packets) - 1:
        yield (packets[i], packets[i + 1])
        i += 2

def main() -> None:
    packets = parse_packets(PeekableLexer(lex(sys.stdin)))
    print(sum(
      i + 1
      for i, (p1, p2) in enumerate(chunks2(packets))
      if compare(p1, p2) <= 0
    ))
    p_sorted = sorted(packets + [[[2]], [[6]]], key=cmp_to_key(compare))
    print((p_sorted.index([[2]]) + 1) * (p_sorted.index([[6]]) + 1))

if __name__ == '__main__':
    main()
