import sys
from functools import cmp_to_key
from typing import Iterator, Tuple, TypeAlias

Packet: TypeAlias = int | list['Packet']

def parse_packet(line: str) -> Packet:
    stack: list[list[Packet]] = []
    num = -1
    for c in line:
        if c.isdecimal():
            if num == -1:
                num = 0
            num *= 10
            num += int(c)
            continue
        elif num != -1:
            stack[-1].append(num)
            num = -1
        match c:
            case '[':
                stack.append([])
            case ']':
                this = stack.pop()
                if stack:
                    stack[-1].append(this)
                else:
                    return this
            case ',':
                if not stack or not stack[-1]:
                    raise ValueError(f'bad comma')
                continue
            case _: raise ValueError(f'bad token: {c}')
    raise ValueError('no packet')

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
    packets = [parse_packet(l.rstrip()) for l in sys.stdin if l.rstrip() != '']
    print(sum(
      i + 1
      for i, (p1, p2) in enumerate(chunks2(packets))
      if compare(p1, p2) <= 0
    ))
    p_sorted = sorted(packets + [[[2]], [[6]]], key=cmp_to_key(compare))
    print((p_sorted.index([[2]]) + 1) * (p_sorted.index([[6]]) + 1))

if __name__ == '__main__':
    main()
