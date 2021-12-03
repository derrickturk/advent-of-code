import sys
from collections import defaultdict

from typing import DefaultDict, Iterable, Generic, List, Tuple, TypeVar

T = TypeVar('T')

def choose_by_frequency(xs: Iterable[T], modal: bool) -> T:
    freq: DefaultDict[T, int] = defaultdict(lambda: 0)
    # this could be a reduce
    for x in xs:
        freq[x] += 1

    # we take advantage of a nice property of the rules for part 2:
    #   the tiebreak for "most common value" is "highest value" (1 over 0);
    #   and conversely for "least common value".
    # using (frequency, value) as the sort key (in appropriate order)
    #   gets us what we need.
    ranked = sorted(freq.items(), key=lambda t: (t[1], t[0]), reverse=modal)
    if not ranked:
        raise ValueError('empty sequence')
    return ranked[0][0]

def modal(xs: Iterable[T]) -> T:
    return choose_by_frequency(xs, True)

def antimodal(xs: Iterable[T]) -> T:
    return choose_by_frequency(xs, False)

def oxygen(diags: List[str]) -> int:
    if not diags:
        raise ValueError('empty sequence') 
    # for each digit of input
    for i in range(len(diags[0])):
        modal_digit = modal(d[i] for d in diags)
        diags = [d for d in diags if d[i] == modal_digit]
        if len(diags) == 1:
            return int(diags[0], base=2)
    raise ValueError('no unique solution')

def co2(diags: List[str]) -> int:
    if not diags:
        raise ValueError('empty sequence') 
    # for each digit of input
    for i in range(len(diags[0])):
        antimodal_digit = antimodal(d[i] for d in diags)
        diags = [d for d in diags if d[i] == antimodal_digit]
        if len(diags) == 1:
            return int(diags[0], base=2)
    raise ValueError('no unique solution')

def part1(diags: List[str]) -> Tuple[int, int]:
    # zip(*thing) is a nice way to "transpose"
    gamma = ''.join(modal(digit) for digit in zip(*diags))
    epsilon = ''.join(antimodal(digit) for digit in zip(*diags))
    return int(gamma, base=2), int(epsilon, base=2)

def part2(diags: List[str]) -> Tuple[int, int]:
    if not diags:
        raise ValueError('empty sequence') 
    return oxygen(diags), co2(diags)

def main() -> int:
    diags = [l.strip() for l in sys.stdin]
    gamma, epsilon = part1(diags)
    print(gamma * epsilon)

    o, c = part2(diags)
    print(o * c)

    return 0

if __name__ == '__main__':
    sys.exit(main())
