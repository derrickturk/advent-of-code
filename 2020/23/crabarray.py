import sys

from typing import List

def make_cuparray(initial: List[int], up_to: int) -> List[int]:
    result = [0] * (up_to + 1)
    last_cup = 0
    for cup in initial:
        result[last_cup] = cup
        last_cup = cup
    for cup in range(max(initial) + 1, up_to + 1):
        result[last_cup] = cup
        last_cup = cup
    result[last_cup] = result[0]
    return result

def remove_clockwise(cups: List[int], focus: int) -> int:
    val = cups[focus]
    cups[focus] = cups[cups[focus]]
    cups[val] = 0 # "missing indicator"
    return val

def insert_clockwise(cups: List[int], target: int, cup: int) -> int:
    if cups[cup] != 0:
        raise ValueError('duplicate insert!')
    cups[cup] = cups[target]
    cups[target] = cup
    return cup

def to_sequence(cups: List[int], focus: int) -> List[int]:
    result = list()
    next_cup = focus
    while True:
        result.append(next_cup)
        next_cup = cups[next_cup]
        if next_cup == focus:
            break
    return result

def main(argv: List[str]) -> int:
    if len(argv) != 4:
        print(f'Usage: {argv[0]} init-cups n-cups n-iters', file=sys.stderr)
        return 1

    init_cups = [int(x) for x in argv[1].split(',')]
    n_cups = int(argv[2])
    n_iters = int(argv[3])

    cups = make_cuparray(init_cups, n_cups)
    focus = init_cups[0]

    for _ in range(n_iters):
        first = remove_clockwise(cups, focus)
        second = remove_clockwise(cups, focus)
        third = remove_clockwise(cups, focus)

        target = focus - 1
        if target < 1:
            target = n_cups
        while target in (first, second, third):
            target -= 1
            if target < 1:
                target = n_cups

        target = insert_clockwise(cups, target, first)
        target = insert_clockwise(cups, target, second)
        target = insert_clockwise(cups, target, third)

        focus = cups[focus]

    # print(','.join(str(x) for x in to_sequence(cups, focus)))

    one_cw = cups[1]
    one_cw2 = cups[one_cw]
    print(f'{one_cw} x {one_cw2} = {one_cw * one_cw2}')

    return 0

if __name__ == '__main__':
    sys.exit(main(sys.argv))
