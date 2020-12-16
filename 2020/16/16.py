import sys

from typing import Dict, List, NamedTuple, TextIO, Tuple

class Data(NamedTuple):
    rules: Dict[str, List[Tuple[int, int]]]
    mine: List[int]
    others: List[List[int]]

def parse_input(stream: TextIO) -> Data:
    rules = dict()
    while True:
        l = next(stream).strip()
        if l == '':
            break
        clauses = list()
        tag, rest = l.split(': ')
        ranges = rest.split(' or ')
        for r in ranges:
            low, high = r.split('-')
            clauses.append((int(low), int(high)))
        rules[tag] = clauses

    if next(stream).strip() != 'your ticket:':
        raise ValueError('invalid input!')

    mine = [int(x) for x in next(stream).strip().split(',')]

    if next(stream).strip() != '':
        raise ValueError('invalid input!')

    if next(stream).strip() != 'nearby tickets:':
        raise ValueError('invalid input!')

    others = [
        [int(x) for x in l.strip().split(',')]
        for l in stream
    ]

    return Data(rules, mine, others)

def valid(rules: Dict[str, List[Tuple[int, int]]], rule: str, val: int) -> bool:
    clauses = rules[rule]
    for (low, high) in clauses:
        if low <= val and val <= high:
            return True
    return False

def any_valid(rules: Dict[str, List[Tuple[int, int]]], val: int) -> bool:
    return any(low <= val <= high
      for clauses in rules.values()
      for (low, high) in clauses)

def error_rate(data: Data) -> int:
    rate = 0
    for t in data.others:
        for val in t:
            if not any_valid(data.rules, val):
                rate += val
    return rate

def main() -> int:
    data = parse_input(sys.stdin)
    print(error_rate(data))

    return 0

if __name__ == '__main__':
    sys.exit(main())
