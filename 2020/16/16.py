import sys

from typing import Dict, List, NamedTuple, Optional, TextIO, Tuple

RuleClause = Tuple[int, int] # e.g. 25-35
Rule = List[RuleClause] # e.g. 25-35 or 47-90
RuleMap = Dict[str, Rule] # e.g. xxx: 25-35 or 47-90

Ticket = List[int]

RuleAssignment = List[str]

class Data(NamedTuple):
    rules: RuleMap
    mine: Ticket
    others: List[Ticket]

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

def valid(rules: RuleMap, rule: str, val: int) -> bool:
    clauses = rules[rule]
    for (low, high) in clauses:
        if low <= val and val <= high:
            return True
    return False

def any_valid(rules: RuleMap, val: int) -> bool:
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

def valid_tickets(data: Data) -> List[Ticket]:
    result = list()
    for t in data.others:
        valid = True
        for val in t:
            if not any_valid(data.rules, val):
                valid = False
                break
        if valid:
            result.append(t)
    return result

def solve_mapping(data: Data) -> RuleAssignment:
    valid_tix = valid_tickets(data)
    res = solve_sub_mapping(data.rules, valid_tix, [''] * len(data.mine), 0)
    if res is None:
        raise ValueError('no solution!')
    return res

def solve_sub_mapping(rules: RuleMap, tix: List[Ticket],
        mapping: RuleAssignment, start: int) -> Optional[RuleAssignment]:
    if start == len(mapping):
        return mapping

    locked = set(mapping[:start])
    possible = set(k for k in rules if k not in locked)
    for p in possible:
        try_mapping = list(mapping)
        try_mapping[start] = p

        discredited = False
        for t in tix:
            if not valid(rules, p, t[start]):
                discredited = True
                break
        if discredited:
            continue

        solved = solve_sub_mapping(rules, tix, try_mapping, start + 1)
        if solved is not None:
            return solved

    return None

def main() -> int:
    data = parse_input(sys.stdin)

    print(error_rate(data))

    assignment = solve_mapping(data)

    prod = 1
    for i, rule in enumerate(assignment):
        if rule.startswith('departure'):
            prod *= data.mine[i]
    print(prod)

    return 0

if __name__ == '__main__':
    sys.exit(main())
