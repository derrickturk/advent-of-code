import sys
import re

from typing import Dict, List, NamedTuple, Optional, Pattern, Sequence
from typing_extensions import Protocol

class Rule(Protocol):
    def compile(self, rules: 'Dict[int, Rule]') -> str:
        ...

class Lit(NamedTuple):
    lit: str

    def compile(self, rules: Dict[int, Rule]) -> str:
        return self.lit

class Ref(NamedTuple):
    num: int

    def compile(self, rules: Dict[int, Rule]) -> str:
        return rules[self.num].compile(rules)

class Chain(NamedTuple):
    rules: Sequence[Rule]

    def compile(self, rules: Dict[int, Rule]) -> str:
        return ''.join(r.compile(rules) for r in self.rules)

class Alt(NamedTuple):
    rules: Sequence[Rule]

    def compile(self, rules: Dict[int, Rule]) -> str:
        return '(' + '|'.join(r.compile(rules) for r in self.rules) + ')'

def parse(line: str, rules: Dict[int, Rule]) -> None:
    num, rule = line.strip().split(': ')

    alts = rule.split('|')
    alted = list()
    for a in alts:
        chain = a.split()
        chained = list()
        for c in chain:
            base: Rule
            if c.startswith('"') and c.endswith('"'):
                base = Lit(c[1:-1])
            else:
                base = Ref(int(c))
            chained.append(base)
        alted.append(Chain(chained))

    if len(alted) == 1:
        if len(alted[0].rules) == 1:
            rules[int(num)] = alted[0].rules[0]
        else:
            rules[int(num)] = alted[0]
    else:
        rules[int(num)] = Alt(alted)

def main() -> int:
    return 0

if __name__ == '__main__':
    rules: Dict[int, Rule] = dict()
    the_re: Optional[Pattern[str]] = None
    forty_two: Optional[Pattern[str]] = None
    thirty_one: Optional[Pattern[str]] = None

    parse_done = False
    valid = 0
    valid2 = 0
    for l in sys.stdin:
        if l.rstrip() == '':
            parse_done = True
            the_re = re.compile(rules[0].compile(rules))
            forty_two = re.compile(rules[42].compile(rules))
            thirty_one = re.compile(rules[31].compile(rules))
        elif parse_done:
            l = l.rstrip()

            assert the_re is not None
            if the_re.fullmatch(l):
                valid += 1

            assert forty_two is not None
            assert thirty_one is not None
            n_42 = 0
            n_31 = 0

            while True:
                m = forty_two.match(l)
                if m:
                    l = l[m.end(0):]
                    n_42 += 1
                else:
                    break

            while True:
                m = thirty_one.match(l)
                if m:
                    l = l[m.end(0):]
                    n_31 += 1
                else:
                    break

            if n_31 >= 1 and n_42 >= n_31 + 1 and l == '':
                valid2 += 1
        else:
            parse(l, rules)
    print(valid)
    print(valid2)

    sys.exit(main())
