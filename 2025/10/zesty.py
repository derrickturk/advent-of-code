# /// script
# requires-python = ">=3.13"
# ///

import sys
from typing import Iterable, IO, NamedTuple

from z3 import *


class Machine(NamedTuple):
    buttons: list[list[int]]
    voltage: list[int]

    def constraints_minimize(self):
        bs = [Int(f'b{i}') for i in range(len(self.buttons))]
        nonegative = [b >= 0 for b in bs]
        addy = []
        for i, v in enumerate(self.voltage):
            relevant_bs = [
              b for b, b_spec in zip(bs, self.buttons)
              if i in b_spec
            ]
            this_cons = relevant_bs[0]
            for r in relevant_bs[1:]:
                this_cons = this_cons + r
            addy.append(this_cons == v)

        total = Int('total')
        return total, [total == sum(bs), *nonegative, *addy]


def parse_machines(inp: IO[str]) -> Iterable[Machine]:
    for line in inp:
        _, *rest = line.strip().split(' ')
        volt_spec = rest[-1]
        button_spec = rest[:-1]
        if volt_spec.startswith('{') and volt_spec.endswith('}'):
            volts = [int(v) for v in volt_spec[1:-1].split(',')]
        else:
            raise ValueError(f'bad volt spec: {volt_spec}')
        buttons = []
        for b in button_spec:
            if b.startswith('(') and b.endswith(')'):
                buttons.append([int(i) for i in b[1:-1].split(',')])
            else:
                raise ValueError(f'bad button spec: {b}')
        yield Machine(buttons, volts)


def main(argv: list[str]) -> int:
    all_presses = 0
    for m in parse_machines(sys.stdin):
        o = z3.Optimize()
        total, cs = m.constraints_minimize()
        o.add(*cs)
        h = o.minimize(total)
        r = o.check()
        if r != sat:
            raise ValueError(f'not sat! for {m}')
        mdl = o.model()
        all_presses += mdl[total].as_long()
    print(all_presses)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
