import sys

from typing import Callable, Dict, Iterable, List, TextIO

REQUIRED_KEYS: Dict[str, Callable[[str], bool]] = {
    'byr': lambda _: True,
    "iyr": lambda _: True,
    "eyr": lambda _: True,
    "hgt": lambda _: True,
    "hcl": lambda _: True,
    "ecl": lambda _: True,
    "pid": lambda _: True,
};

def parse_passports(stream: TextIO) -> Iterable[Dict[str, str]]:
    this_passport: Dict[str, str] = dict()
    for line in stream:
        line = line.rstrip()
        if line == '' and len(this_passport) > 0:
            yield this_passport
            this_passport = dict()
        for entry in line.split():
            k, v = entry.split(':')
            if k in this_passport:
                raise ValueError(f'duplicate key: {k}')
            this_passport[k] = v
    if len(this_passport) > 0:
        yield this_passport

def valid1(passport: Dict[str, str]) -> bool:
    return all(k in passport for k in REQUIRED_KEYS.keys())

def main(argv: List[str]) -> int:
    passports = parse_passports(sys.stdin)
    print(f'{sum(1 if valid1(p) else 0 for p in passports)} valid1')
    return 0

if __name__ == '__main__':
    sys.exit(main(sys.argv))
